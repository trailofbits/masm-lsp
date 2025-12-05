use std::collections::HashMap;

use miden_assembly_syntax::ast::visit::Visit;
use miden_assembly_syntax::ast::{Block, Module, Op, Procedure};
use miden_debug_types::{DefaultSourceManager, SourceSpan, Spanned};

use crate::analysis::{
    abstract_interpretation::{analyze_repeat_loop, analyze_while_loop},
    pre_analyze_procedure, ContractStore, StackEffect, SymbolicExpr,
};
use crate::decompiler::ssa::{
    extract_declaration_prefix, format_procedure_signature, DecompilerState, PseudocodeTemplate,
    SsaId,
};
use crate::diagnostics::span_to_range;
use crate::symbol_resolution::SymbolResolver;

use super::types::{shift_loop_stack, ActiveLoop, ParametricExpr, SsaHint, TrackingFailure};

/// SSA-based collector that uses phi nodes for variable tracking at merge points.
///
/// This collector uses a two-pass approach:
/// 1. **Pass 1**: Visit AST, generate templates with SSA IDs, create phi nodes at merge points
/// 2. **Pass 2**: Resolve phi relationships, convert templates to final strings
pub struct PseudocodeCollector<'a> {
    /// Current SSA decompiler state
    pub(super) state: Option<DecompilerState>,
    /// Collected hint templates: (line, template, indent_level)
    pub(super) hints: Vec<SsaHint>,
    /// Index of the first hint for the current procedure
    pub(super) proc_hint_start: usize,
    /// Number of outputs for current procedure
    pub(super) proc_outputs: Option<usize>,
    /// Line number of the procedure declaration
    pub(super) proc_decl_line: Option<u32>,
    /// Collected tracking failures for diagnostics
    pub(super) failures: Vec<TrackingFailure>,
    /// Source manager for span conversion
    pub(super) sources: &'a DefaultSourceManager,
    /// Symbol resolver for resolving invocation targets (reserved for procedure calls)
    #[allow(dead_code)]
    pub(super) resolver: SymbolResolver<'a>,
    /// Contract store for looking up procedure stack effects
    pub(super) contracts: Option<&'a ContractStore>,
    /// Source text for extracting declaration prefixes
    pub(super) source_text: &'a str,
    /// Current indentation level
    pub(super) indent_level: usize,
    /// Finalized hints (after resolution)
    pub(super) resolved_hints: Vec<(u32, String)>,
    /// Stack of active loops for parametric variable naming
    pub(super) loop_stack: Vec<ActiveLoop>,
    /// Counter for generating loop counter names
    pub(super) next_counter_id: usize,
    /// Current procedure name (for diagnostics)
    pub(super) current_proc_name: Option<String>,
    /// Whether provided contracts came from a local inference fallback (not workspace-wide).
    pub(super) contracts_are_fallback: bool,
}

impl<'a> PseudocodeCollector<'a> {
    pub fn new(
        module: &'a Module,
        sources: &'a DefaultSourceManager,
        contracts: Option<&'a ContractStore>,
        source_text: &'a str,
        contracts_are_fallback: bool,
    ) -> Self {
        Self {
            state: None,
            hints: Vec::new(),
            proc_hint_start: 0,
            proc_outputs: None,
            proc_decl_line: None,
            failures: Vec::new(),
            sources,
            resolver: crate::symbol_resolution::create_resolver(module),
            contracts,
            source_text,
            indent_level: 0,
            resolved_hints: Vec::new(),
            loop_stack: Vec::new(),
            next_counter_id: 0,
            current_proc_name: None,
            contracts_are_fallback,
        }
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Loop Context Methods
    // ─────────────────────────────────────────────────────────────────────────

    /// Generate a new loop counter name (i, j, k, ...).
    pub(super) fn new_counter(&mut self) -> String {
        let names = crate::decompiler::ssa::LOOP_COUNTER_NAMES;
        let name = if self.next_counter_id < names.len() {
            names[self.next_counter_id].to_string()
        } else {
            format!("i{}", self.next_counter_id - names.len())
        };
        self.next_counter_id += 1;
        name
    }

    /// Enter a new loop context.
    pub(super) fn enter_loop_context(
        &mut self,
        analyzed_stack: Vec<SymbolicExpr>,
        net_effect: i32,
        local_base: usize,
    ) -> String {
        let counter = self.new_counter();
        self.loop_stack.push(ActiveLoop::new(
            counter.clone(),
            analyzed_stack,
            net_effect,
            local_base,
        ));
        counter
    }

    /// Exit the current (innermost) loop context.
    pub(super) fn exit_loop_context(&mut self) {
        self.loop_stack.pop();
    }

    /// Get counter names for all active loops.
    pub(super) fn counter_names(&self) -> Vec<String> {
        self.loop_stack
            .iter()
            .map(|info| info.counter_name.clone())
            .collect()
    }

    /// Build a parametric map for the current template.
    pub(super) fn build_parametric_map(
        &self,
        template: &PseudocodeTemplate,
        counter_len: usize,
    ) -> HashMap<SsaId, ParametricExpr> {
        let mut map = HashMap::new();

        if self.loop_stack.is_empty() {
            return map;
        }

        let state = match &self.state {
            Some(s) => s,
            None => return map,
        };

        for id in template.ssa_ids() {
            if let Some(expr) = self.compute_parametric_expr(state, id, counter_len) {
                map.insert(id, expr);
            }
        }

        map
    }

    /// Compute a parametric expression for an SSA ID if it represents a loop-dependent position.
    pub(super) fn compute_parametric_expr(
        &self,
        state: &DecompilerState,
        id: SsaId,
        counter_len: usize,
    ) -> Option<ParametricExpr> {
        use crate::decompiler::ssa::VarKind;

        let value = state.ctx.get_value(id)?;

        let base_idx = match &value.kind {
            VarKind::Argument(idx) => Some(*idx),
            _ => state.ctx.argument_index_via_phi(id),
        };

        if base_idx.is_none() {
            return self.compute_output_parametric_expr(state, id, counter_len);
        }

        let base_idx = base_idx.unwrap();
        let mut merged: Option<ParametricExpr> = None;

        for loop_ctx in &self.loop_stack {
            if let Some(expr) = loop_ctx.expr_for_argument(base_idx) {
                match &expr {
                    SymbolicExpr::ParametricInput { loop_terms, .. } => {
                        if loop_terms.iter().all(|t| t.loop_depth < counter_len) {
                            if let Some(param_expr) = ParametricExpr::from_symbolic(&expr) {
                                merged = Some(match merged {
                                    None => param_expr,
                                    Some(mut existing) => {
                                        if existing.base == param_expr.base {
                                            existing.terms.extend(param_expr.terms);
                                            existing
                                        } else {
                                            existing
                                        }
                                    }
                                });
                            }
                        }
                    }
                    SymbolicExpr::Input(i) if *i == base_idx => {
                        if let Some(param_expr) = ParametricExpr::from_symbolic(&expr) {
                            merged.get_or_insert(param_expr);
                        }
                    }
                    _ => {}
                }
            }
        }

        merged
    }

    /// Build a parametric expression for loop-produced outputs (e.g., r_i).
    pub(super) fn compute_output_parametric_expr(
        &self,
        state: &DecompilerState,
        id: SsaId,
        counter_len: usize,
    ) -> Option<ParametricExpr> {
        if counter_len == 0 || self.loop_stack.is_empty() {
            return None;
        }

        let value = state.ctx.get_value(id)?;
        let base_idx = match value.kind {
            crate::decompiler::ssa::VarKind::Argument(i) => i as i32,
            crate::decompiler::ssa::VarKind::Local(i) => i as i32,
            crate::decompiler::ssa::VarKind::Return(i) => i as i32,
        };

        for (counter_idx, loop_ctx) in self.loop_stack.iter().enumerate().rev() {
            if counter_idx >= counter_len {
                continue;
            }
            if loop_ctx.net_effect < 0 {
                return Some(ParametricExpr::output(base_idx, counter_idx));
            } else {
                let produced = loop_ctx.net_effect.max(0) as usize;
                if produced > 0 {
                    if let crate::decompiler::ssa::VarKind::Local(idx) = value.kind {
                        if idx < loop_ctx.local_base + produced {
                            continue;
                        }
                    }
                }
                return Some(ParametricExpr::local(base_idx, counter_idx));
            }
        }

        None
    }

    /// Record a fatal decompilation failure for the current procedure.
    pub(super) fn fail_decompilation(&mut self, span: SourceSpan, reason: &str) {
        if let Some(ref mut state) = self.state {
            state.fail();
        }

        self.hints.truncate(self.proc_hint_start);

        let proc_name = self
            .current_proc_name
            .clone()
            .unwrap_or_else(|| "<unknown>".to_string());
        self.failures.push(TrackingFailure {
            span,
            reason: reason.to_string(),
            proc_name,
            related: None,
        });
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Hint Collection Methods
    // ─────────────────────────────────────────────────────────────────────────

    /// Add a literal hint (no SSA references to resolve).
    pub(super) fn add_literal_hint(&mut self, line: u32, text: &str) {
        let template = PseudocodeTemplate::new().literal(text);
        self.hints
            .push(SsaHint::new(line, template, self.indent_level));
    }

    /// Add a hint from a template, computing parametric map for loop-dependent positions.
    pub(super) fn add_template_hint(&mut self, line: u32, template: PseudocodeTemplate) {
        let counter_names = self.counter_names();
        let parametric_map = self.build_parametric_map(&template, counter_names.len());
        self.hints.push(SsaHint::with_parametric(
            line,
            template,
            self.indent_level,
            parametric_map,
            counter_names,
        ));
    }

    /// Generate pseudocode template for an instruction.
    pub(super) fn instruction_to_template(
        &mut self,
        inst: &miden_assembly_syntax::ast::Instruction,
        span: SourceSpan,
    ) -> Option<PseudocodeTemplate> {
        if self.state.as_ref().map_or(true, |s| s.tracking_failed) {
            return None;
        }

        self.handle_push_ops(inst)
            .or_else(|| self.handle_stack_ops(inst, span))
            .or_else(|| self.handle_arithmetic_ops(inst))
            .or_else(|| self.handle_comparison_ops(inst))
            .or_else(|| self.handle_boolean_ops(inst))
            .or_else(|| self.handle_ext2_ops(inst))
            .or_else(|| self.handle_u32_ops(inst))
            .or_else(|| self.handle_memory_ops(inst))
            .or_else(|| self.handle_local_memory_ops(inst))
            .or_else(|| self.handle_advice_ops(inst))
            .or_else(|| self.handle_crypto_ops(inst))
            .or_else(|| self.handle_merkle_ops(inst))
            .or_else(|| self.handle_word_stack_ops(inst, span))
            .or_else(|| self.handle_conditional_ops(inst))
            .or_else(|| self.handle_assertions(inst))
            .or_else(|| self.handle_misc_ops(inst))
            .or_else(|| self.handle_complex_stark_ops(inst))
            .or_else(|| self.handle_memory_stream_op(inst))
            .or_else(|| self.handle_proc_ref(inst))
            .or_else(|| self.handle_calls(inst, span))
            .or_else(|| self.handle_dynamic_calls(inst, span))
    }

    /// Move word at position n to top.
    pub(super) fn movupw(&mut self, word_n: usize) {
        if let Some(ref mut state) = self.state {
            let base = word_n * 4;
            for i in (0..4).rev() {
                state.movup(base + i);
            }
        }
    }

    /// Move top word to position n.
    pub(super) fn movdnw(&mut self, word_n: usize) {
        if let Some(ref mut state) = self.state {
            let target = word_n * 4;
            for i in 0..4 {
                state.movdn(target + i);
            }
        }
    }

    /// Swap two words on the stack.
    pub(super) fn swapw(&mut self, a: usize, b: usize) {
        if let Some(ref mut state) = self.state {
            let base_a = a * 4;
            let base_b = b * 4;
            for i in 0..4 {
                state.swap(base_a + i, base_b + i);
            }
        }
    }

    /// Generate dup template.
    pub(super) fn dup_template(&mut self, n: usize) -> Option<PseudocodeTemplate> {
        let state = self.state.as_mut()?;
        let src = state.peek(n)?;
        state.dup(n);
        let var = state.new_local();
        state.pop();
        state.push(var);

        let mut out = crate::decompiler::ssa::PseudocodeBuilder::new();
        out.var(var);
        out.text(" = ");
        out.var(src);
        Some(out.build())
    }

    /// Generate dupw template for word duplication.
    pub(super) fn dupw_template(&mut self, word_idx: usize) -> Option<PseudocodeTemplate> {
        let state = self.state.as_mut()?;
        let base = word_idx * 4;

        let mut sources = Vec::new();
        for i in 0..4 {
            if let Some(src) = state.peek(base + i) {
                sources.push(src);
            }
        }
        if sources.len() != 4 {
            return None;
        }

        for src in sources.iter().rev() {
            state.push(*src);
        }

        let mut new_vars = Vec::new();
        for _ in 0..4 {
            let var = state.new_local();
            state.pop();
            state.push(var);
            new_vars.push(var);
        }
        new_vars.reverse();

        let mut out = crate::decompiler::ssa::PseudocodeBuilder::new();
        out.text("(");
        for (i, var) in new_vars.iter().enumerate() {
            if i > 0 {
                out.text(", ");
            }
            out.var(var);
        }
        out.text(") = (");
        for (i, src) in sources.iter().rev().enumerate() {
            if i > 0 {
                out.text(", ");
            }
            out.var(src);
        }
        out.text(")");
        Some(out.build())
    }

    /// Visit all operations in a block, handling control flow.
    pub(super) fn visit_block_ssa(&mut self, block: &Block) {
        for op in block.iter() {
            if self.state.as_ref().map_or(false, |s| s.tracking_failed) {
                break;
            }
            self.visit_op_ssa(op);
        }
    }

    /// Visit a single operation in SSA mode.
    pub(super) fn visit_op_ssa(&mut self, op: &Op) {
        match op {
            Op::Inst(inst) => {
                if let Some(template) = self.instruction_to_template(inst.inner(), inst.span()) {
                    if let Some(range) = span_to_range(self.sources, inst.span()) {
                        self.add_template_hint(range.start.line, template);
                    }
                }
            }
            Op::If {
                then_blk, else_blk, ..
            } => {
                let condition_id = self.state.as_mut().and_then(|s| s.peek(0));
                let entry_stack = if let Some(ref mut state) = self.state {
                    state.pop();
                    Some(state.save_stack())
                } else {
                    None
                };

                if let Some(range) = span_to_range(self.sources, op.span()) {
                    let mut template = PseudocodeTemplate::new().literal("if ");
                    if let Some(cond_id) = condition_id {
                        template = template.ssa_ref(cond_id);
                    } else {
                        template = template.literal("?");
                    }
                    template = template.literal(":");
                    self.add_template_hint(range.start.line, template);
                }

                self.indent_level += 1;
                self.visit_block_ssa(then_blk);
                self.indent_level -= 1;

                let then_exit_stack = self.state.as_ref().map(|s| s.save_stack());

                if !else_blk.is_empty() {
                    if let Some(range) = span_to_range(self.sources, else_blk.span()) {
                        let else_line = range.start.line.saturating_sub(1);
                        self.add_literal_hint(else_line, "else");
                    }

                    if let (Some(ref mut state), Some(ref saved)) = (&mut self.state, &entry_stack)
                    {
                        state.restore_stack(saved);
                    }

                    self.indent_level += 1;
                    self.visit_block_ssa(else_blk);
                    self.indent_level -= 1;

                    if let (Some(ref mut state), Some(ref then_stack)) =
                        (&mut self.state, &then_exit_stack)
                    {
                        let else_stack = state.save_stack();
                        if then_stack.len() != else_stack.len() {
                            self.fail_decompilation(
                                op.span(),
                                "if branches have different stack effects",
                            );
                            return;
                        }
                        state.create_if_else_phis(then_stack, &else_stack);
                    }
                }

                if let Some(range) = span_to_range(self.sources, op.span()) {
                    self.add_literal_hint(range.end.line, "end");
                }
            }
            Op::While { body, .. } => {
                let analysis = analyze_while_loop(body);
                if analysis.failure_reason.is_some() || !analysis.is_consistent {
                    let span = op.span();
                    let reason = analysis
                        .failure_reason
                        .as_deref()
                        .unwrap_or("while loop stack effect unknown");
                    self.fail_decompilation(span, reason);
                    return;
                }

                let depth_offset = self.loop_stack.len();
                let analyzed_stack = shift_loop_stack(&analysis.post_iteration_stack, depth_offset);
                let condition_id = self.state.as_mut().and_then(|s| s.peek(0));

                let local_base = self
                    .state
                    .as_ref()
                    .map(|s| s.ctx.next_local_index())
                    .unwrap_or(0);
                self.enter_loop_context(
                    analyzed_stack,
                    analysis.net_effect_per_iteration,
                    local_base,
                );

                let loop_entry_stack = self.state.as_ref().map(|s| s.save_stack());
                if let Some(ref mut state) = self.state {
                    state.pop();
                }

                if let Some(range) = span_to_range(self.sources, op.span()) {
                    let mut template = PseudocodeTemplate::new().literal("while ");
                    if let Some(cond_id) = condition_id {
                        template = template.ssa_ref(cond_id);
                    } else {
                        template = template.literal("?");
                    }
                    template = template.literal(":");
                    self.add_template_hint(range.start.line, template);
                }

                self.indent_level += 1;
                self.visit_block_ssa(body);
                self.indent_level -= 1;

                if let (Some(ref mut state), Some(ref entry_stack)) =
                    (&mut self.state, &loop_entry_stack)
                {
                    let exit_depth = state.depth();
                    let entry_depth = entry_stack.len();
                    if exit_depth > entry_depth {
                        let drop_count = exit_depth - entry_depth;
                        for _ in 0..drop_count {
                            state.pop();
                        }
                    }

                    if let Err(err) = state.create_loop_phis(entry_stack) {
                        self.fail_decompilation(
                            op.span(),
                            &format!("while loop phi creation failed: {err}"),
                        );
                        return;
                    }
                }

                self.exit_loop_context();

                if let Some(range) = span_to_range(self.sources, op.span()) {
                    self.add_literal_hint(range.end.line, "end");
                }
            }
            Op::Repeat { count, body, .. } => {
                let loop_analysis = analyze_repeat_loop(body, *count as usize);
                let net_effect = loop_analysis.net_effect_per_iteration;
                let iterations_remaining = count.saturating_sub(1);

                if loop_analysis.failure_reason.is_some() || !loop_analysis.is_consistent {
                    let span = op.span();
                    let reason = loop_analysis
                        .failure_reason
                        .as_deref()
                        .unwrap_or("repeat loop stack effect unknown");
                    self.fail_decompilation(span, reason);
                    return;
                }

                let depth_offset = self.loop_stack.len();
                let analyzed_stack =
                    shift_loop_stack(&loop_analysis.post_iteration_stack, depth_offset);

                let local_base = self
                    .state
                    .as_ref()
                    .map(|s| s.ctx.next_local_index())
                    .unwrap_or(0);
                let counter = self.enter_loop_context(analyzed_stack, net_effect, local_base);

                let loop_entry_stack = self.state.as_ref().map(|s| s.save_stack());

                if let Some(range) = span_to_range(self.sources, op.span()) {
                    let hint = format!(
                        "for {} = 0; {} < {}; {} = {} + 1:",
                        counter, counter, count, counter, counter
                    );
                    self.add_literal_hint(range.start.line, &hint);
                }

                self.indent_level += 1;
                self.visit_block_ssa(body);
                self.indent_level -= 1;

                let mut adjusted_remaining_iters = false;
                if net_effect < 0 && iterations_remaining > 0 {
                    if let (Some(ref mut state), Some(ref entry_stack)) =
                        (&mut self.state, &loop_entry_stack)
                    {
                        let entry_depth = entry_stack.len();
                        let final_depth =
                            (entry_depth as i32 + net_effect * (*count as i32)).max(0) as usize;
                        let loop_inputs = loop_analysis
                            .total_inputs_for_loop
                            .unwrap_or(loop_analysis.min_inputs_required);
                        let unaffected = entry_depth.saturating_sub(loop_inputs);
                        let outputs_count = final_depth.saturating_sub(unaffected);
                        let local_base = self
                            .loop_stack
                            .last()
                            .map(|l| l.local_base)
                            .unwrap_or_else(|| state.ctx.next_local_index());

                        let current_stack = state.save_stack();
                        let mut outputs: Vec<SsaId> = current_stack
                            .iter()
                            .skip(unaffected)
                            .filter(|id| {
                                state.ctx.get_value(**id).map_or(false, |v| match v.kind {
                                    crate::decompiler::ssa::VarKind::Local(idx) => {
                                        idx >= local_base
                                    }
                                    _ => false,
                                })
                            })
                            .cloned()
                            .collect();

                        outputs.truncate(outputs_count);
                        while outputs.len() < outputs_count {
                            outputs.push(state.new_local());
                        }

                        let mut new_stack: Vec<SsaId> =
                            current_stack.into_iter().take(unaffected).collect();
                        new_stack.extend(outputs.into_iter());
                        state.restore_stack(&new_stack);
                        adjusted_remaining_iters = true;
                    }
                }

                if net_effect == 0 {
                    if let (Some(ref mut state), Some(ref entry_stack)) =
                        (&mut self.state, &loop_entry_stack)
                    {
                        if state.depth() != entry_stack.len() {
                            self.fail_decompilation(
                                op.span(),
                                "repeat loop branches with mismatched stack depth",
                            );
                            return;
                        }
                        if let Err(err) = state.create_loop_phis(entry_stack) {
                            self.fail_decompilation(
                                op.span(),
                                &format!("repeat loop phi creation failed: {err}"),
                            );
                            return;
                        }
                    }
                }

                self.exit_loop_context();

                if let Some(ref mut state) = self.state {
                    let iterations_remaining = iterations_remaining as i32;
                    if iterations_remaining > 0 && net_effect != 0 && !adjusted_remaining_iters {
                        let total_effect = net_effect * iterations_remaining;
                        if total_effect < 0 && state.depth() < (-total_effect) as usize {
                            self.fail_decompilation(
                                op.span(),
                                "repeat loop consumes more stack elements than available",
                            );
                            return;
                        }
                        if state.apply_net_effect(total_effect).is_err() {
                            self.fail_decompilation(
                                op.span(),
                                "repeat loop consumes more stack elements than available",
                            );
                            return;
                        }
                    }
                }

                if let Some(range) = span_to_range(self.sources, op.span()) {
                    self.add_literal_hint(range.end.line, "end");
                }
            }
        }
    }

    /// Resolve all hints after visiting the AST.
    pub(super) fn resolve_all_hints(&mut self) {
        if let Some(ref mut state) = self.state {
            state.ctx.resolve_names();
            for hint in &self.hints {
                let resolved = hint.resolve(&state.ctx);
                self.resolved_hints.push((hint.line, resolved));
            }
        }
        self.hints.clear();
    }
}

impl<'a> Visit for PseudocodeCollector<'a> {
    fn visit_procedure(&mut self, proc: &Procedure) -> core::ops::ControlFlow<()> {
        let proc_name = proc.name().as_str().to_string();
        self.current_proc_name = Some(proc_name.clone());

        let decl_range = span_to_range(self.sources, proc.name().span());
        let decl_line = decl_range.as_ref().map(|r| r.start.line);

        self.loop_stack.clear();
        self.next_counter_id = 0;

        let contract = self.contracts.and_then(|c| c.get_by_name(&proc_name));
        let contract_effect = contract.map(|c| c.stack_effect.clone());
        let contract_signature = if self.contracts_are_fallback {
            None
        } else {
            contract.and_then(|c| c.signature.clone())
        };

        let pre_analysis = pre_analyze_procedure(proc.body());
        let pre_analyzed_inputs = pre_analysis.total_inputs_required;

        let (initial_input_count, output_count) = if self.contracts_are_fallback {
            (pre_analyzed_inputs, None)
        } else {
            match &contract_effect {
                Some(StackEffect::Known { inputs, outputs }) => (*inputs, Some(*outputs)),
                Some(StackEffect::KnownInputs { inputs }) => (*inputs, None),
                _ => (pre_analyzed_inputs, None),
            }
        };

        self.proc_hint_start = self.hints.len();
        self.proc_outputs = output_count;
        self.proc_decl_line = decl_line;

        if let Some(line) = decl_line {
            let decl_prefix = extract_declaration_prefix(self.source_text, line);
            let signature = format_procedure_signature(
                &decl_prefix,
                &proc_name,
                proc.visibility(),
                initial_input_count,
                output_count,
                contract_signature.as_ref(),
                None,
                false,
            );
            self.add_literal_hint(line, &signature);
        }

        self.state = Some(DecompilerState::new(initial_input_count));

        self.indent_level = 1;
        self.visit_block_ssa(proc.body());
        self.indent_level = 0;

        if let Some(ref state) = self.state {
            if state.tracking_failed {
                self.hints.truncate(self.proc_hint_start);
                self.state = None;
                self.proc_outputs = None;
                self.proc_decl_line = None;
                return core::ops::ControlFlow::Continue(());
            }
        }

        let final_input_count = self
            .state
            .as_ref()
            .map(|s| s.ctx.argument_count())
            .unwrap_or(initial_input_count);

        let mut final_output_count = output_count;
        let mut output_names: Option<Vec<String>> = None;
        if let Some(ref mut state) = self.state {
            let inferred = final_output_count.unwrap_or_else(|| state.depth());
            if inferred > 0 {
                state.mark_returns(inferred);
                final_output_count = Some(inferred);
                state.ctx.resolve_names();
                output_names = Some(state.ctx.return_names());
            }
        }
        self.proc_outputs = final_output_count;

        if contract_signature.is_none() {
            let should_refresh = final_input_count != initial_input_count
                || final_output_count.is_some()
                || output_names.is_some();
            if should_refresh {
                if let Some(line) = decl_line {
                    let decl_prefix = extract_declaration_prefix(self.source_text, line);
                    let signature = format_procedure_signature(
                        &decl_prefix,
                        &proc_name,
                        proc.visibility(),
                        final_input_count,
                        final_output_count,
                        None,
                        output_names.as_deref(),
                        false,
                    );
                    if let Some(sig_hint) = self.hints.get_mut(self.proc_hint_start) {
                        *sig_hint = SsaHint::new(
                            sig_hint.line,
                            PseudocodeTemplate::new().literal(&signature),
                            sig_hint.indent_level,
                        );
                    }
                }
            }
        }

        self.resolve_all_hints();

        if let Some(body_range) = span_to_range(self.sources, proc.body().span()) {
            let search_start = body_range.end.line.saturating_add(1) as usize;
            let end_line = self
                .source_text
                .lines()
                .enumerate()
                .skip(search_start)
                .find_map(|(idx, line)| (line.trim() == "end").then_some(idx as u32))
                .unwrap_or(body_range.end.line);

            let already_has_end = self
                .resolved_hints
                .iter()
                .any(|(line, hint)| *line == end_line && hint.trim() == "end");
            if !already_has_end {
                self.resolved_hints.push((end_line, "end".to_string()));
            }
        }

        self.state = None;
        self.proc_outputs = None;
        self.proc_decl_line = None;
        self.current_proc_name = None;

        core::ops::ControlFlow::Continue(())
    }
}
