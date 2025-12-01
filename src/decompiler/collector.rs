//! Decompilation hint collection and visitor logic.
//!
//! This module contains the `DecompilationCollector` which traverses the AST
//! and generates pseudocode hints for each procedure.

use std::collections::HashMap;

use miden_assembly_syntax::ast::visit::{self, Visit};
use miden_assembly_syntax::ast::{Block, Module, Op, Procedure};
use miden_debug_types::{DefaultSourceManager, SourceSpan, Spanned};
use tower_lsp::lsp_types::{
    Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, InlayHint, InlayHintKind,
    InlayHintLabel, Location, Position, Range, Url,
};

use crate::analysis::{
    analyze_repeat_loop, pre_analyze_procedure, ContractStore,
    StackEffect,
};
use crate::diagnostics::{span_to_range, SOURCE_DECOMPILATION};
use crate::symbol_resolution::SymbolResolver;

use super::pseudocode::{
    apply_counter_indexing, apply_output_indexing, extract_declaration_prefix,
    format_procedure_signature, rename_variable, ToPseudocode,
};
use super::state::DecompilerState;

// ═══════════════════════════════════════════════════════════════════════════
// Hint Collection Types
// ═══════════════════════════════════════════════════════════════════════════

/// Related information for a tracking failure diagnostic.
#[derive(Clone)]
pub struct RelatedInfo {
    /// The span of the related location
    pub span: SourceSpan,
    /// Message explaining the related location
    pub message: String,
}

/// A tracking failure that should be reported as a diagnostic.
pub(crate) struct TrackingFailure {
    /// The span where decompilation failed
    pub span: SourceSpan,
    /// The reason for the failure
    pub reason: String,
    /// The name of the procedure where failure occurred
    pub proc_name: String,
    /// Optional related information (e.g., the root cause in another procedure)
    pub related: Option<RelatedInfo>,
}

/// Collector that visits procedures and instructions.
pub struct DecompilationCollector<'a> {
    /// Current decompiler state
    state: Option<DecompilerState>,
    /// Collected hints: (line, hint_text)
    hints: Vec<(u32, String)>,
    /// Index of the first hint for the current procedure (for renaming returns)
    proc_hint_start: usize,
    /// Number of outputs for current procedure (for renaming returns)
    proc_outputs: Option<usize>,
    /// Line number of the procedure declaration (for updating signature)
    proc_decl_line: Option<u32>,
    /// Collected tracking failures for diagnostics
    failures: Vec<TrackingFailure>,
    /// Source manager for span conversion
    sources: &'a DefaultSourceManager,
    /// Symbol resolver for resolving invocation targets to fully-qualified paths
    resolver: SymbolResolver<'a>,
    /// Contract store for looking up procedure stack effects
    contracts: Option<&'a ContractStore>,
    /// Source text for extracting declaration prefixes
    source_text: &'a str,
    /// Current indentation level (number of 4-space indents)
    indent_level: usize,
}

impl<'a> DecompilationCollector<'a> {
    pub fn new(
        module: &'a Module,
        sources: &'a DefaultSourceManager,
        contracts: Option<&'a ContractStore>,
        source_text: &'a str,
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
        }
    }

    /// Get the current indentation string.
    fn indent(&self) -> String {
        "    ".repeat(self.indent_level)
    }

    /// Visit all operations in a block, handling control flow.
    fn visit_block(&mut self, block: &Block) {
        for op in block.iter() {
            self.visit_op(op);
        }
    }

    /// Visit a single operation, generating hints and handling control flow.
    fn visit_op(&mut self, op: &Op) {
        match op {
            Op::Inst(inst) => {
                if let Some(ref mut state) = self.state {
                    if let Some(pseudocode) = inst.inner().to_pseudocode(state, inst.span(), Some(&self.resolver), self.contracts) {
                        if let Some(range) = span_to_range(self.sources, inst.span()) {
                            let indented = format!("{}{}", self.indent(), pseudocode);
                            self.hints.push((range.start.line, indented));
                        }
                    }
                }
            }
            Op::If { then_blk, else_blk, .. } => {
                // Get condition variable from top of stack (if.true consumes it)
                let condition = self.state.as_ref()
                    .and_then(|s| s.stack.last())
                    .map(|v| v.name.clone())
                    .unwrap_or_else(|| "?".to_string());

                // Pop the condition from the stack and save state for else branch
                let entry_state = if let Some(ref mut state) = self.state {
                    state.stack.pop();
                    Some(state.save_state())
                } else {
                    None
                };

                // Generate hint for "if.true" with condition
                if let Some(range) = span_to_range(self.sources, op.span()) {
                    let hint = format!("{}if {}:", self.indent(), condition);
                    self.hints.push((range.start.line, hint));
                }

                // Visit then block with increased indent
                self.indent_level += 1;
                self.visit_block(then_blk);
                self.indent_level -= 1;

                // Save then-branch exit state for merge
                let then_exit_state = self.state.as_ref().map(|s| s.save_state());

                // Generate hint for "else" if else block is non-empty
                if !else_blk.is_empty() {
                    if let Some(range) = span_to_range(self.sources, else_blk.span()) {
                        // The else keyword is on the line before the else block starts
                        let else_line = range.start.line.saturating_sub(1);
                        let hint = format!("{}else", self.indent());
                        self.hints.push((else_line, hint));
                    }

                    // Restore entry state for else branch (crucial fix!)
                    if let (Some(ref mut state), Some(ref saved)) = (&mut self.state, &entry_state) {
                        state.restore_state(saved);
                    }

                    // Track where else-branch hints start for potential renaming
                    let else_hint_start = self.hints.len();

                    self.indent_level += 1;
                    self.visit_block(else_blk);
                    self.indent_level -= 1;

                    // At merge point: rename else-branch variables to match then-branch positions
                    // This ensures consistent variable names after the if/else
                    if let (Some(ref mut state), Some(ref then_state)) = (&mut self.state, &then_exit_state) {
                        let renames = state.get_rename_map(then_state);
                        if !renames.is_empty() {
                            // Apply renames to else-branch hints
                            for hint in &mut self.hints[else_hint_start..] {
                                for (from, to) in &renames {
                                    hint.1 = rename_variable(&hint.1, from, to);
                                }
                            }
                            // Update state to use the then-branch variable names
                            state.apply_renames(&renames);
                        }
                    }
                }

                // Generate hint for "end"
                if let Some(range) = span_to_range(self.sources, op.span()) {
                    let hint = format!("{}end", self.indent());
                    self.hints.push((range.end.line, hint));
                }
            }
            Op::While { body, .. } => {
                // Get condition variable from top of stack
                // while.true checks and CONSUMES the condition each iteration
                let condition = self.state.as_ref()
                    .and_then(|s| s.stack.last())
                    .map(|v| v.name.clone())
                    .unwrap_or_else(|| "?".to_string());

                // Save the loop entry state (with condition on top)
                // This represents the expected stack shape at each iteration start
                let loop_entry_state = self.state.as_ref().map(|s| s.save_state());

                // Pop the condition from the stack (while.true consumes it)
                // and record entry depth (after condition pop) for net effect calculation
                let entry_depth = if let Some(ref mut state) = self.state {
                    state.stack.pop();
                    state.stack.len()
                } else {
                    0
                };

                // Track start_var_id before visiting loop body (for output indexing)
                let start_var_id = self.state.as_ref().map(|s| s.next_var_id).unwrap_or(0);

                // Track where while loop hint will be (index for potential modification)
                let while_hint_idx = self.hints.len();

                // Generate hint for "while.true" with condition (may be updated later with counter init)
                if let Some(range) = span_to_range(self.sources, op.span()) {
                    let hint = format!("{}while {}:", self.indent(), condition);
                    self.hints.push((range.start.line, hint));
                }

                // Track where loop body hints start for potential modification
                let loop_body_hint_start = self.hints.len();

                // Visit body with increased indent
                self.indent_level += 1;
                self.visit_block(body);
                self.indent_level -= 1;

                // Calculate vars_per_iteration (for output indexing)
                let end_var_id = self.state.as_ref().map(|s| s.next_var_id).unwrap_or(0);
                let vars_per_iteration = end_var_id.saturating_sub(start_var_id);

                // Calculate net stack effect per iteration
                // Note: at loop end, there's a new condition on top, so we compare to entry_depth + 1
                let exit_depth = self.state.as_ref().map(|s| s.stack.len()).unwrap_or(0);
                let net_effect = exit_depth as i32 - (entry_depth as i32 + 1); // +1 for the new condition

                if net_effect != 0 {
                    // Check if a previous loop produced dynamic stack content
                    // If so, we can't reliably decompile this loop
                    if let Some(ref state) = self.state {
                        if state.dynamic_stack_source.is_some() {
                            // Fail decompilation - we have dynamic stack content from a previous loop
                            if let Some(ref mut state) = self.state {
                                state.fail_tracking(
                                    op.span(),
                                    "loop with variable iteration count preceded by another loop that modified stack size"
                                );
                            }
                            // Remove hints generated for this loop
                            self.hints.truncate(while_hint_idx);
                            return;
                        }
                    }

                    // Non-zero net effect: stack positions shift each iteration
                    // Generate a counter and apply indexing
                    let counter = self.state.as_mut().map(|s| s.new_counter()).unwrap_or_else(|| "i".to_string());

                    // Update the while hint to include counter initialization
                    // Format: "while i = 0; condition:" instead of separate "i = 0" line
                    if while_hint_idx < self.hints.len() {
                        self.hints[while_hint_idx].1 = format!("{}while {} = 0; {}:", self.indent(), counter, condition);
                    }

                    // Apply counter indexing to both input and output variable references in loop body
                    for hint in &mut self.hints[loop_body_hint_start..] {
                        hint.1 = apply_counter_indexing(&hint.1, &counter, net_effect);
                        hint.1 = apply_output_indexing(&hint.1, &counter, start_var_id, vars_per_iteration);
                    }

                    // Mark stack as dynamic for subsequent loops
                    if let Some(ref mut state) = self.state {
                        state.dynamic_stack_source = Some(op.span());
                    }
                } else {
                    // Zero net effect: stack shape is preserved, use renaming for consistency
                    if let (Some(ref mut state), Some(ref entry_state)) = (&mut self.state, &loop_entry_state) {
                        let renames = state.get_rename_map(entry_state);
                        if !renames.is_empty() {
                            // Apply renames to all hints generated in the loop body
                            for hint in &mut self.hints[loop_body_hint_start..] {
                                for (from, to) in &renames {
                                    hint.1 = rename_variable(&hint.1, from, to);
                                }
                            }
                            // Update state to use the entry variable names
                            state.apply_renames(&renames);
                        }
                    }
                }

                // Generate hint for "end"
                if let Some(range) = span_to_range(self.sources, op.span()) {
                    let hint = format!("{}end", self.indent());
                    self.hints.push((range.end.line, hint));
                }
            }
            Op::Repeat { count, body, .. } => {
                // Track where repeat loop hint will be (index for potential removal on failure)
                let repeat_hint_idx = self.hints.len();

                // Generate a loop counter for this repeat loop
                let counter = self.state.as_mut().map(|s| s.new_counter()).unwrap_or_else(|| "i".to_string());

                // Use abstract interpretation to analyze the loop
                let loop_analysis = analyze_repeat_loop(body, *count as usize);

                // Save the loop entry state for calculating net effect
                let loop_entry_state = self.state.as_ref().map(|s| s.save_state());
                let entry_depth = self.state.as_ref().map(|s| s.stack.len()).unwrap_or(0);

                // Track start_var_id before visiting loop body (for output indexing)
                let start_var_id = self.state.as_ref().map(|s| s.next_var_id).unwrap_or(0);

                // If pre-analysis discovered we need more inputs, ensure we have them
                if let Some(ref mut state) = self.state {
                    if let Some(total_inputs) = loop_analysis.total_inputs_for_loop {
                        use crate::analysis::stack_ops::StackLike;
                        state.ensure_depth(total_inputs);
                    }
                }

                // Generate hint for "for c in 0..N:" (replaces "repeat N times:")
                if let Some(range) = span_to_range(self.sources, op.span()) {
                    let hint = format!("{}for {} in 0..{}:", self.indent(), counter, count);
                    self.hints.push((range.start.line, hint));
                }

                // Track where loop body hints start for potential modification
                let loop_body_hint_start = self.hints.len();

                // Visit body with increased indent
                self.indent_level += 1;
                self.visit_block(body);
                self.indent_level -= 1;

                // Calculate vars_per_iteration (for output indexing)
                let end_var_id = self.state.as_ref().map(|s| s.next_var_id).unwrap_or(0);
                let vars_per_iteration = end_var_id.saturating_sub(start_var_id);

                // Use the pre-analyzed net effect (more accurate than measuring after dynamic discovery)
                let net_effect = loop_analysis.net_effect_per_iteration;

                // Fall back to measured effect if pre-analysis failed
                let net_effect = if loop_analysis.is_consistent {
                    net_effect
                } else {
                    let exit_depth = self.state.as_ref().map(|s| s.stack.len()).unwrap_or(0);
                    exit_depth as i32 - entry_depth as i32
                };

                if net_effect != 0 {
                    // Check if a previous loop produced dynamic stack content
                    // If so, we can't reliably decompile this loop
                    if let Some(ref state) = self.state {
                        if state.dynamic_stack_source.is_some() {
                            // Fail decompilation - we have dynamic stack content from a previous loop
                            if let Some(ref mut state) = self.state {
                                state.fail_tracking(
                                    op.span(),
                                    "loop with non-zero stack effect preceded by another loop that modified stack size"
                                );
                            }
                            // Remove hints generated for this loop
                            self.hints.truncate(repeat_hint_idx);
                            return;
                        }
                    }

                    // Non-zero net effect: stack positions shift each iteration
                    // Apply counter indexing to both input and output variable references
                    for hint in &mut self.hints[loop_body_hint_start..] {
                        hint.1 = apply_counter_indexing(&hint.1, &counter, net_effect);
                        hint.1 = apply_output_indexing(&hint.1, &counter, start_var_id, vars_per_iteration);
                    }

                    // Mark stack as dynamic for subsequent loops
                    if let Some(ref mut state) = self.state {
                        state.dynamic_stack_source = Some(op.span());
                    }
                } else {
                    // Zero net effect: stack shape is preserved, use renaming for consistency
                    if let (Some(ref mut state), Some(ref entry_state)) = (&mut self.state, &loop_entry_state) {
                        let renames = state.get_rename_map(entry_state);
                        if !renames.is_empty() {
                            // Apply renames to all hints generated in the loop body
                            for hint in &mut self.hints[loop_body_hint_start..] {
                                for (from, to) in &renames {
                                    hint.1 = rename_variable(&hint.1, from, to);
                                }
                            }
                            // Update state to use the entry variable names
                            state.apply_renames(&renames);
                        }
                    }
                }

                // Generate hint for "end"
                if let Some(range) = span_to_range(self.sources, op.span()) {
                    let hint = format!("{}end", self.indent());
                    self.hints.push((range.end.line, hint));
                }
            }
        }
    }
}

impl<'a> Visit for DecompilationCollector<'a> {
    fn visit_procedure(&mut self, proc: &Procedure) -> core::ops::ControlFlow<()> {
        let proc_name = proc.name().as_str().to_string();

        let decl_range = span_to_range(self.sources, proc.name().span());
        let decl_line = decl_range.as_ref().map(|r| r.start.line);

        // Priority for determining input/output counts:
        // 1. Contract store (inferred from implementation - source of truth)
        // 2. Pre-analysis via abstract interpretation (discovers inputs from loops)
        // 3. Default to 0 (dynamic discovery will find inputs as needed)
        //
        // Note: We intentionally do NOT use the explicit signature annotation as the
        // source of truth. The implementation defines the actual stack effect, and
        // the annotation might be incorrect or use complex types we can't parse.

        // Get contract from store (inferred from the actual implementation)
        let contract = self.contracts.and_then(|c| c.get_by_name(&proc_name));
        let contract_effect = contract.map(|c| c.stack_effect.clone());
        let contract_signature = contract.and_then(|c| c.signature.clone());

        // Pre-analyze procedure to discover inputs needed (especially for loops)
        let pre_analysis = pre_analyze_procedure(proc.body());
        let pre_analyzed_inputs = pre_analysis.total_inputs_required;

        // Determine initial input/output counts from contract inference
        let (initial_input_count, output_count) = match &contract_effect {
            Some(StackEffect::Known { inputs, outputs }) => {
                ((*inputs).max(pre_analyzed_inputs), Some(*outputs))
            }
            Some(StackEffect::KnownInputs { inputs }) => {
                ((*inputs).max(pre_analyzed_inputs), None)
            }
            _ => (pre_analyzed_inputs, None), // Use pre-analyzed inputs
        };

        // Track where this procedure's hints start (for renaming returns and updating signature)
        self.proc_hint_start = self.hints.len();
        self.proc_outputs = output_count;
        self.proc_decl_line = decl_line;

        // Generate initial signature hint for the procedure declaration
        if let Some(line) = decl_line {
            let decl_prefix = extract_declaration_prefix(self.source_text, line);
            let signature = format_procedure_signature(&decl_prefix, &proc_name, initial_input_count, output_count, contract_signature.as_ref());
            self.hints.push((line, signature));
        }

        self.state = Some(DecompilerState::new(initial_input_count));

        // Visit procedure body with indentation
        self.indent_level = 1;
        self.visit_block(proc.body());
        self.indent_level = 0;

        // After visiting, check if tracking failed and record the failure
        if let Some(ref state) = self.state {
            if state.tracking_failed {
                if let (Some(span), Some(reason)) = (state.failure_span, state.failure_reason.clone()) {
                    // Convert state's related info to TrackingFailure's related info
                    let related = state.failure_related.as_ref().map(|r| RelatedInfo {
                        span: r.span,
                        message: r.message.clone(),
                    });
                    self.failures.push(TrackingFailure {
                        span,
                        reason,
                        proc_name: proc_name.clone(),
                        related,
                    });
                }
                // Remove all hints for this procedure when decompilation fails
                self.hints.truncate(self.proc_hint_start);
                // Clear state and return early - skip signature updates and return renaming
                self.state = None;
                self.proc_outputs = None;
                self.proc_decl_line = None;
                return core::ops::ControlFlow::Continue(());
            }
        }

        // Check if we discovered more inputs than initially expected
        // and update the signature hint accordingly
        if let Some(ref state) = self.state {
            let discovered_inputs = state.total_inputs();
            if discovered_inputs > initial_input_count {
                // Update the signature hint with the discovered input count
                if let Some(line) = self.proc_decl_line {
                    // Find and update the signature hint for this procedure
                    if let Some(sig_hint) = self.hints.iter_mut().find(|(l, _)| *l == line) {
                        let decl_prefix = extract_declaration_prefix(self.source_text, line);
                        sig_hint.1 = format_procedure_signature(&decl_prefix, &proc_name, discovered_inputs, output_count, contract_signature.as_ref());
                    }
                }
            }
        }

        // Rename final stack values to r_0, r_1, ... if we know the output count
        if let (Some(ref state), Some(outputs)) = (&self.state, self.proc_outputs) {
            if !state.tracking_failed && outputs > 0 {
                // Build a map from final stack variable names to return names (0-indexed)
                let mut rename_map: Vec<(String, String)> = Vec::new();
                for (i, named_value) in state.stack.iter().rev().take(outputs).enumerate() {
                    let return_name = format!("r_{}", i);
                    if named_value.name != return_name {
                        rename_map.push((named_value.name.clone(), return_name));
                    }
                }

                // Apply renames to all hints for this procedure
                for hint in &mut self.hints[self.proc_hint_start..] {
                    for (old_name, new_name) in &rename_map {
                        // Replace as whole word (with word boundaries)
                        hint.1 = rename_variable(&hint.1, old_name, new_name);
                    }
                }
            }
        }

        // Add "end" hint for procedure by finding the "end" keyword in source
        if let Some(body_range) = span_to_range(self.sources, proc.body().span()) {
            // Scan source lines starting from body end to find "end" keyword
            let start_line = body_range.end.line as usize;
            for (offset, line_text) in self.source_text.lines().skip(start_line).enumerate() {
                if line_text.trim() == "end" {
                    let end_line = (start_line + offset) as u32;
                    self.hints.push((end_line, "end".to_string()));
                    break;
                }
            }
        }

        // Clear state for next procedure
        self.state = None;
        self.proc_outputs = None;
        self.proc_decl_line = None;

        core::ops::ControlFlow::Continue(())
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Public API
// ═══════════════════════════════════════════════════════════════════════════

/// Result of decompilation hint collection.
pub struct DecompilationResult {
    /// Inlay hints for pseudocode
    pub hints: Vec<InlayHint>,
    /// Diagnostics for tracking failures
    pub diagnostics: Vec<Diagnostic>,
}

/// Collect decompilation hints for all procedures in a module.
pub fn collect_decompilation_hints(
    module: &Module,
    sources: &DefaultSourceManager,
    uri: &Url,
    visible_range: &Range,
    tab_count: usize,
    source_text: &str,
    contracts: Option<&ContractStore>,
) -> DecompilationResult {
    let mut collector = DecompilationCollector::new(module, sources, contracts, source_text);
    let _ = visit::visit_module(&mut collector, module);

    // Fixed padding (in columns) between instruction end and hint.
    let padding = (tab_count.max(1)) as u32;

    // Pre-compute line lengths (trimmed of trailing whitespace) for positioning hints
    let line_lengths: Vec<u32> = source_text
        .lines()
        .map(|line| line.trim_end().len() as u32)
        .collect();

    // Group hints by line (in case multiple instructions on same line)
    let mut line_hints: HashMap<u32, Vec<String>> = HashMap::new();
    for (line, hint) in collector.hints {
        // Filter by visible range
        if line >= visible_range.start.line && line <= visible_range.end.line {
            line_hints.entry(line).or_default().push(hint);
        }
    }

    // Convert to InlayHints
    let mut hints: Vec<InlayHint> = line_hints
        .into_iter()
        .map(|(line_num, pseudocodes)| {
            let line_end = line_lengths.get(line_num as usize).copied().unwrap_or(0);
            let label = pseudocodes.join("; ");

            InlayHint {
                position: Position {
                    line: line_num,
                    character: line_end.saturating_add(padding),
                },
                label: InlayHintLabel::String(label),
                kind: Some(InlayHintKind::TYPE),
                text_edits: None,
                tooltip: None,
                padding_left: None,
                padding_right: None,
                data: None,
            }
        })
        .collect();

    // Sort hints by position
    hints.sort_by(|a, b| {
        a.position
            .line
            .cmp(&b.position.line)
            .then_with(|| a.position.character.cmp(&b.position.character))
    });

    // Convert tracking failures to diagnostics
    let diagnostics: Vec<Diagnostic> = collector
        .failures
        .into_iter()
        .filter_map(|failure| {
            let range = span_to_range(sources, failure.span)?;

            // Convert related info to DiagnosticRelatedInformation
            let related_information = failure.related.and_then(|related| {
                let related_range = span_to_range(sources, related.span)?;
                Some(vec![DiagnosticRelatedInformation {
                    location: Location {
                        uri: uri.clone(),
                        range: related_range,
                    },
                    message: related.message,
                }])
            });

            Some(Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::WARNING),
                code: None,
                code_description: None,
                source: Some(SOURCE_DECOMPILATION.to_string()),
                message: format!(
                    "Pseudocode unavailable in `{}`: {}",
                    failure.proc_name, failure.reason
                ),
                related_information,
                tags: None,
                data: None,
            })
        })
        .collect();

    DecompilationResult { hints, diagnostics }
}
