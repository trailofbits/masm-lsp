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

use crate::analysis::{pre_analyze_procedure, ContractStore, StackEffect};
use crate::diagnostics::{span_to_range, SOURCE_DECOMPILATION};
use crate::symbol_resolution::SymbolResolver;

use super::pseudocode::{extract_declaration_prefix, format_procedure_signature};
use super::ssa::{
    PseudocodeTemplate, SsaContext, SsaDecompilerState,
    TemplateOutput, PseudocodeOutput,
};

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

// ═══════════════════════════════════════════════════════════════════════════
// SSA-based Decompilation Collector
// ═══════════════════════════════════════════════════════════════════════════

/// A hint template that will be resolved to a final string after phi resolution.
#[derive(Clone)]
struct SsaHint {
    /// Line number for the hint
    line: u32,
    /// Template to resolve
    template: PseudocodeTemplate,
    /// Indentation level when this hint was created
    indent_level: usize,
}

impl SsaHint {
    fn new(line: u32, template: PseudocodeTemplate, indent_level: usize) -> Self {
        Self { line, template, indent_level }
    }

    /// Resolve to a final hint string with proper indentation.
    fn resolve(&self, ctx: &SsaContext) -> String {
        let indent = "    ".repeat(self.indent_level);
        let content = self.template.resolve(ctx);
        format!("{}{}", indent, content)
    }
}

/// SSA-based collector that uses phi nodes for variable tracking at merge points.
///
/// This collector uses a two-pass approach:
/// 1. **Pass 1**: Visit AST, generate templates with SSA IDs, create phi nodes at merge points
/// 2. **Pass 2**: Resolve phi relationships, convert templates to final strings
pub struct SsaDecompilationCollector<'a> {
    /// Current SSA decompiler state
    state: Option<SsaDecompilerState>,
    /// Collected hint templates: (line, template, indent_level)
    hints: Vec<SsaHint>,
    /// Index of the first hint for the current procedure
    proc_hint_start: usize,
    /// Number of outputs for current procedure
    proc_outputs: Option<usize>,
    /// Line number of the procedure declaration
    proc_decl_line: Option<u32>,
    /// Collected tracking failures for diagnostics
    failures: Vec<TrackingFailure>,
    /// Source manager for span conversion
    sources: &'a DefaultSourceManager,
    /// Symbol resolver for resolving invocation targets (reserved for procedure calls)
    #[allow(dead_code)]
    resolver: SymbolResolver<'a>,
    /// Contract store for looking up procedure stack effects
    contracts: Option<&'a ContractStore>,
    /// Source text for extracting declaration prefixes
    source_text: &'a str,
    /// Current indentation level
    indent_level: usize,
    /// Finalized hints (after resolution)
    resolved_hints: Vec<(u32, String)>,
}

impl<'a> SsaDecompilationCollector<'a> {
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
            resolved_hints: Vec::new(),
        }
    }

    /// Add a literal hint (no SSA references to resolve).
    fn add_literal_hint(&mut self, line: u32, text: &str) {
        let template = PseudocodeTemplate::new().literal(text);
        self.hints.push(SsaHint::new(line, template, self.indent_level));
    }

    /// Add a hint from a template.
    fn add_template_hint(&mut self, line: u32, template: PseudocodeTemplate) {
        self.hints.push(SsaHint::new(line, template, self.indent_level));
    }

    /// Generate pseudocode template for an instruction.
    ///
    /// This is the SSA equivalent of `ToPseudocode::to_pseudocode`.
    fn instruction_to_template(
        &mut self,
        inst: &miden_assembly_syntax::ast::Instruction,
        _span: SourceSpan,
    ) -> Option<PseudocodeTemplate> {
        use miden_assembly_syntax::ast::Instruction;
        use super::ssa::{binary_op, binary_imm_op, comparison, comparison_imm, unary_op, unary_fn};

        let state = self.state.as_mut()?;
        if state.tracking_failed {
            return None;
        }

        // Helper to format immediate values
        fn format_imm<T: std::fmt::Display>(imm: &miden_assembly_syntax::ast::Immediate<T>) -> String {
            format!("{}", imm)
        }

        match inst {
            // ─────────────────────────────────────────────────────────────────────
            // Push operations
            // ─────────────────────────────────────────────────────────────────────
            Instruction::Push(imm) => {
                let value = format!("{}", imm);
                let var = state.new_local();
                state.push(var);
                let mut out = TemplateOutput::default();
                out.var(&var);
                out.text(" = ");
                out.text(&value);
                Some(out.into_template())
            }

            // ─────────────────────────────────────────────────────────────────────
            // Stack manipulation - Dup
            // ─────────────────────────────────────────────────────────────────────
            Instruction::Dup0 => self.dup_template(0),
            Instruction::Dup1 => self.dup_template(1),
            Instruction::Dup2 => self.dup_template(2),
            Instruction::Dup3 => self.dup_template(3),
            Instruction::Dup4 => self.dup_template(4),
            Instruction::Dup5 => self.dup_template(5),
            Instruction::Dup6 => self.dup_template(6),
            Instruction::Dup7 => self.dup_template(7),
            Instruction::Dup8 => self.dup_template(8),
            Instruction::Dup9 => self.dup_template(9),
            Instruction::Dup10 => self.dup_template(10),
            Instruction::Dup11 => self.dup_template(11),
            Instruction::Dup12 => self.dup_template(12),
            Instruction::Dup13 => self.dup_template(13),
            Instruction::Dup14 => self.dup_template(14),
            Instruction::Dup15 => self.dup_template(15),

            // ─────────────────────────────────────────────────────────────────────
            // Stack manipulation - Drop
            // ─────────────────────────────────────────────────────────────────────
            Instruction::Drop => {
                state.pop();
                None
            }
            Instruction::DropW => {
                for _ in 0..4 {
                    state.pop();
                }
                None
            }

            // ─────────────────────────────────────────────────────────────────────
            // Stack manipulation - Swap
            // ─────────────────────────────────────────────────────────────────────
            Instruction::Swap1 => { state.swap(0, 1); None }
            Instruction::Swap2 => { state.swap(0, 2); None }
            Instruction::Swap3 => { state.swap(0, 3); None }
            Instruction::Swap4 => { state.swap(0, 4); None }
            Instruction::Swap5 => { state.swap(0, 5); None }
            Instruction::Swap6 => { state.swap(0, 6); None }
            Instruction::Swap7 => { state.swap(0, 7); None }
            Instruction::Swap8 => { state.swap(0, 8); None }
            Instruction::Swap9 => { state.swap(0, 9); None }
            Instruction::Swap10 => { state.swap(0, 10); None }
            Instruction::Swap11 => { state.swap(0, 11); None }
            Instruction::Swap12 => { state.swap(0, 12); None }
            Instruction::Swap13 => { state.swap(0, 13); None }
            Instruction::Swap14 => { state.swap(0, 14); None }
            Instruction::Swap15 => { state.swap(0, 15); None }

            // ─────────────────────────────────────────────────────────────────────
            // Stack manipulation - Move
            // ─────────────────────────────────────────────────────────────────────
            Instruction::MovUp2 => { state.movup(2); None }
            Instruction::MovUp3 => { state.movup(3); None }
            Instruction::MovUp4 => { state.movup(4); None }
            Instruction::MovUp5 => { state.movup(5); None }
            Instruction::MovUp6 => { state.movup(6); None }
            Instruction::MovUp7 => { state.movup(7); None }
            Instruction::MovUp8 => { state.movup(8); None }
            Instruction::MovUp9 => { state.movup(9); None }
            Instruction::MovUp10 => { state.movup(10); None }
            Instruction::MovUp11 => { state.movup(11); None }
            Instruction::MovUp12 => { state.movup(12); None }
            Instruction::MovUp13 => { state.movup(13); None }
            Instruction::MovUp14 => { state.movup(14); None }
            Instruction::MovUp15 => { state.movup(15); None }

            Instruction::MovDn2 => { state.movdn(2); None }
            Instruction::MovDn3 => { state.movdn(3); None }
            Instruction::MovDn4 => { state.movdn(4); None }
            Instruction::MovDn5 => { state.movdn(5); None }
            Instruction::MovDn6 => { state.movdn(6); None }
            Instruction::MovDn7 => { state.movdn(7); None }
            Instruction::MovDn8 => { state.movdn(8); None }
            Instruction::MovDn9 => { state.movdn(9); None }
            Instruction::MovDn10 => { state.movdn(10); None }
            Instruction::MovDn11 => { state.movdn(11); None }
            Instruction::MovDn12 => { state.movdn(12); None }
            Instruction::MovDn13 => { state.movdn(13); None }
            Instruction::MovDn14 => { state.movdn(14); None }
            Instruction::MovDn15 => { state.movdn(15); None }

            // ─────────────────────────────────────────────────────────────────────
            // Arithmetic operations
            // ─────────────────────────────────────────────────────────────────────
            Instruction::Add => Some(binary_op::<SsaDecompilerState, TemplateOutput>(state, "+").into_template()),
            Instruction::Sub => Some(binary_op::<SsaDecompilerState, TemplateOutput>(state, "-").into_template()),
            Instruction::Mul => Some(binary_op::<SsaDecompilerState, TemplateOutput>(state, "*").into_template()),
            Instruction::Div => Some(binary_op::<SsaDecompilerState, TemplateOutput>(state, "/").into_template()),

            Instruction::AddImm(imm) => Some(binary_imm_op::<SsaDecompilerState, TemplateOutput>(state, "+", &format_imm(imm)).into_template()),
            Instruction::SubImm(imm) => Some(binary_imm_op::<SsaDecompilerState, TemplateOutput>(state, "-", &format_imm(imm)).into_template()),
            Instruction::MulImm(imm) => Some(binary_imm_op::<SsaDecompilerState, TemplateOutput>(state, "*", &format_imm(imm)).into_template()),
            Instruction::DivImm(imm) => Some(binary_imm_op::<SsaDecompilerState, TemplateOutput>(state, "/", &format_imm(imm)).into_template()),

            Instruction::Neg => Some(unary_op::<SsaDecompilerState, TemplateOutput>(state, "-").into_template()),
            Instruction::Inv => {
                let a = state.pop();
                let var = state.new_local();
                state.push(var);
                let mut out = TemplateOutput::default();
                out.var(&var);
                out.text(" = 1/");
                out.var(&a);
                Some(out.into_template())
            }
            Instruction::Incr => {
                let a = state.pop();
                let var = state.new_local();
                state.push(var);
                let mut out = TemplateOutput::default();
                out.var(&var);
                out.text(" = ");
                out.var(&a);
                out.text(" + 1");
                Some(out.into_template())
            }

            // ─────────────────────────────────────────────────────────────────────
            // Comparison operations
            // ─────────────────────────────────────────────────────────────────────
            Instruction::Eq => Some(comparison::<SsaDecompilerState, TemplateOutput>(state, "==").into_template()),
            Instruction::Neq => Some(comparison::<SsaDecompilerState, TemplateOutput>(state, "!=").into_template()),
            Instruction::Lt => Some(comparison::<SsaDecompilerState, TemplateOutput>(state, "<").into_template()),
            Instruction::Lte => Some(comparison::<SsaDecompilerState, TemplateOutput>(state, "<=").into_template()),
            Instruction::Gt => Some(comparison::<SsaDecompilerState, TemplateOutput>(state, ">").into_template()),
            Instruction::Gte => Some(comparison::<SsaDecompilerState, TemplateOutput>(state, ">=").into_template()),

            Instruction::EqImm(imm) => Some(comparison_imm::<SsaDecompilerState, TemplateOutput>(state, "==", &format_imm(imm)).into_template()),
            Instruction::NeqImm(imm) => Some(comparison_imm::<SsaDecompilerState, TemplateOutput>(state, "!=", &format_imm(imm)).into_template()),

            // ─────────────────────────────────────────────────────────────────────
            // Boolean operations
            // ─────────────────────────────────────────────────────────────────────
            Instruction::And => Some(binary_op::<SsaDecompilerState, TemplateOutput>(state, "&&").into_template()),
            Instruction::Or => Some(binary_op::<SsaDecompilerState, TemplateOutput>(state, "||").into_template()),
            Instruction::Xor => Some(binary_op::<SsaDecompilerState, TemplateOutput>(state, "^").into_template()),
            Instruction::Not => Some(unary_op::<SsaDecompilerState, TemplateOutput>(state, "!").into_template()),

            // ─────────────────────────────────────────────────────────────────────
            // u32 operations
            // ─────────────────────────────────────────────────────────────────────
            Instruction::U32And => Some(binary_op::<SsaDecompilerState, TemplateOutput>(state, "&").into_template()),
            Instruction::U32Or => Some(binary_op::<SsaDecompilerState, TemplateOutput>(state, "|").into_template()),
            Instruction::U32Xor => Some(binary_op::<SsaDecompilerState, TemplateOutput>(state, "^").into_template()),
            Instruction::U32Not => Some(unary_op::<SsaDecompilerState, TemplateOutput>(state, "~").into_template()),

            Instruction::U32WrappingAdd => Some(binary_op::<SsaDecompilerState, TemplateOutput>(state, "+").into_template()),
            Instruction::U32WrappingSub => Some(binary_op::<SsaDecompilerState, TemplateOutput>(state, "-").into_template()),
            Instruction::U32WrappingMul => Some(binary_op::<SsaDecompilerState, TemplateOutput>(state, "*").into_template()),
            Instruction::U32Div => Some(binary_op::<SsaDecompilerState, TemplateOutput>(state, "/").into_template()),
            Instruction::U32Mod => Some(binary_op::<SsaDecompilerState, TemplateOutput>(state, "%").into_template()),

            Instruction::U32WrappingAddImm(imm) => Some(binary_imm_op::<SsaDecompilerState, TemplateOutput>(state, "+", &format_imm(imm)).into_template()),
            Instruction::U32WrappingSubImm(imm) => Some(binary_imm_op::<SsaDecompilerState, TemplateOutput>(state, "-", &format_imm(imm)).into_template()),
            Instruction::U32WrappingMulImm(imm) => Some(binary_imm_op::<SsaDecompilerState, TemplateOutput>(state, "*", &format_imm(imm)).into_template()),
            Instruction::U32DivImm(imm) => Some(binary_imm_op::<SsaDecompilerState, TemplateOutput>(state, "/", &format_imm(imm)).into_template()),
            Instruction::U32ModImm(imm) => Some(binary_imm_op::<SsaDecompilerState, TemplateOutput>(state, "%", &format_imm(imm)).into_template()),

            Instruction::U32Shl => Some(binary_op::<SsaDecompilerState, TemplateOutput>(state, "<<").into_template()),
            Instruction::U32Shr => Some(binary_op::<SsaDecompilerState, TemplateOutput>(state, ">>").into_template()),
            Instruction::U32ShlImm(imm) => Some(binary_imm_op::<SsaDecompilerState, TemplateOutput>(state, "<<", &format_imm(imm)).into_template()),
            Instruction::U32ShrImm(imm) => Some(binary_imm_op::<SsaDecompilerState, TemplateOutput>(state, ">>", &format_imm(imm)).into_template()),

            Instruction::U32Lt => Some(comparison::<SsaDecompilerState, TemplateOutput>(state, "<").into_template()),
            Instruction::U32Lte => Some(comparison::<SsaDecompilerState, TemplateOutput>(state, "<=").into_template()),
            Instruction::U32Gt => Some(comparison::<SsaDecompilerState, TemplateOutput>(state, ">").into_template()),
            Instruction::U32Gte => Some(comparison::<SsaDecompilerState, TemplateOutput>(state, ">=").into_template()),
            Instruction::U32Min => Some(binary_op::<SsaDecompilerState, TemplateOutput>(state, "min").into_template()),
            Instruction::U32Max => Some(binary_op::<SsaDecompilerState, TemplateOutput>(state, "max").into_template()),

            // u32 special operations
            Instruction::U32Popcnt => Some(unary_fn::<SsaDecompilerState, TemplateOutput>(state, "popcnt").into_template()),
            Instruction::U32Clz => Some(unary_fn::<SsaDecompilerState, TemplateOutput>(state, "clz").into_template()),
            Instruction::U32Ctz => Some(unary_fn::<SsaDecompilerState, TemplateOutput>(state, "ctz").into_template()),
            Instruction::U32Clo => Some(unary_fn::<SsaDecompilerState, TemplateOutput>(state, "clo").into_template()),
            Instruction::U32Cto => Some(unary_fn::<SsaDecompilerState, TemplateOutput>(state, "cto").into_template()),

            Instruction::U32Cast => Some(unary_fn::<SsaDecompilerState, TemplateOutput>(state, "u32").into_template()),

            // u32 assertions - no output
            Instruction::U32Assert | Instruction::U32AssertWithError(_) |
            Instruction::U32Assert2 | Instruction::U32Assert2WithError(_) |
            Instruction::U32AssertW | Instruction::U32AssertWWithError(_) => None,

            // ─────────────────────────────────────────────────────────────────────
            // Memory operations
            // ─────────────────────────────────────────────────────────────────────
            Instruction::MemLoad => {
                let addr = state.pop();
                let var = state.new_local();
                state.push(var);
                let mut out = TemplateOutput::default();
                out.var(&var);
                out.text(" = mem[");
                out.var(&addr);
                out.text("]");
                Some(out.into_template())
            }
            Instruction::MemLoadImm(imm) => {
                let addr = format_imm(imm);
                let var = state.new_local();
                state.push(var);
                let mut out = TemplateOutput::default();
                out.var(&var);
                out.text(&format!(" = mem[{}]", addr));
                Some(out.into_template())
            }
            Instruction::MemStore => {
                let addr = state.pop();
                let val = state.pop();
                let mut out = TemplateOutput::default();
                out.text("mem[");
                out.var(&addr);
                out.text("] = ");
                out.var(&val);
                Some(out.into_template())
            }
            Instruction::MemStoreImm(imm) => {
                let addr = format_imm(imm);
                let val = state.pop();
                let mut out = TemplateOutput::default();
                out.text(&format!("mem[{}] = ", addr));
                out.var(&val);
                Some(out.into_template())
            }

            // ─────────────────────────────────────────────────────────────────────
            // Assertions
            // ─────────────────────────────────────────────────────────────────────
            Instruction::Assert | Instruction::AssertWithError(_) => {
                let cond = state.pop();
                let mut out = TemplateOutput::default();
                out.text("assert(");
                out.var(&cond);
                out.text(")");
                Some(out.into_template())
            }
            Instruction::AssertEq | Instruction::AssertEqWithError(_) => {
                let b = state.pop();
                let a = state.pop();
                let mut out = TemplateOutput::default();
                out.text("assert(");
                out.var(&a);
                out.text(" == ");
                out.var(&b);
                out.text(")");
                Some(out.into_template())
            }
            Instruction::Assertz | Instruction::AssertzWithError(_) => {
                let val = state.pop();
                let mut out = TemplateOutput::default();
                out.text("assert(");
                out.var(&val);
                out.text(" == 0)");
                Some(out.into_template())
            }

            // ─────────────────────────────────────────────────────────────────────
            // Other operations
            // ─────────────────────────────────────────────────────────────────────
            Instruction::ILog2 => Some(unary_fn::<SsaDecompilerState, TemplateOutput>(state, "ilog2").into_template()),
            Instruction::Pow2 => Some(unary_fn::<SsaDecompilerState, TemplateOutput>(state, "pow2").into_template()),
            Instruction::Exp => Some(binary_op::<SsaDecompilerState, TemplateOutput>(state, "**").into_template()),
            Instruction::ExpImm(imm) => Some(binary_imm_op::<SsaDecompilerState, TemplateOutput>(state, "**", &format_imm(imm)).into_template()),
            Instruction::IsOdd => Some(unary_fn::<SsaDecompilerState, TemplateOutput>(state, "is_odd").into_template()),

            // No-ops
            Instruction::Nop | Instruction::Breakpoint | Instruction::Debug(_) |
            Instruction::Emit | Instruction::EmitImm(_) | Instruction::Trace(_) |
            Instruction::SysEvent(_) => None,

            // For unsupported instructions, fall back to the string-based approach
            // and convert to a literal template
            _ => {
                // Create a temporary DecompilerState to use the existing ToPseudocode
                // This is a fallback for instructions not yet ported to SSA
                None
            }
        }
    }

    /// Generate dup template.
    fn dup_template(&mut self, n: usize) -> Option<PseudocodeTemplate> {
        let state = self.state.as_mut()?;
        let src = state.peek(n)?;
        state.dup(n);
        let var = state.new_local();
        // Replace the dup'd value with new variable
        state.pop();
        state.push(var);

        let mut out = TemplateOutput::default();
        out.var(&var);
        out.text(" = ");
        out.var(&src);
        Some(out.into_template())
    }

    /// Visit all operations in a block, handling control flow.
    fn visit_block_ssa(&mut self, block: &Block) {
        for op in block.iter() {
            self.visit_op_ssa(op);
        }
    }

    /// Visit a single operation in SSA mode.
    fn visit_op_ssa(&mut self, op: &Op) {
        match op {
            Op::Inst(inst) => {
                if let Some(template) = self.instruction_to_template(inst.inner(), inst.span()) {
                    if let Some(range) = span_to_range(self.sources, inst.span()) {
                        self.add_template_hint(range.start.line, template);
                    }
                }
            }
            Op::If { then_blk, else_blk, .. } => {
                // Get condition variable SSA ID
                let condition_id = self.state.as_ref().and_then(|s| s.peek(0));

                // Pop the condition and save state for else branch
                let entry_stack = if let Some(ref mut state) = self.state {
                    state.pop();
                    Some(state.save_stack())
                } else {
                    None
                };

                // Generate "if" hint with condition
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

                // Visit then block
                self.indent_level += 1;
                self.visit_block_ssa(then_blk);
                self.indent_level -= 1;

                // Save then-exit state
                let then_exit_stack = self.state.as_ref().map(|s| s.save_stack());

                // Handle else block
                if !else_blk.is_empty() {
                    if let Some(range) = span_to_range(self.sources, else_blk.span()) {
                        let else_line = range.start.line.saturating_sub(1);
                        self.add_literal_hint(else_line, "else");
                    }

                    // Restore entry state for else branch
                    if let (Some(ref mut state), Some(ref saved)) = (&mut self.state, &entry_stack) {
                        state.restore_stack(saved);
                    }

                    self.indent_level += 1;
                    self.visit_block_ssa(else_blk);
                    self.indent_level -= 1;

                    // Create phi nodes at merge point
                    if let (Some(ref mut state), Some(ref then_stack)) = (&mut self.state, &then_exit_stack) {
                        let else_stack = state.save_stack();
                        state.create_if_else_phis(then_stack, &else_stack);
                    }
                }

                // Generate "end" hint
                if let Some(range) = span_to_range(self.sources, op.span()) {
                    self.add_literal_hint(range.end.line, "end");
                }
            }
            Op::While { body, .. } => {
                // Get condition variable SSA ID
                let condition_id = self.state.as_ref().and_then(|s| s.peek(0));

                // Save loop entry state (with condition on top)
                let loop_entry_stack = self.state.as_ref().map(|s| s.save_stack());

                // Pop condition and record entry depth
                if let Some(ref mut state) = self.state {
                    state.pop();
                }

                // Generate "while" hint
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

                // Visit body
                self.indent_level += 1;
                self.visit_block_ssa(body);
                self.indent_level -= 1;

                // Create phi nodes for loop (at loop exit, stack should match entry with new condition)
                if let (Some(ref mut state), Some(ref entry_stack)) = (&mut self.state, &loop_entry_stack) {
                    state.create_loop_phis(entry_stack);
                }

                // Generate "end" hint
                if let Some(range) = span_to_range(self.sources, op.span()) {
                    self.add_literal_hint(range.end.line, "end");
                }
            }
            Op::Repeat { count, body, .. } => {
                // Generate a loop counter
                let counter = self.state.as_mut().map(|s| s.new_counter()).unwrap_or_else(|| "i".to_string());

                // Save loop entry state
                let loop_entry_stack = self.state.as_ref().map(|s| s.save_stack());

                // Generate "for" hint
                if let Some(range) = span_to_range(self.sources, op.span()) {
                    let hint = format!("for {} = 0; {} < {};:", counter, counter, count);
                    self.add_literal_hint(range.start.line, &hint);
                }

                // Visit body
                self.indent_level += 1;
                self.visit_block_ssa(body);
                self.indent_level -= 1;

                // Create phi nodes for loop
                if let (Some(ref mut state), Some(ref entry_stack)) = (&mut self.state, &loop_entry_stack) {
                    state.create_loop_phis(entry_stack);
                }

                // Generate "end" hint
                if let Some(range) = span_to_range(self.sources, op.span()) {
                    self.add_literal_hint(range.end.line, "end");
                }
            }
        }
    }

    /// Resolve all hints after visiting the AST.
    ///
    /// This is Pass 2: resolve phi relationships and convert templates to strings.
    fn resolve_all_hints(&mut self) {
        if let Some(ref mut state) = self.state {
            // Resolve phi relationships using union-find
            state.ctx.resolve_names();

            // Convert all templates to strings
            for hint in &self.hints {
                let resolved = hint.resolve(&state.ctx);
                self.resolved_hints.push((hint.line, resolved));
            }
        }

        // Clear templates after resolution
        self.hints.clear();
    }
}

impl<'a> Visit for SsaDecompilationCollector<'a> {
    fn visit_procedure(&mut self, proc: &Procedure) -> core::ops::ControlFlow<()> {
        let proc_name = proc.name().as_str().to_string();

        let decl_range = span_to_range(self.sources, proc.name().span());
        let decl_line = decl_range.as_ref().map(|r| r.start.line);

        // Get contract info
        let contract = self.contracts.and_then(|c| c.get_by_name(&proc_name));
        let contract_effect = contract.map(|c| c.stack_effect.clone());
        let contract_signature = contract.and_then(|c| c.signature.clone());

        // Pre-analyze procedure
        let pre_analysis = pre_analyze_procedure(proc.body());
        let pre_analyzed_inputs = pre_analysis.total_inputs_required;

        // Determine input/output counts
        let (initial_input_count, output_count) = match &contract_effect {
            Some(StackEffect::Known { inputs, outputs }) => {
                ((*inputs).max(pre_analyzed_inputs), Some(*outputs))
            }
            Some(StackEffect::KnownInputs { inputs }) => {
                ((*inputs).max(pre_analyzed_inputs), None)
            }
            _ => (pre_analyzed_inputs, None),
        };

        // Track procedure state
        self.proc_hint_start = self.hints.len();
        self.proc_outputs = output_count;
        self.proc_decl_line = decl_line;

        // Generate signature hint (as literal - no SSA refs needed)
        if let Some(line) = decl_line {
            let decl_prefix = extract_declaration_prefix(self.source_text, line);
            let signature = format_procedure_signature(&decl_prefix, &proc_name, initial_input_count, output_count, contract_signature.as_ref());
            self.add_literal_hint(line, &signature);
        }

        // Initialize SSA state
        self.state = Some(SsaDecompilerState::new(initial_input_count));

        // Visit procedure body (Pass 1)
        self.indent_level = 1;
        self.visit_block_ssa(proc.body());
        self.indent_level = 0;

        // Check for tracking failure
        if let Some(ref state) = self.state {
            if state.tracking_failed {
                // Remove hints and record failure
                self.hints.truncate(self.proc_hint_start);
                self.state = None;
                self.proc_outputs = None;
                self.proc_decl_line = None;
                return core::ops::ControlFlow::Continue(());
            }
        }

        // Resolve hints (Pass 2)
        self.resolve_all_hints();

        // Add "end" hint
        if let Some(body_range) = span_to_range(self.sources, proc.body().span()) {
            let start_line = body_range.end.line as usize;
            for (offset, line_text) in self.source_text.lines().skip(start_line).enumerate() {
                if line_text.trim() == "end" {
                    let end_line = (start_line + offset) as u32;
                    self.resolved_hints.push((end_line, "end".to_string()));
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
///
/// This function uses SSA (Static Single Assignment) form with phi nodes to track
/// variable identity across control flow, producing consistent variable names in loops.
pub fn collect_decompilation_hints(
    module: &Module,
    sources: &DefaultSourceManager,
    uri: &Url,
    visible_range: &Range,
    tab_count: usize,
    source_text: &str,
    contracts: Option<&ContractStore>,
) -> DecompilationResult {
    let mut collector = SsaDecompilationCollector::new(module, sources, contracts, source_text);
    let _ = visit::visit_module(&mut collector, module);

    // Fixed padding (in columns) between instruction end and hint.
    let padding = (tab_count.max(1)) as u32;

    // Pre-compute line lengths for positioning hints
    let line_lengths: Vec<u32> = source_text
        .lines()
        .map(|line| line.trim_end().len() as u32)
        .collect();

    // Group hints by line
    let mut line_hints: HashMap<u32, Vec<String>> = HashMap::new();
    for (line, hint) in collector.resolved_hints {
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

#[cfg(test)]
mod tests {
    use super::*;

    /// Test the SsaHint structure directly.
    #[test]
    fn test_ssa_hint_resolve() {
        let mut ctx = SsaContext::new();
        let a0 = ctx.new_argument(0);
        let v0 = ctx.new_local();

        // Create a template: "v_0 = a_0 + 1"
        let template = PseudocodeTemplate::new()
            .ssa_ref(v0)
            .literal(" = ")
            .ssa_ref(a0)
            .literal(" + 1");

        let hint = SsaHint::new(5, template, 1);
        let resolved = hint.resolve(&ctx);

        assert_eq!(resolved, "    v_0 = a_0 + 1");
    }

    /// Test the SsaHint with phi resolution.
    #[test]
    fn test_ssa_hint_with_phi() {
        let mut ctx = SsaContext::new();
        let a0 = ctx.new_argument(0);
        let v0 = ctx.new_local();

        // Link v0 to a0 via phi (they're the same variable in a loop)
        ctx.add_phi(v0, vec![a0]);
        ctx.resolve_names();

        // Create a template: "v_0 = a_0 + 1"
        let template = PseudocodeTemplate::new()
            .ssa_ref(v0)
            .literal(" = ")
            .ssa_ref(a0)
            .literal(" + 1");

        let hint = SsaHint::new(5, template, 0);
        let resolved = hint.resolve(&ctx);

        // After phi resolution, v_0 should display as a_0
        assert_eq!(resolved, "a_0 = a_0 + 1");
    }

    /// Test instruction_to_template for basic arithmetic.
    #[test]
    fn test_instruction_to_template_add() {
        // Create a mock collector with some initial state
        let mut state = SsaDecompilerState::new(2);

        // Simulate add instruction: pop 2, push 1
        let template = {
            use super::super::ssa::{binary_op, TemplateOutput};
            binary_op::<SsaDecompilerState, TemplateOutput>(&mut state, "+").into_template()
        };

        // Resolve
        let resolved = template.resolve(&state.ctx);

        // Should produce "v_0 = a_1 + a_0" (operand order matches stack semantics)
        assert_eq!(resolved, "v_0 = a_1 + a_0");
        assert_eq!(state.depth(), 1); // One result on stack
    }

    /// Test that loop phi resolution unifies variables.
    #[test]
    fn test_loop_phi_unification() {
        let mut state = SsaDecompilerState::new(1);

        // Initial: [a_0] on stack
        let entry_stack = state.save_stack();

        // Loop body: pop a_0, push v_0 (new value)
        state.pop();
        let v0 = state.new_local();
        state.push(v0);

        // Create loop phis
        state.create_loop_phis(&entry_stack);
        state.ctx.resolve_names();

        // v_0 should now display as a_0 (unified via phi)
        assert_eq!(state.ctx.get_display_name(v0), "a_0");
    }

    /// Test memcopy_elements procedure to verify while loop condition unification.
    ///
    /// This test simulates the exact instruction sequence from examples/stdlib/mem.masm:
    /// ```masm
    /// pub proc memcopy_elements
    ///     neg                 # v_0 = -a_0
    ///     dup neq.0           # v_1 = (v_0 != 0) - loop condition
    ///     while.true
    ///         dup.1 mem_load  # v_2 = mem[a_1]
    ///         dup.3 mem_store # mem[a_2] = v_2
    ///         add.1 movup.2 add.1 movup.2 add.1 movup.2  # update pointers
    ///         dup neq.0       # new condition - should unify with v_1
    ///     end
    ///     drop drop drop
    /// end
    /// ```
    ///
    /// The key test: the condition variable at loop entry (`v_1`) and at loop exit
    /// (new condition after body) should resolve to the SAME display name via phi nodes.
    #[test]
    fn test_memcopy_elements_while_loop_condition() {
        use super::super::ssa::{binary_imm_op, comparison_imm, unary_op, TemplateOutput};

        // Initialize with 3 inputs: [n, read_ptr, write_ptr] with n (a_0) on top
        let mut state = SsaDecompilerState::new(3);

        // neg: v_0 = -a_0
        let neg_template: PseudocodeTemplate = unary_op::<SsaDecompilerState, TemplateOutput>(&mut state, "-").into_template();

        // dup: duplicate v_0 to top
        state.dup(0);

        // neq.0: v_1 = (dup_v_0 != 0) - this is the INITIAL loop condition
        let initial_cond_template: PseudocodeTemplate = comparison_imm::<SsaDecompilerState, TemplateOutput>(&mut state, "!=", "0").into_template();
        let initial_condition = state.peek(0).unwrap();

        // Save loop entry state (with condition on top)
        // Stack: [v_1 (condition), v_0 (counter), a_1 (read_ptr), a_2 (write_ptr)]
        let loop_entry_stack = state.save_stack();

        // Pop condition (while.true consumes it)
        state.pop();

        // === Loop body ===

        // dup.1 mem_load: v_2 = mem[a_1]
        // Stack before: [v_0, a_1, a_2]
        let _read_ptr = state.peek(1).unwrap();
        let loaded = state.new_local(); // v_2
        state.push(loaded);
        // Stack: [v_2, v_0, a_1, a_2]

        // dup.3 mem_store: mem[a_2] = v_2
        // This pops the value and address
        let _write_ptr = state.peek(3).unwrap();
        let _stored_val = state.pop(); // v_2
        // Stack: [v_0, a_1, a_2]

        // add.1: v_3 = v_0 + 1 (update counter)
        let _counter_update: PseudocodeTemplate = binary_imm_op::<SsaDecompilerState, TemplateOutput>(&mut state, "+", "1").into_template();
        // Stack: [v_3, a_1, a_2]

        // movup.2: bring a_2 to top
        state.movup(2);
        // Stack: [a_2, v_3, a_1]

        // add.1: v_4 = a_2 + 1 (update write_ptr)
        let _write_ptr_update: PseudocodeTemplate = binary_imm_op::<SsaDecompilerState, TemplateOutput>(&mut state, "+", "1").into_template();
        // Stack: [v_4, v_3, a_1]

        // movup.2: bring a_1 to top
        state.movup(2);
        // Stack: [a_1, v_4, v_3]

        // add.1: v_5 = a_1 + 1 (update read_ptr)
        let _read_ptr_update: PseudocodeTemplate = binary_imm_op::<SsaDecompilerState, TemplateOutput>(&mut state, "+", "1").into_template();
        // Stack: [v_5, v_4, v_3]

        // movup.2: bring v_3 (counter) to top
        state.movup(2);
        // Stack: [v_3, v_5, v_4]

        // dup: duplicate counter for condition check
        state.dup(0);

        // neq.0: v_6 = (v_3 != 0) - this is the END-OF-LOOP condition
        let end_cond_template: PseudocodeTemplate = comparison_imm::<SsaDecompilerState, TemplateOutput>(&mut state, "!=", "0").into_template();
        let end_condition = state.peek(0).unwrap();
        // Stack: [v_6 (new condition), v_3 (counter), v_5 (read_ptr), v_4 (write_ptr)]

        // Before phi resolution, the conditions have different SSA IDs
        assert_ne!(initial_condition, end_condition, "Before phi, conditions should have different SSA IDs");

        // Create phi nodes at loop end
        state.create_loop_phis(&loop_entry_stack);

        // Resolve names
        state.ctx.resolve_names();

        // After phi resolution, both conditions should display with the SAME name!
        let initial_name = state.ctx.get_display_name(initial_condition);
        let end_name = state.ctx.get_display_name(end_condition);

        assert_eq!(
            initial_name, end_name,
            "Loop condition should have consistent name: initial='{}', end='{}'",
            initial_name, end_name
        );

        // Verify the condition uses a reasonable name (should be v_1 since it's the second local after v_0)
        assert!(
            initial_name.starts_with("v_"),
            "Condition should be a local variable: got '{}'",
            initial_name
        );

        // Also verify the counter is unified
        // Note: save_stack() returns Vec where index 0 is BOTTOM of stack
        // So we need len - 2 to get position 1 from top (the counter)
        let counter_at_entry = loop_entry_stack[loop_entry_stack.len() - 2]; // v_0 position
        let counter_at_end = state.peek(1).unwrap(); // current counter position
        let entry_counter_name = state.ctx.get_display_name(counter_at_entry);
        let end_counter_name = state.ctx.get_display_name(counter_at_end);

        assert_eq!(
            entry_counter_name, end_counter_name,
            "Counter should have consistent name: entry='{}', end='{}'",
            entry_counter_name, end_counter_name
        );

        // Print the resolved templates for debugging
        println!("neg template: {}", neg_template.resolve(&state.ctx));
        println!("initial condition template: {}", initial_cond_template.resolve(&state.ctx));
        println!("end condition template: {}", end_cond_template.resolve(&state.ctx));
    }
}
