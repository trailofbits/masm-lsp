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
    abstract_interpretation::{analyze_repeat_loop, analyze_while_loop, LoopTerm},
    pre_analyze_procedure, ContractStore, StackEffect, SymbolicExpr,
};
use crate::diagnostics::{span_to_range, SOURCE_DECOMPILATION};
use crate::symbol_resolution::SymbolResolver;

use super::ssa::{
    binary_imm_op,
    // Helper functions for pseudocode generation
    binary_op,
    comparison,
    comparison_imm,
    ext2_binary_op,
    ext2_unary_fn,
    ext2_unary_op,
    unary_fn,
    unary_op,
    DecompilerState,
    PseudocodeTemplate,
    SsaContext,
};
use crate::decompiler::ssa::{
    extract_declaration_prefix, format_procedure_signature, PseudocodeBuilder,
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
// Parametric Types for Resolution-Time Computation
// ═══════════════════════════════════════════════════════════════════════════

use super::ssa::SsaId;

/// A term in a parametric expression representing `stride * counter`.
///
/// Used at hint resolution time to compute parametric variable names
/// based on the loop context at the time the hint was created.
#[derive(Debug, Clone)]
struct ParametricTerm {
    /// Coefficient of the loop counter (negative for decrementing access)
    stride: i32,
    /// Index into the counter names list (0 = outermost loop)
    counter_idx: usize,
}

impl ParametricTerm {
    fn new(stride: i32, counter_idx: usize) -> Self {
        Self {
            stride,
            counter_idx,
        }
    }
}

/// A parametric expression of the form `base + stride_0 * counter_0 + stride_1 * counter_1 + ...`
///
/// Computed at hint creation time based on the active loops and their stack effects.
/// Used at resolution time to generate display names like `a_(3-i)`.
#[derive(Debug, Clone)]
struct ParametricExpr {
    /// Variable prefix ("a" for arguments, "v" for locals)
    prefix: char,
    /// Base index (constant part)
    base: i32,
    /// Loop terms - each represents dependency on one loop counter
    terms: Vec<ParametricTerm>,
}

impl ParametricExpr {
    /// Create a new parametric expression for an argument.
    fn argument(base: i32, terms: Vec<ParametricTerm>) -> Self {
        Self {
            prefix: 'a',
            base,
            terms,
        }
    }

    /// Create a parametric expression for a loop-produced output (r-indexed).
    fn output(base: i32, counter_idx: usize) -> Self {
        Self {
            prefix: 'r',
            base,
            terms: vec![ParametricTerm::new(1, counter_idx)],
        }
    }

    /// Create a parametric expression for a loop-produced local (v-indexed).
    fn local(base: i32, counter_idx: usize) -> Self {
        Self {
            prefix: 'v',
            base,
            terms: vec![ParametricTerm::new(1, counter_idx)],
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Instruction Handlers (defined here, called from PseudocodeCollector)
// ═══════════════════════════════════════════════════════════════════════════

impl<'a> PseudocodeCollector<'a> {
    fn handle_memory_ops(
        &mut self,
        inst: &miden_assembly_syntax::ast::Instruction,
    ) -> Option<PseudocodeTemplate> {
        use miden_assembly_syntax::ast::Instruction;
        let state = self.state.as_mut()?;

        match inst {
            Instruction::MemLoad => {
                let addr = state.pop();
                let var = state.new_local();
                state.push(var);
                let mut out = PseudocodeBuilder::new();
                out.var(var);
                out.text(" = mem[");
                out.var(addr);
                out.text("]");
                Some(out.build())
            }
            Instruction::MemLoadImm(imm) => {
                let addr = Self::format_imm(imm);
                let var = state.new_local();
                state.push(var);
                let mut out = PseudocodeBuilder::new();
                out.var(var);
                out.text(&format!(" = mem[{}]", addr));
                Some(out.build())
            }
            Instruction::MemStore => {
                let addr = state.pop();
                let val = state.pop();
                let mut out = PseudocodeBuilder::new();
                out.text("mem[");
                out.var(addr);
                out.text("] = ");
                out.var(val);
                Some(out.build())
            }
            Instruction::MemStoreImm(imm) => {
                let addr = Self::format_imm(imm);
                let val = state.pop();
                let mut out = PseudocodeBuilder::new();
                out.text(&format!("mem[{}] = ", addr));
                out.var(val);
                Some(out.build())
            }
            Instruction::MemLoadWBe | Instruction::MemLoadWLe => {
                let addr = state.pop();
                let mut vars = Vec::new();
                for _ in 0..4 {
                    let var = state.new_local();
                    state.push(var);
                    vars.push(var);
                }
                vars.reverse();
                let mut out = PseudocodeBuilder::new();
                out.text("(");
                for (i, var) in vars.iter().enumerate() {
                    if i > 0 {
                        out.text(", ");
                    }
                    out.var(*var);
                }
                out.text(") = mem_w[");
                out.var(addr);
                out.text("]");
                Some(out.build())
            }
            Instruction::MemLoadWBeImm(imm) | Instruction::MemLoadWLeImm(imm) => {
                let addr = Self::format_imm(imm);
                let mut vars = Vec::new();
                for _ in 0..4 {
                    let var = state.new_local();
                    state.push(var);
                    vars.push(var);
                }
                vars.reverse();
                let mut out = PseudocodeBuilder::new();
                out.text("(");
                for (i, var) in vars.iter().enumerate() {
                    if i > 0 {
                        out.text(", ");
                    }
                    out.var(*var);
                }
                out.text(&format!(") = mem_w[{}]", addr));
                Some(out.build())
            }
            Instruction::MemStoreWBe | Instruction::MemStoreWLe => {
                let addr = state.pop();
                let mut vals = Vec::new();
                for _ in 0..4 {
                    vals.push(state.pop());
                }
                vals.reverse();
                let mut out = PseudocodeBuilder::new();
                out.text("mem_w[");
                out.var(addr);
                out.text("] = (");
                for (i, val) in vals.iter().enumerate() {
                    if i > 0 {
                        out.text(", ");
                    }
                    out.var(*val);
                }
                out.text(")");
                Some(out.build())
            }
            Instruction::MemStoreWBeImm(imm) | Instruction::MemStoreWLeImm(imm) => {
                let addr = Self::format_imm(imm);
                let mut vals = Vec::new();
                for _ in 0..4 {
                    vals.push(state.pop());
                }
                vals.reverse();
                let mut out = PseudocodeBuilder::new();
                out.text(&format!("mem_w[{}] = (", addr));
                for (i, val) in vals.iter().enumerate() {
                    if i > 0 {
                        out.text(", ");
                    }
                    out.var(*val);
                }
                out.text(")");
                Some(out.build())
            }
            _ => None,
        }
    }

    fn handle_local_memory_ops(
        &mut self,
        inst: &miden_assembly_syntax::ast::Instruction,
    ) -> Option<PseudocodeTemplate> {
        use miden_assembly_syntax::ast::Instruction;
        let state = self.state.as_mut()?;

        match inst {
            Instruction::LocLoad(idx) => {
                let var = state.new_local();
                state.push(var);
                let mut out = PseudocodeBuilder::new();
                out.var(var);
                out.text(&format!(" = local[{}]", idx));
                Some(out.build())
            }
            Instruction::LocStore(idx) => {
                let val = state.pop();
                let mut out = PseudocodeBuilder::new();
                out.text(&format!("local[{}] = ", idx));
                out.var(val);
                Some(out.build())
            }
            Instruction::LocLoadWBe(idx) | Instruction::LocLoadWLe(idx) => {
                let mut vars = Vec::new();
                for _ in 0..4 {
                    let var = state.new_local();
                    state.push(var);
                    vars.push(var);
                }
                vars.reverse();
                let mut out = PseudocodeBuilder::new();
                out.text("(");
                for (i, var) in vars.iter().enumerate() {
                    if i > 0 {
                        out.text(", ");
                    }
                    out.var(*var);
                }
                out.text(&format!(") = local_w[{}]", idx));
                Some(out.build())
            }
            Instruction::LocStoreWBe(idx) | Instruction::LocStoreWLe(idx) => {
                let mut vals = Vec::new();
                for _ in 0..4 {
                    vals.push(state.pop());
                }
                vals.reverse();
                let mut out = PseudocodeBuilder::new();
                out.text(&format!("local_w[{}] = (", idx));
                for (i, val) in vals.iter().enumerate() {
                    if i > 0 {
                        out.text(", ");
                    }
                    out.var(*val);
                }
                out.text(")");
                Some(out.build())
            }
            Instruction::Locaddr(idx) => {
                let var = state.new_local();
                state.push(var);
                let mut out = PseudocodeBuilder::new();
                out.var(var);
                out.text(&format!(" = &local[{}]", idx));
                Some(out.build())
            }
            _ => None,
        }
    }

    fn handle_advice_ops(
        &mut self,
        inst: &miden_assembly_syntax::ast::Instruction,
    ) -> Option<PseudocodeTemplate> {
        use miden_assembly_syntax::ast::Instruction;
        let state = self.state.as_mut()?;

        match inst {
            Instruction::AdvPush(n) => {
                let count = match n {
                    miden_assembly_syntax::ast::Immediate::Value(v) => v.into_inner() as usize,
                    _ => 1,
                };
                let mut vars = Vec::new();
                for _ in 0..count {
                    let var = state.new_local();
                    state.push(var);
                    vars.push(var);
                }
                vars.reverse();
                let mut out = PseudocodeBuilder::new();
                out.text("(");
                for (i, var) in vars.iter().enumerate() {
                    if i > 0 {
                        out.text(", ");
                    }
                    out.var(*var);
                }
                out.text(") = advice()");
                Some(out.build())
            }
            Instruction::AdvLoadW => {
                for _ in 0..4 {
                    state.pop();
                }
                let mut vars = Vec::new();
                for _ in 0..4 {
                    let var = state.new_local();
                    state.push(var);
                    vars.push(var);
                }
                vars.reverse();
                let mut out = PseudocodeBuilder::new();
                out.text("(");
                for (i, var) in vars.iter().enumerate() {
                    if i > 0 {
                        out.text(", ");
                    }
                    out.var(*var);
                }
                out.text(") = advice_w()");
                Some(out.build())
            }
            Instruction::AdvPipe => {
                for _ in 0..8 {
                    state.pop();
                }
                let mut vars = Vec::new();
                for _ in 0..8 {
                    let var = state.new_local();
                    state.push(var);
                    vars.push(var);
                }
                vars.reverse();
                let mut out = PseudocodeBuilder::new();
                out.text("(");
                for (i, var) in vars.iter().enumerate() {
                    if i > 0 {
                        out.text(", ");
                    }
                    out.var(*var);
                }
                out.text(") = advice_pipe()");
                Some(out.build())
            }
            _ => None,
        }
    }

    fn handle_crypto_ops(
        &mut self,
        inst: &miden_assembly_syntax::ast::Instruction,
    ) -> Option<PseudocodeTemplate> {
        use miden_assembly_syntax::ast::Instruction;
        let state = self.state.as_mut()?;

        match inst {
            Instruction::Hash => {
                let mut args = Vec::new();
                for _ in 0..4 {
                    args.push(state.pop());
                }
                args.reverse();
                let mut vars = Vec::new();
                for _ in 0..4 {
                    let var = state.new_local();
                    state.push(var);
                    vars.push(var);
                }
                vars.reverse();
                let mut out = PseudocodeBuilder::new();
                out.text("(");
                for (i, var) in vars.iter().enumerate() {
                    if i > 0 {
                        out.text(", ");
                    }
                    out.var(*var);
                }
                out.text(") = hash((");
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        out.text(", ");
                    }
                    out.var(*arg);
                }
                out.text("))");
                Some(out.build())
            }
            Instruction::HMerge => {
                for _ in 0..8 {
                    state.pop();
                }
                let mut vars = Vec::new();
                for _ in 0..4 {
                    let var = state.new_local();
                    state.push(var);
                    vars.push(var);
                }
                vars.reverse();
                let mut out = PseudocodeBuilder::new();
                out.text("(");
                for (i, var) in vars.iter().enumerate() {
                    if i > 0 {
                        out.text(", ");
                    }
                    out.var(*var);
                }
                out.text(") = hmerge(...)");
                Some(out.build())
            }
            Instruction::HPerm => {
                let mut inputs = Vec::new();
                for i in 0..12 {
                    if let Some(id) = state.peek(i) {
                        inputs.push(id);
                    }
                }
                inputs.reverse();

                for _ in 0..12 {
                    state.pop();
                }

                let mut outputs = Vec::new();
                for _ in 0..12 {
                    let var = state.new_local();
                    state.push(var);
                    outputs.push(var);
                }
                outputs.reverse();

                let mut out = PseudocodeBuilder::new();
                out.text("(");
                for (i, var) in outputs.iter().enumerate() {
                    if i > 0 {
                        out.text(", ");
                    }
                    out.var(*var);
                }
                out.text(") = hperm(");
                for (i, var) in inputs.iter().enumerate() {
                    if i > 0 {
                        out.text(", ");
                    }
                    out.var(*var);
                }
                out.text(")");
                Some(out.build())
            }
            _ => None,
        }
    }

    fn handle_merkle_ops(
        &mut self,
        inst: &miden_assembly_syntax::ast::Instruction,
    ) -> Option<PseudocodeTemplate> {
        use miden_assembly_syntax::ast::Instruction;
        let state = self.state.as_mut()?;

        match inst {
            Instruction::MTreeGet => {
                state.pop();
                state.pop();
                for _ in 0..4 {
                    state.pop();
                }
                let mut vars = Vec::new();
                for _ in 0..4 {
                    let var = state.new_local();
                    state.push(var);
                    vars.push(var);
                }
                vars.reverse();
                let mut out = PseudocodeBuilder::new();
                out.text("(");
                for (i, var) in vars.iter().enumerate() {
                    if i > 0 {
                        out.text(", ");
                    }
                    out.var(*var);
                }
                out.text(") = mtree_get()");
                Some(out.build())
            }
            Instruction::MTreeSet => {
                state.pop();
                state.pop();
                for _ in 0..4 {
                    state.pop();
                }
                for _ in 0..4 {
                    state.pop();
                }
                let mut vars = Vec::new();
                for _ in 0..4 {
                    let var = state.new_local();
                    state.push(var);
                    vars.push(var);
                }
                vars.reverse();
                let mut out = PseudocodeBuilder::new();
                out.text("(");
                for (i, var) in vars.iter().enumerate() {
                    if i > 0 {
                        out.text(", ");
                    }
                    out.var(*var);
                }
                out.text(") = mtree_set()");
                Some(out.build())
            }
            Instruction::MTreeMerge => {
                for _ in 0..8 {
                    state.pop();
                }
                let mut vars = Vec::new();
                for _ in 0..4 {
                    let var = state.new_local();
                    state.push(var);
                    vars.push(var);
                }
                vars.reverse();
                let mut out = PseudocodeBuilder::new();
                out.text("(");
                for (i, var) in vars.iter().enumerate() {
                    if i > 0 {
                        out.text(", ");
                    }
                    out.var(*var);
                }
                out.text(") = mtree_merge()");
                Some(out.build())
            }
            Instruction::MTreeVerify | Instruction::MTreeVerifyWithError(_) => {
                Some(PseudocodeTemplate::new().literal("mtree_verify()"))
            }
            _ => None,
        }
    }

    fn handle_word_stack_ops(
        &mut self,
        inst: &miden_assembly_syntax::ast::Instruction,
        _span: SourceSpan,
    ) -> Option<PseudocodeTemplate> {
        use miden_assembly_syntax::ast::Instruction;
        let state = self.state.as_mut()?;

        match inst {
            Instruction::SwapW1 => {
                self.swapw(0, 1);
                None
            }
            Instruction::SwapW2 => {
                self.swapw(0, 2);
                None
            }
            Instruction::SwapW3 => {
                self.swapw(0, 3);
                None
            }
            Instruction::SwapDw => {
                for i in 0..8 {
                    state.swap(i, 8 + i);
                }
                None
            }
            Instruction::MovUpW2 => {
                self.movupw(2);
                None
            }
            Instruction::MovUpW3 => {
                self.movupw(3);
                None
            }
            Instruction::MovDnW2 => {
                self.movdnw(2);
                None
            }
            Instruction::MovDnW3 => {
                self.movdnw(3);
                None
            }
            Instruction::DupW0 => self.dupw_template(0),
            Instruction::DupW1 => self.dupw_template(1),
            Instruction::DupW2 => self.dupw_template(2),
            Instruction::DupW3 => self.dupw_template(3),
            Instruction::PadW => {
                let mut vars = Vec::new();
                for _ in 0..4 {
                    let var = state.new_local();
                    state.push(var);
                    vars.push(var);
                }
                vars.reverse();
                let mut out = PseudocodeBuilder::new();
                out.text("(");
                for (i, var) in vars.iter().enumerate() {
                    if i > 0 {
                        out.text(", ");
                    }
                    out.var(*var);
                }
                out.text(") = (0, 0, 0, 0)");
                Some(out.build())
            }
            Instruction::Reversew => {
                state.swap(0, 3);
                state.swap(1, 2);
                None
            }
            Instruction::Reversedw => {
                for i in 0..4 {
                    state.swap(i, 7 - i);
                }
                None
            }
            Instruction::Eqw => {
                for _ in 0..8 {
                    state.pop();
                }
                let var = state.new_local();
                state.push(var);
                let mut out = PseudocodeBuilder::new();
                out.var(var);
                out.text(" = eqw()");
                Some(out.build())
            }
            Instruction::AssertEqw | Instruction::AssertEqwWithError(_) => {
                for _ in 0..8 {
                    state.pop();
                }
                Some(PseudocodeTemplate::new().literal("assert_eqw()"))
            }
            _ => None,
        }
    }

    fn handle_conditional_ops(
        &mut self,
        inst: &miden_assembly_syntax::ast::Instruction,
    ) -> Option<PseudocodeTemplate> {
        use miden_assembly_syntax::ast::Instruction;
        let state = self.state.as_mut()?;

        match inst {
            Instruction::CSwap => {
                let cond = state.pop();
                let top0 = state.pop();
                let top1 = state.pop();

                let lower = state.new_local();
                let upper = state.new_local();
                state.push(lower);
                state.push(upper);

                state.ctx.add_phi(lower, vec![top1, top0]);
                state.ctx.add_phi(upper, vec![top0, top1]);

                let mut out = PseudocodeBuilder::new();
                out.text("(");
                out.var(lower);
                out.text(", ");
                out.var(upper);
                out.text(") = cswap(");
                out.var(cond);
                out.text(", ");
                out.var(top1);
                out.text(", ");
                out.var(top0);
                out.text(")");
                Some(out.build())
            }
            Instruction::CSwapW => {
                let cond = state.pop();

                let mut upper = Vec::new();
                for _ in 0..4 {
                    upper.push(state.pop());
                }
                let mut lower = Vec::new();
                for _ in 0..4 {
                    lower.push(state.pop());
                }

                let mut lower_new = Vec::new();
                for _ in 0..4 {
                    let var = state.new_local();
                    state.push(var);
                    lower_new.push(var);
                }
                let mut upper_new = Vec::new();
                for _ in 0..4 {
                    let var = state.new_local();
                    state.push(var);
                    upper_new.push(var);
                }

                for i in 0..4 {
                    state.ctx.add_phi(lower_new[i], vec![lower[i], upper[i]]);
                    state.ctx.add_phi(upper_new[i], vec![upper[i], lower[i]]);
                }

                let mut out = PseudocodeBuilder::new();
                out.text("cswapw(");
                out.var(cond);
                out.text(")");
                Some(out.build())
            }
            Instruction::CDrop => {
                let cond = state.pop();
                state.pop();
                let mut out = PseudocodeBuilder::new();
                out.text("cdrop(");
                out.var(cond);
                out.text(")");
                Some(out.build())
            }
            Instruction::CDropW => {
                let cond = state.pop();
                for _ in 0..4 {
                    state.pop();
                }
                let mut out = PseudocodeBuilder::new();
                out.text("cdropw(");
                out.var(cond);
                out.text(")");
                Some(out.build())
            }
            _ => None,
        }
    }

    fn handle_assertions(
        &mut self,
        inst: &miden_assembly_syntax::ast::Instruction,
    ) -> Option<PseudocodeTemplate> {
        use miden_assembly_syntax::ast::Instruction;
        let state = self.state.as_mut()?;

        match inst {
            Instruction::Assert | Instruction::AssertWithError(_) => {
                let cond = state.pop();
                let mut out = PseudocodeBuilder::new();
                out.text("assert(");
                out.var(cond);
                out.text(")");
                Some(out.build())
            }
            Instruction::AssertEq | Instruction::AssertEqWithError(_) => {
                let b = state.pop();
                let a = state.pop();
                let mut out = PseudocodeBuilder::new();
                out.text("assert(");
                out.var(a);
                out.text(" == ");
                out.var(b);
                out.text(")");
                Some(out.build())
            }
            Instruction::Assertz | Instruction::AssertzWithError(_) => {
                let val = state.pop();
                let mut out = PseudocodeBuilder::new();
                out.text("assert(");
                out.var(val);
                out.text(" == 0)");
                Some(out.build())
            }
            _ => None,
        }
    }

    fn handle_misc_ops(
        &mut self,
        inst: &miden_assembly_syntax::ast::Instruction,
    ) -> Option<PseudocodeTemplate> {
        use miden_assembly_syntax::ast::Instruction;
        let state = self.state.as_mut()?;

        match inst {
            Instruction::ILog2 => Some(unary_fn(state, "ilog2")),
            Instruction::Pow2 => Some(unary_fn(state, "pow2")),
            Instruction::Exp => Some(binary_op(state, "**")),
            Instruction::ExpImm(imm) => Some(binary_imm_op(state, "**", &Self::format_imm(imm))),
            Instruction::ExpBitLength(bits) => {
                let exp = state.pop();
                let base = state.pop();
                let var = state.new_local();
                state.push(var);
                let mut out = PseudocodeBuilder::new();
                out.var(var);
                out.text(" = ");
                out.var(base);
                out.text(&format!(" ** ("));
                out.var(exp);
                out.text(&format!(", {}-bit)", bits));
                Some(out.build())
            }
            Instruction::IsOdd => Some(unary_fn(state, "is_odd")),
            Instruction::Nop
            | Instruction::Breakpoint
            | Instruction::Debug(_)
            | Instruction::Emit
            | Instruction::EmitImm(_)
            | Instruction::Trace(_)
            | Instruction::SysEvent(_) => None,
            _ => None,
        }
    }

    fn handle_complex_stark_ops(
        &mut self,
        inst: &miden_assembly_syntax::ast::Instruction,
    ) -> Option<PseudocodeTemplate> {
        use miden_assembly_syntax::ast::Instruction;
        let state = self.state.as_mut()?;

        match inst {
            Instruction::HornerBase => {
                let mut inputs = Vec::new();
                for _ in 0..16 {
                    inputs.push(state.pop());
                }
                inputs.reverse();

                for i in 0..16 {
                    if i == 0 || i == 1 {
                        let var = state.new_local();
                        state.push(var);
                    } else {
                        state.push(inputs[i]);
                    }
                }

                let acc0_new = state.peek(15).unwrap_or_else(|| state.new_local());
                let acc1_new = state.peek(14).unwrap_or_else(|| state.new_local());

                let mut out = PseudocodeBuilder::new();
                out.text("(");
                out.var(acc1_new);
                out.text(", ");
                out.var(acc0_new);
                out.text(") = horner_eval_base(");
                for i in 0..8 {
                    if i > 0 {
                        out.text(", ");
                    }
                    out.var(inputs[i]);
                }
                out.text(", alpha@");
                out.var(inputs[13]);
                out.text(", ");
                out.var(inputs[14]);
                out.text(", ");
                out.var(inputs[15]);
                out.text(")");
                Some(out.build())
            }
            Instruction::HornerExt => {
                let mut inputs = Vec::new();
                for _ in 0..16 {
                    inputs.push(state.pop());
                }
                inputs.reverse();

                for i in 0..16 {
                    if i == 0 || i == 1 {
                        let var = state.new_local();
                        state.push(var);
                    } else {
                        state.push(inputs[i]);
                    }
                }

                let acc0_new = state.peek(15).unwrap_or_else(|| state.new_local());
                let acc1_new = state.peek(14).unwrap_or_else(|| state.new_local());

                let mut out = PseudocodeBuilder::new();
                out.text("(");
                out.var(acc1_new);
                out.text(", ");
                out.var(acc0_new);
                out.text(") = horner_eval_ext(");
                for i in 0..8 {
                    if i > 0 {
                        out.text(", ");
                    }
                    out.var(inputs[i]);
                }
                out.text(", alpha@");
                out.var(inputs[13]);
                out.text(", ");
                out.var(inputs[14]);
                out.text(", ");
                out.var(inputs[15]);
                out.text(")");
                Some(out.build())
            }
            Instruction::FriExt2Fold4 => {
                let mut inputs = Vec::new();
                for _ in 0..17 {
                    inputs.push(state.pop());
                }
                inputs.reverse();

                let mut outputs = Vec::new();
                for _ in 0..16 {
                    let var = state.new_local();
                    state.push(var);
                    outputs.push(var);
                }

                let out_vars: Vec<_> = (0..16).filter_map(|i| state.peek(i)).collect();

                let mut out = PseudocodeBuilder::new();
                out.text("(_, _, _, _, _, _, _, _, _, _, ");
                if out_vars.len() >= 6 {
                    out.var(out_vars[5]);
                    out.text(", ");
                    out.var(out_vars[4]);
                    out.text(", ");
                    out.var(out_vars[3]);
                    out.text(", ");
                    out.var(out_vars[2]);
                    out.text(", ");
                    out.var(out_vars[1]);
                    out.text(", ");
                    out.var(out_vars[0]);
                }
                out.text(") = fri_ext2fold4(");
                for i in 0..8.min(inputs.len()) {
                    if i > 0 {
                        out.text(", ");
                    }
                    out.var(inputs[i]);
                }
                out.text(", ...)");
                Some(out.build())
            }
            Instruction::EvalCircuit => {
                let n_eval = state.peek(0);
                let n_read = state.peek(1);
                let ptr = state.peek(2);

                let mut out = PseudocodeBuilder::new();
                out.text("eval_circuit(ptr=");
                if let Some(p) = ptr {
                    out.var(p);
                }
                out.text(", n_read=");
                if let Some(n) = n_read {
                    out.var(n);
                }
                out.text(", n_eval=");
                if let Some(n) = n_eval {
                    out.var(n);
                }
                out.text(")");
                Some(out.build())
            }
            Instruction::LogPrecompile => {
                let mut inputs = Vec::new();
                for _ in 0..8 {
                    inputs.push(state.pop());
                }
                inputs.reverse();

                for _ in 0..12 {
                    let var = state.new_local();
                    state.push(var);
                }

                let out_vars: Vec<_> = (0..12).filter_map(|i| state.peek(i)).collect();

                let mut out = PseudocodeBuilder::new();
                out.text("(");
                for (i, var) in out_vars.iter().rev().enumerate() {
                    if i > 0 {
                        out.text(", ");
                    }
                    out.var(*var);
                }
                out.text(") = log_precompile(");
                for (i, var) in inputs.iter().enumerate() {
                    if i > 0 {
                        out.text(", ");
                    }
                    out.var(*var);
                }
                out.text(")");
                Some(out.build())
            }
            _ => None,
        }
    }

    fn handle_memory_stream_op(
        &mut self,
        inst: &miden_assembly_syntax::ast::Instruction,
    ) -> Option<PseudocodeTemplate> {
        use miden_assembly_syntax::ast::Instruction;
        let state = self.state.as_mut()?;

        match inst {
            Instruction::MemStream => {
                let mut inputs = Vec::new();
                for _ in 0..13 {
                    inputs.push(state.pop());
                }
                inputs.reverse();

                let mut outputs = Vec::new();
                for _ in 0..13 {
                    let var = state.new_local();
                    state.push(var);
                    outputs.push(var);
                }
                outputs.reverse();

                let mut out = PseudocodeBuilder::new();
                out.text("(");
                for (i, var) in outputs.iter().enumerate() {
                    if i > 0 {
                        out.text(", ");
                    }
                    out.var(*var);
                }
                out.text(") = mem_stream(");
                for (i, var) in inputs.iter().enumerate() {
                    if i > 0 {
                        out.text(", ");
                    }
                    out.var(*var);
                }
                out.text(")");
                Some(out.build())
            }
            _ => None,
        }
    }

    fn handle_proc_ref(
        &mut self,
        inst: &miden_assembly_syntax::ast::Instruction,
    ) -> Option<PseudocodeTemplate> {
        use miden_assembly_syntax::ast::Instruction;
        let state = self.state.as_mut()?;

        match inst {
            Instruction::ProcRef(target) => {
                let mut vars = Vec::new();
                for _ in 0..4 {
                    let var = state.new_local();
                    state.push(var);
                    vars.push(var);
                }
                vars.reverse();
                let mut out = PseudocodeBuilder::new();
                out.text("(");
                for (i, var) in vars.iter().enumerate() {
                    if i > 0 {
                        out.text(", ");
                    }
                    out.var(*var);
                }
                out.text(&format!(") = procref({})", target));
                Some(out.build())
            }
            _ => None,
        }
    }

    fn handle_calls(
        &mut self,
        inst: &miden_assembly_syntax::ast::Instruction,
        span: SourceSpan,
    ) -> Option<PseudocodeTemplate> {
        use crate::analysis::contracts::StackEffect;
        use miden_assembly_syntax::ast::Instruction;
        let state = self.state.as_mut()?;

        match inst {
            Instruction::Exec(target)
            | Instruction::Call(target)
            | Instruction::SysCall(target) => {
                let resolved_path = self.resolver.resolve_target(target);
                let stack_effect = resolved_path
                    .as_ref()
                    .and_then(|path| self.contracts.and_then(|c| c.get(path)))
                    .map(|c| c.stack_effect.clone());

                match stack_effect {
                    Some(StackEffect::Known { inputs, outputs }) => {
                        let mut input_vars = Vec::new();
                        for _ in 0..inputs {
                            input_vars.push(state.pop());
                        }
                        input_vars.reverse();

                        let mut output_vars = Vec::new();
                        for _ in 0..outputs {
                            let var = state.new_local();
                            state.push(var);
                            output_vars.push(var);
                        }
                        output_vars.reverse();

                        let mut out = PseudocodeBuilder::new();
                        if !output_vars.is_empty() {
                            out.text("(");
                            for (i, var) in output_vars.iter().enumerate() {
                                if i > 0 {
                                    out.text(", ");
                                }
                                out.var(*var);
                            }
                            out.text(") = ");
                        }
                        out.text(&format!("{}(", target));
                        for (i, var) in input_vars.iter().enumerate() {
                            if i > 0 {
                                out.text(", ");
                            }
                            out.var(*var);
                        }
                        out.text(")");
                        Some(out.build())
                    }
                    Some(StackEffect::KnownInputs { inputs }) => {
                        let mut input_vars = Vec::new();
                        for _ in 0..inputs {
                            input_vars.push(state.pop());
                        }
                        input_vars.reverse();
                        let mut output_vars = Vec::new();
                        for _ in 0..inputs {
                            let var = state.new_local();
                            state.push(var);
                            output_vars.push(var);
                        }
                        output_vars.reverse();

                        let mut out = PseudocodeBuilder::new();
                        if !output_vars.is_empty() {
                            out.text("(");
                            for (i, var) in output_vars.iter().enumerate() {
                                if i > 0 {
                                    out.text(", ");
                                }
                                out.var(*var);
                            }
                            out.text(") = ");
                        }
                        out.text(&format!("{}(", target));
                        for (i, var) in input_vars.iter().enumerate() {
                            if i > 0 {
                                out.text(", ");
                            }
                            out.var(*var);
                        }
                        out.text(")");
                        Some(out.build())
                    }
                    _ => {
                        self.fail_decompilation(
                            span,
                            &format!("call target `{target}` has no known stack effect"),
                        );
                        None
                    }
                }
            }
            _ => None,
        }
    }

    fn handle_dynamic_calls(
        &mut self,
        inst: &miden_assembly_syntax::ast::Instruction,
        span: SourceSpan,
    ) -> Option<PseudocodeTemplate> {
        use miden_assembly_syntax::ast::Instruction;

        match inst {
            Instruction::DynExec | Instruction::DynCall => {
                let name = if matches!(inst, Instruction::DynExec) {
                    "dynexec"
                } else {
                    "dyncall"
                };
                self.fail_decompilation(
                    span,
                    &format!("dynamic call `{name}` prevents decompilation"),
                );
                None
            }
            _ => None,
        }
    }

    fn handle_comparison_ops(
        &mut self,
        inst: &miden_assembly_syntax::ast::Instruction,
    ) -> Option<PseudocodeTemplate> {
        use miden_assembly_syntax::ast::Instruction;
        let state = self.state.as_mut()?;

        match inst {
            Instruction::Eq => Some(comparison(state, "==")),
            Instruction::Neq => Some(comparison(state, "!=")),
            Instruction::Lt => Some(comparison(state, "<")),
            Instruction::Lte => Some(comparison(state, "<=")),
            Instruction::Gt => Some(comparison(state, ">")),
            Instruction::Gte => Some(comparison(state, ">=")),
            Instruction::EqImm(imm) => Some(comparison_imm(state, "==", &Self::format_imm(imm))),
            Instruction::NeqImm(imm) => Some(comparison_imm(state, "!=", &Self::format_imm(imm))),
            _ => None,
        }
    }

    fn handle_boolean_ops(
        &mut self,
        inst: &miden_assembly_syntax::ast::Instruction,
    ) -> Option<PseudocodeTemplate> {
        use miden_assembly_syntax::ast::Instruction;
        let state = self.state.as_mut()?;

        match inst {
            Instruction::And => Some(binary_op(state, "&&")),
            Instruction::Or => Some(binary_op(state, "||")),
            Instruction::Xor => Some(binary_op(state, "^")),
            Instruction::Not => Some(unary_op(state, "!")),
            _ => None,
        }
    }

    fn handle_ext2_ops(
        &mut self,
        inst: &miden_assembly_syntax::ast::Instruction,
    ) -> Option<PseudocodeTemplate> {
        use miden_assembly_syntax::ast::Instruction;
        let state = self.state.as_mut()?;

        match inst {
            Instruction::Ext2Add => Some(ext2_binary_op(state, "+")),
            Instruction::Ext2Sub => Some(ext2_binary_op(state, "-")),
            Instruction::Ext2Mul => Some(ext2_binary_op(state, "*")),
            Instruction::Ext2Div => Some(ext2_binary_op(state, "/")),
            Instruction::Ext2Neg => Some(ext2_unary_op(state, "-")),
            Instruction::Ext2Inv => Some(ext2_unary_fn(state, "inv")),
            _ => None,
        }
    }

    fn handle_u32_ops(
        &mut self,
        inst: &miden_assembly_syntax::ast::Instruction,
    ) -> Option<PseudocodeTemplate> {
        use miden_assembly_syntax::ast::Instruction;
        let state = self.state.as_mut()?;

        match inst {
            Instruction::U32And => Some(binary_op(state, "&")),
            Instruction::U32Or => Some(binary_op(state, "|")),
            Instruction::U32Xor => Some(binary_op(state, "^")),
            Instruction::U32Not => Some(unary_op(state, "~")),
            Instruction::U32WrappingAdd => Some(binary_op(state, "+")),
            Instruction::U32WrappingSub => Some(binary_op(state, "-")),
            Instruction::U32WrappingMul => Some(binary_op(state, "*")),
            Instruction::U32Div => Some(binary_op(state, "/")),
            Instruction::U32Mod => Some(binary_op(state, "%")),
            Instruction::U32WrappingAddImm(imm) => {
                Some(binary_imm_op(state, "+", &Self::format_imm(imm)))
            }
            Instruction::U32WrappingSubImm(imm) => {
                Some(binary_imm_op(state, "-", &Self::format_imm(imm)))
            }
            Instruction::U32WrappingMulImm(imm) => {
                Some(binary_imm_op(state, "*", &Self::format_imm(imm)))
            }
            Instruction::U32DivImm(imm) => Some(binary_imm_op(state, "/", &Self::format_imm(imm))),
            Instruction::U32ModImm(imm) => Some(binary_imm_op(state, "%", &Self::format_imm(imm))),
            Instruction::U32Shl => Some(binary_op(state, "<<")),
            Instruction::U32Shr => Some(binary_op(state, ">>")),
            Instruction::U32ShlImm(imm) => Some(binary_imm_op(state, "<<", &Self::format_imm(imm))),
            Instruction::U32ShrImm(imm) => Some(binary_imm_op(state, ">>", &Self::format_imm(imm))),
            Instruction::U32Rotl => Some(binary_op(state, "rotl")),
            Instruction::U32Rotr => Some(binary_op(state, "rotr")),
            Instruction::U32RotlImm(imm) => {
                Some(binary_imm_op(state, "rotl", &Self::format_imm(imm)))
            }
            Instruction::U32RotrImm(imm) => {
                Some(binary_imm_op(state, "rotr", &Self::format_imm(imm)))
            }
            Instruction::U32Lt => Some(comparison(state, "<")),
            Instruction::U32Lte => Some(comparison(state, "<=")),
            Instruction::U32Gt => Some(comparison(state, ">")),
            Instruction::U32Gte => Some(comparison(state, ">=")),
            Instruction::U32Min => Some(binary_op(state, "min")),
            Instruction::U32Max => Some(binary_op(state, "max")),
            Instruction::U32Popcnt => Some(unary_fn(state, "popcnt")),
            Instruction::U32Clz => Some(unary_fn(state, "clz")),
            Instruction::U32Ctz => Some(unary_fn(state, "ctz")),
            Instruction::U32Clo => Some(unary_fn(state, "clo")),
            Instruction::U32Cto => Some(unary_fn(state, "cto")),
            Instruction::U32Cast => Some(unary_fn(state, "u32")),
            Instruction::U32Assert
            | Instruction::U32AssertWithError(_)
            | Instruction::U32Assert2
            | Instruction::U32Assert2WithError(_)
            | Instruction::U32AssertW
            | Instruction::U32AssertWWithError(_) => None,
            Instruction::U32OverflowingAdd => {
                let b = state.pop();
                let a = state.pop();
                let overflow = state.new_local();
                let result = state.new_local();
                state.push(overflow);
                state.push(result);
                let mut out = PseudocodeBuilder::new();
                out.text("(");
                out.var(result);
                out.text(", ");
                out.var(overflow);
                out.text(") = ");
                out.var(a);
                out.text(" + ");
                out.var(b);
                out.text(" (overflow)");
                Some(out.build())
            }
            Instruction::U32OverflowingSub => {
                let b = state.pop();
                let a = state.pop();
                let underflow = state.new_local();
                let result = state.new_local();
                state.push(underflow);
                state.push(result);
                let mut out = PseudocodeBuilder::new();
                out.text("(");
                out.var(result);
                out.text(", ");
                out.var(underflow);
                out.text(") = ");
                out.var(a);
                out.text(" - ");
                out.var(b);
                out.text(" (underflow)");
                Some(out.build())
            }
            Instruction::U32OverflowingMul => {
                let b = state.pop();
                let a = state.pop();
                let overflow = state.new_local();
                let result = state.new_local();
                state.push(overflow);
                state.push(result);
                let mut out = PseudocodeBuilder::new();
                out.text("(");
                out.var(result);
                out.text(", ");
                out.var(overflow);
                out.text(") = ");
                out.var(a);
                out.text(" * ");
                out.var(b);
                out.text(" (overflow)");
                Some(out.build())
            }
            Instruction::U32OverflowingAddImm(imm) => {
                let a = state.pop();
                let overflow = state.new_local();
                let result = state.new_local();
                state.push(overflow);
                state.push(result);
                let mut out = PseudocodeBuilder::new();
                out.text("(");
                out.var(result);
                out.text(", ");
                out.var(overflow);
                out.text(") = ");
                out.var(a);
                out.text(&format!(" + {} (overflow)", Self::format_imm(imm)));
                Some(out.build())
            }
            Instruction::U32OverflowingSubImm(imm) => {
                let a = state.pop();
                let underflow = state.new_local();
                let result = state.new_local();
                state.push(underflow);
                state.push(result);
                let mut out = PseudocodeBuilder::new();
                out.text("(");
                out.var(result);
                out.text(", ");
                out.var(underflow);
                out.text(") = ");
                out.var(a);
                out.text(&format!(" - {} (underflow)", Self::format_imm(imm)));
                Some(out.build())
            }
            Instruction::U32OverflowingMulImm(imm) => {
                let a = state.pop();
                let overflow = state.new_local();
                let result = state.new_local();
                state.push(overflow);
                state.push(result);
                let mut out = PseudocodeBuilder::new();
                out.text("(");
                out.var(result);
                out.text(", ");
                out.var(overflow);
                out.text(") = ");
                out.var(a);
                out.text(&format!(" * {} (overflow)", Self::format_imm(imm)));
                Some(out.build())
            }
            Instruction::U32OverflowingAdd3 => {
                let c = state.pop();
                let b = state.pop();
                let a = state.pop();
                let carry = state.new_local();
                let result = state.new_local();
                state.push(carry);
                state.push(result);
                let mut out = PseudocodeBuilder::new();
                out.text("(");
                out.var(result);
                out.text(", ");
                out.var(carry);
                out.text(") = ");
                out.var(a);
                out.text(" + ");
                out.var(b);
                out.text(" + ");
                out.var(c);
                out.text(" (carry)");
                Some(out.build())
            }
            Instruction::U32WrappingAdd3 => {
                let c = state.pop();
                let b = state.pop();
                let a = state.pop();
                let result = state.new_local();
                state.push(result);
                let mut out = PseudocodeBuilder::new();
                out.var(result);
                out.text(" = ");
                out.var(a);
                out.text(" + ");
                out.var(b);
                out.text(" + ");
                out.var(c);
                Some(out.build())
            }
            Instruction::U32OverflowingMadd => {
                let c = state.pop();
                let b = state.pop();
                let a = state.pop();
                let overflow = state.new_local();
                let result = state.new_local();
                state.push(overflow);
                state.push(result);
                let mut out = PseudocodeBuilder::new();
                out.text("(");
                out.var(result);
                out.text(", ");
                out.var(overflow);
                out.text(") = ");
                out.var(a);
                out.text(" * ");
                out.var(b);
                out.text(" + ");
                out.var(c);
                out.text(" (overflow)");
                Some(out.build())
            }
            Instruction::U32WrappingMadd => {
                let c = state.pop();
                let b = state.pop();
                let a = state.pop();
                let result = state.new_local();
                state.push(result);
                let mut out = PseudocodeBuilder::new();
                out.var(result);
                out.text(" = ");
                out.var(a);
                out.text(" * ");
                out.var(b);
                out.text(" + ");
                out.var(c);
                Some(out.build())
            }
            Instruction::U32DivMod => {
                let b = state.pop();
                let a = state.pop();
                let remainder = state.new_local();
                let quotient = state.new_local();
                state.push(remainder);
                state.push(quotient);
                let mut out = PseudocodeBuilder::new();
                out.text("(");
                out.var(quotient);
                out.text(", ");
                out.var(remainder);
                out.text(") = divmod(");
                out.var(a);
                out.text(", ");
                out.var(b);
                out.text(")");
                Some(out.build())
            }
            Instruction::U32DivModImm(imm) => {
                let a = state.pop();
                let remainder = state.new_local();
                let quotient = state.new_local();
                state.push(remainder);
                state.push(quotient);
                let mut out = PseudocodeBuilder::new();
                out.text("(");
                out.var(quotient);
                out.text(", ");
                out.var(remainder);
                out.text(") = divmod(");
                out.var(a);
                out.text(&format!(", {})", Self::format_imm(imm)));
                Some(out.build())
            }
            Instruction::U32Split => {
                let a = state.pop();
                let lo = state.new_local();
                let hi = state.new_local();
                state.push(lo);
                state.push(hi);
                let mut out = PseudocodeBuilder::new();
                out.text("(");
                out.var(hi);
                out.text(", ");
                out.var(lo);
                out.text(") = split(");
                out.var(a);
                out.text(")");
                Some(out.build())
            }
            Instruction::U32Test | Instruction::U32TestW => {
                let var = state.new_local();
                state.push(var);
                let mut out = PseudocodeBuilder::new();
                out.var(var);
                out.text(" = is_u32(top)");
                Some(out.build())
            }
            _ => None,
        }
    }
}

impl ParametricExpr {
    /// Format the parametric expression using the given counter names.
    ///
    /// Applies simplification rules:
    /// - If only one term with base=0 and stride=1: `a_i` instead of `a_(i)`
    /// - Zero base is omitted: `a_(3*i)` instead of `a_(0+3*i)`
    /// - Coefficient of 1 is omitted: `a_(3+i)` instead of `a_(3+1*i)`
    fn format(&self, counter_names: &[String]) -> String {
        let loop_terms: Vec<LoopTerm> = self
            .terms
            .iter()
            .map(|t| LoopTerm::new(t.stride, t.counter_idx))
            .collect();
        let symbolic = if loop_terms.is_empty() {
            SymbolicExpr::Input(self.base as usize)
        } else {
            SymbolicExpr::ParametricInput {
                base: self.base,
                loop_terms,
            }
        };

        let counters: Vec<&str> = counter_names.iter().map(|s| s.as_str()).collect();
        let formatted = symbolic.to_pseudocode_multi(&counters);
        // Replace the default "a_" prefix with the requested one (currently only 'a').
        formatted.replacen("a_", &format!("{}_", self.prefix), 1)
    }

    fn from_symbolic(expr: &SymbolicExpr) -> Option<Self> {
        match expr {
            SymbolicExpr::ParametricInput { base, loop_terms } => {
                let terms = loop_terms
                    .iter()
                    .map(|t| ParametricTerm::new(t.stride, t.loop_depth))
                    .collect();
                Some(ParametricExpr::argument(*base, terms))
            }
            SymbolicExpr::Input(i) => Some(ParametricExpr::argument(*i as i32, Vec::new())),
            _ => None,
        }
    }
}

/// Information about an active loop during decompilation.
///
/// Stored in the collector's loop_stack to track loop context for parametric naming.
#[derive(Debug, Clone)]
struct ActiveLoop {
    /// Counter variable name (e.g., "i", "j", "k")
    counter_name: String,
    /// Symbolic stack snapshot after one iteration (with loop term offsets applied)
    analyzed_stack: Vec<SymbolicExpr>,
    /// Net stack effect per iteration for this loop
    net_effect: i32,
    /// Next local index when the loop was entered (to identify loop-created locals)
    local_base: usize,
}

impl ActiveLoop {
    fn new(
        counter_name: String,
        analyzed_stack: Vec<SymbolicExpr>,
        net_effect: i32,
        local_base: usize,
    ) -> Self {
        Self {
            counter_name,
            analyzed_stack,
            net_effect,
            local_base,
        }
    }

    fn expr_for_argument(&self, arg_idx: usize) -> Option<SymbolicExpr> {
        // Prefer the topmost occurrence of this argument (by base index).
        self.analyzed_stack
            .iter()
            .rev()
            .find_map(|expr| find_param_expr(expr, arg_idx))
    }
}

fn find_param_expr(expr: &SymbolicExpr, arg_idx: usize) -> Option<SymbolicExpr> {
    match expr {
        SymbolicExpr::Input(i) if *i == arg_idx => Some(SymbolicExpr::Input(*i)),
        SymbolicExpr::ParametricInput { base, .. } if *base == arg_idx as i32 => Some(expr.clone()),
        SymbolicExpr::BinaryOp { left, right, .. } => {
            find_param_expr(left, arg_idx).or_else(|| find_param_expr(right, arg_idx))
        }
        SymbolicExpr::UnaryOp { operand, .. } => find_param_expr(operand, arg_idx),
        SymbolicExpr::MemoryLoad { address } => find_param_expr(address, arg_idx),
        _ => None,
    }
}

fn shift_loop_depth(expr: &SymbolicExpr, offset: usize) -> SymbolicExpr {
    match expr {
        SymbolicExpr::ParametricInput { base, loop_terms } => {
            let shifted_terms = loop_terms
                .iter()
                .map(|t| LoopTerm::new(t.stride, t.loop_depth + offset))
                .collect();
            SymbolicExpr::ParametricInput {
                base: *base,
                loop_terms: shifted_terms,
            }
        }
        SymbolicExpr::BinaryOp { op, left, right } => SymbolicExpr::BinaryOp {
            op: *op,
            left: Box::new(shift_loop_depth(left, offset)),
            right: Box::new(shift_loop_depth(right, offset)),
        },
        SymbolicExpr::UnaryOp { op, operand } => SymbolicExpr::UnaryOp {
            op: *op,
            operand: Box::new(shift_loop_depth(operand, offset)),
        },
        SymbolicExpr::MemoryLoad { address } => SymbolicExpr::MemoryLoad {
            address: Box::new(shift_loop_depth(address, offset)),
        },
        _ => expr.clone(),
    }
}

fn shift_loop_stack(stack: &[SymbolicExpr], offset: usize) -> Vec<SymbolicExpr> {
    stack
        .iter()
        .map(|expr| shift_loop_depth(expr, offset))
        .collect()
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
    /// Map from SSA IDs to parametric expressions (computed at hint creation time)
    parametric_map: HashMap<SsaId, ParametricExpr>,
    /// Counter names at the time of hint creation (for formatting parametric expressions)
    counter_names: Vec<String>,
}

impl SsaHint {
    fn new(line: u32, template: PseudocodeTemplate, indent_level: usize) -> Self {
        Self {
            line,
            template,
            indent_level,
            parametric_map: HashMap::new(),
            counter_names: Vec::new(),
        }
    }

    fn with_parametric(
        line: u32,
        template: PseudocodeTemplate,
        indent_level: usize,
        parametric_map: HashMap<SsaId, ParametricExpr>,
        counter_names: Vec<String>,
    ) -> Self {
        Self {
            line,
            template,
            indent_level,
            parametric_map,
            counter_names,
        }
    }

    /// Resolve to a final hint string with proper indentation.
    ///
    /// Uses the parametric_map to override display names for SSA IDs that
    /// represent loop-dependent positions.
    fn resolve(&self, ctx: &SsaContext) -> String {
        let indent = "    ".repeat(self.indent_level);
        let content = self.template.resolve_with_overrides(ctx, |id| {
            self.parametric_map
                .get(&id)
                .map(|expr| expr.format(&self.counter_names))
        });
        format!("{}{}", indent, content)
    }
}

/// SSA-based collector that uses phi nodes for variable tracking at merge points.
///
/// This collector uses a two-pass approach:
/// 1. **Pass 1**: Visit AST, generate templates with SSA IDs, create phi nodes at merge points
/// 2. **Pass 2**: Resolve phi relationships, convert templates to final strings
pub struct PseudocodeCollector<'a> {
    /// Current SSA decompiler state
    state: Option<DecompilerState>,
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
    /// Stack of active loops for parametric variable naming
    loop_stack: Vec<ActiveLoop>,
    /// Counter for generating loop counter names
    next_counter_id: usize,
    /// Current procedure name (for diagnostics)
    current_proc_name: Option<String>,
}

impl<'a> PseudocodeCollector<'a> {
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
            loop_stack: Vec::new(),
            next_counter_id: 0,
            current_proc_name: None,
        }
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Loop Context Methods
    // ─────────────────────────────────────────────────────────────────────────

    /// Generate a new loop counter name (i, j, k, ...).
    fn new_counter(&mut self) -> String {
        let name = if self.next_counter_id < super::ssa::LOOP_COUNTER_NAMES.len() {
            super::ssa::LOOP_COUNTER_NAMES[self.next_counter_id].to_string()
        } else {
            format!(
                "i{}",
                self.next_counter_id - super::ssa::LOOP_COUNTER_NAMES.len()
            )
        };
        self.next_counter_id += 1;
        name
    }

    /// Enter a new loop context.
    ///
    /// # Arguments
    /// * `analyzed_stack` - Symbolic stack snapshot after one iteration
    ///
    /// # Returns
    /// The counter name for this loop (e.g., "i", "j", "k")
    fn enter_loop_context(
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
    fn exit_loop_context(&mut self) {
        self.loop_stack.pop();
    }

    /// Get counter names for all active loops.
    fn counter_names(&self) -> Vec<String> {
        self.loop_stack
            .iter()
            .map(|info| info.counter_name.clone())
            .collect()
    }

    /// Build a parametric map for the current template.
    ///
    /// This computes parametric expressions for any SSA IDs on the stack
    /// that represent loop-dependent positions (i.e., arguments accessed
    /// within loops).
    fn build_parametric_map(
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

        // For each SSA ID referenced in the template, check if it needs parametric naming
        for id in template.ssa_ids() {
            if let Some(expr) = self.compute_parametric_expr(state, id, counter_len) {
                map.insert(id, expr);
            }
        }

        map
    }

    /// Compute a parametric expression for an SSA ID if it represents
    /// a loop-dependent position.
    fn compute_parametric_expr(
        &self,
        state: &DecompilerState,
        id: SsaId,
        counter_len: usize,
    ) -> Option<ParametricExpr> {
        use super::ssa::VarKind;

        let value = state.ctx.get_value(id)?;

        // Prefer direct argument; otherwise see if phi-equivalent to an argument
        let base_idx = match &value.kind {
            VarKind::Argument(idx) => Some(*idx),
            _ => state.ctx.argument_index_via_phi(id),
        };

        if base_idx.is_none() {
            // Not argument-backed: attempt to map loop-produced values to parametric outputs.
            return self.compute_output_parametric_expr(state, id, counter_len);
        }

        let base_idx = base_idx.unwrap();

        let mut merged: Option<ParametricExpr> = None;

        // Walk outermost → innermost, merging loop terms for the same base argument.
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
    fn compute_output_parametric_expr(
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
            super::ssa::VarKind::Argument(i) => i as i32,
            super::ssa::VarKind::Local(i) => i as i32,
            super::ssa::VarKind::Return(i) => i as i32,
        };

        // Prefer innermost loops when assigning output indices
        for (counter_idx, loop_ctx) in self.loop_stack.iter().enumerate().rev() {
            if counter_idx >= counter_len {
                continue;
            }
            if loop_ctx.net_effect < 0 {
                return Some(ParametricExpr::output(base_idx, counter_idx));
            } else {
                let produced = loop_ctx.net_effect.max(0) as usize;
                if produced > 0 {
                    if let super::ssa::VarKind::Local(idx) = value.kind {
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
    fn fail_decompilation(&mut self, span: SourceSpan, reason: &str) {
        // Mark SSA state as failed to stop further collection
        if let Some(ref mut state) = self.state {
            state.fail();
        }

        // Drop any hints collected for this procedure
        self.hints.truncate(self.proc_hint_start);

        // Record diagnostic
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
    fn add_literal_hint(&mut self, line: u32, text: &str) {
        let template = PseudocodeTemplate::new().literal(text);
        self.hints
            .push(SsaHint::new(line, template, self.indent_level));
    }

    /// Add a hint from a template, computing parametric map for loop-dependent positions.
    fn add_template_hint(&mut self, line: u32, template: PseudocodeTemplate) {
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
    ///
    /// This is the SSA equivalent of `ToPseudocode::to_pseudocode`.
    fn instruction_to_template(
        &mut self,
        inst: &miden_assembly_syntax::ast::Instruction,
        span: SourceSpan,
    ) -> Option<PseudocodeTemplate> {
        // Check if tracking has already failed
        if self.state.as_ref().map_or(true, |s| s.tracking_failed) {
            return None;
        }

        if let Some(t) = self.handle_push_ops(inst) {
            return Some(t);
        }
        if let Some(t) = self.handle_stack_ops(inst, span) {
            return Some(t);
        }
        if let Some(t) = self.handle_arithmetic_ops(inst) {
            return Some(t);
        }
        if let Some(t) = self.handle_comparison_ops(inst) {
            return Some(t);
        }
        if let Some(t) = self.handle_boolean_ops(inst) {
            return Some(t);
        }
        if let Some(t) = self.handle_ext2_ops(inst) {
            return Some(t);
        }
        if let Some(t) = self.handle_u32_ops(inst) {
            return Some(t);
        }
        if let Some(t) = self.handle_memory_ops(inst) {
            return Some(t);
        }
        if let Some(t) = self.handle_local_memory_ops(inst) {
            return Some(t);
        }
        if let Some(t) = self.handle_advice_ops(inst) {
            return Some(t);
        }
        if let Some(t) = self.handle_crypto_ops(inst) {
            return Some(t);
        }
        if let Some(t) = self.handle_merkle_ops(inst) {
            return Some(t);
        }
        if let Some(t) = self.handle_word_stack_ops(inst, span) {
            return Some(t);
        }
        if let Some(t) = self.handle_conditional_ops(inst) {
            return Some(t);
        }
        if let Some(t) = self.handle_assertions(inst) {
            return Some(t);
        }
        if let Some(t) = self.handle_misc_ops(inst) {
            return Some(t);
        }
        if let Some(t) = self.handle_complex_stark_ops(inst) {
            return Some(t);
        }
        if let Some(t) = self.handle_memory_stream_op(inst) {
            return Some(t);
        }
        if let Some(t) = self.handle_proc_ref(inst) {
            return Some(t);
        }
        if let Some(t) = self.handle_calls(inst, span) {
            return Some(t);
        }
        if let Some(t) = self.handle_dynamic_calls(inst, span) {
            return Some(t);
        }
        None
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Instruction category helpers
    // ─────────────────────────────────────────────────────────────────────────

    #[inline]
    fn format_imm<T: std::fmt::Display>(imm: &miden_assembly_syntax::ast::Immediate<T>) -> String {
        format!("{}", imm)
    }

    fn handle_push_ops(
        &mut self,
        inst: &miden_assembly_syntax::ast::Instruction,
    ) -> Option<PseudocodeTemplate> {
        use miden_assembly_syntax::ast::Instruction;
        let state = self.state.as_mut()?;

        match inst {
            Instruction::Push(imm) => {
                let value = format!("{}", imm);
                let var = state.new_local();
                state.push(var);
                let mut out = PseudocodeBuilder::new();
                out.var(var);
                out.text(" = ");
                out.text(&value);
                Some(out.build())
            }
            Instruction::PushSlice(imm, range) => {
                let count = range.len();
                let mut vars = Vec::new();
                for _ in 0..count {
                    let var = state.new_local();
                    state.push(var);
                    vars.push(var);
                }
                vars.reverse();
                let mut out = PseudocodeBuilder::new();
                out.text("(");
                for (i, var) in vars.iter().enumerate() {
                    if i > 0 {
                        out.text(", ");
                    }
                    out.var(*var);
                }
                out.text(&format!(") = {}[{}..{}]", imm, range.start, range.end));
                Some(out.build())
            }
            Instruction::PushFeltList(values) => {
                let count = values.len();
                let mut vars = Vec::new();
                for _ in 0..count {
                    let var = state.new_local();
                    state.push(var);
                    vars.push(var);
                }
                vars.reverse();
                let mut out = PseudocodeBuilder::new();
                out.text("(");
                for (i, var) in vars.iter().enumerate() {
                    if i > 0 {
                        out.text(", ");
                    }
                    out.var(*var);
                }
                out.text(") = [");
                for (i, val) in values.iter().enumerate() {
                    if i > 0 {
                        out.text(", ");
                    }
                    out.text(&format!("{}", val));
                }
                out.text("]");
                Some(out.build())
            }
            Instruction::Sdepth => {
                let var = state.new_local();
                state.push(var);
                let mut out = PseudocodeBuilder::new();
                out.var(var);
                out.text(" = stack_depth()");
                Some(out.build())
            }
            Instruction::Clk => {
                let var = state.new_local();
                state.push(var);
                let mut out = PseudocodeBuilder::new();
                out.var(var);
                out.text(" = clk()");
                Some(out.build())
            }
            Instruction::Caller => {
                let mut vars = Vec::new();
                for _ in 0..4 {
                    let var = state.new_local();
                    state.push(var);
                    vars.push(var);
                }
                vars.reverse();
                let mut out = PseudocodeBuilder::new();
                out.text("(");
                for (i, var) in vars.iter().enumerate() {
                    if i > 0 {
                        out.text(", ");
                    }
                    out.var(*var);
                }
                out.text(") = caller()");
                Some(out.build())
            }
            _ => None,
        }
    }

    fn handle_stack_ops(
        &mut self,
        inst: &miden_assembly_syntax::ast::Instruction,
        _span: SourceSpan,
    ) -> Option<PseudocodeTemplate> {
        use miden_assembly_syntax::ast::Instruction;
        let state = self.state.as_mut()?;

        match inst {
            Instruction::Dup0
            | Instruction::Dup1
            | Instruction::Dup2
            | Instruction::Dup3
            | Instruction::Dup4
            | Instruction::Dup5
            | Instruction::Dup6
            | Instruction::Dup7
            | Instruction::Dup8
            | Instruction::Dup9
            | Instruction::Dup10
            | Instruction::Dup11
            | Instruction::Dup12
            | Instruction::Dup13
            | Instruction::Dup14
            | Instruction::Dup15 => {
                let n = match inst {
                    Instruction::Dup0 => 0,
                    Instruction::Dup1 => 1,
                    Instruction::Dup2 => 2,
                    Instruction::Dup3 => 3,
                    Instruction::Dup4 => 4,
                    Instruction::Dup5 => 5,
                    Instruction::Dup6 => 6,
                    Instruction::Dup7 => 7,
                    Instruction::Dup8 => 8,
                    Instruction::Dup9 => 9,
                    Instruction::Dup10 => 10,
                    Instruction::Dup11 => 11,
                    Instruction::Dup12 => 12,
                    Instruction::Dup13 => 13,
                    Instruction::Dup14 => 14,
                    Instruction::Dup15 => 15,
                    _ => unreachable!(),
                };
                return self.dup_template(n);
            }
            Instruction::Drop => {
                state.pop();
                return None;
            }
            Instruction::DropW => {
                for _ in 0..4 {
                    state.pop();
                }
                return None;
            }
            Instruction::Swap1
            | Instruction::Swap2
            | Instruction::Swap3
            | Instruction::Swap4
            | Instruction::Swap5
            | Instruction::Swap6
            | Instruction::Swap7
            | Instruction::Swap8
            | Instruction::Swap9
            | Instruction::Swap10
            | Instruction::Swap11
            | Instruction::Swap12
            | Instruction::Swap13
            | Instruction::Swap14
            | Instruction::Swap15 => {
                let n = match inst {
                    Instruction::Swap1 => 1,
                    Instruction::Swap2 => 2,
                    Instruction::Swap3 => 3,
                    Instruction::Swap4 => 4,
                    Instruction::Swap5 => 5,
                    Instruction::Swap6 => 6,
                    Instruction::Swap7 => 7,
                    Instruction::Swap8 => 8,
                    Instruction::Swap9 => 9,
                    Instruction::Swap10 => 10,
                    Instruction::Swap11 => 11,
                    Instruction::Swap12 => 12,
                    Instruction::Swap13 => 13,
                    Instruction::Swap14 => 14,
                    Instruction::Swap15 => 15,
                    _ => unreachable!(),
                };
                state.swap(0, n);
                return None;
            }
            Instruction::MovUp2
            | Instruction::MovUp3
            | Instruction::MovUp4
            | Instruction::MovUp5
            | Instruction::MovUp6
            | Instruction::MovUp7
            | Instruction::MovUp8
            | Instruction::MovUp9
            | Instruction::MovUp10
            | Instruction::MovUp11
            | Instruction::MovUp12
            | Instruction::MovUp13
            | Instruction::MovUp14
            | Instruction::MovUp15 => {
                let n = match inst {
                    Instruction::MovUp2 => 2,
                    Instruction::MovUp3 => 3,
                    Instruction::MovUp4 => 4,
                    Instruction::MovUp5 => 5,
                    Instruction::MovUp6 => 6,
                    Instruction::MovUp7 => 7,
                    Instruction::MovUp8 => 8,
                    Instruction::MovUp9 => 9,
                    Instruction::MovUp10 => 10,
                    Instruction::MovUp11 => 11,
                    Instruction::MovUp12 => 12,
                    Instruction::MovUp13 => 13,
                    Instruction::MovUp14 => 14,
                    Instruction::MovUp15 => 15,
                    _ => unreachable!(),
                };
                state.movup(n);
                return None;
            }
            Instruction::MovDn2
            | Instruction::MovDn3
            | Instruction::MovDn4
            | Instruction::MovDn5
            | Instruction::MovDn6
            | Instruction::MovDn7
            | Instruction::MovDn8
            | Instruction::MovDn9
            | Instruction::MovDn10
            | Instruction::MovDn11
            | Instruction::MovDn12
            | Instruction::MovDn13
            | Instruction::MovDn14
            | Instruction::MovDn15 => {
                let n = match inst {
                    Instruction::MovDn2 => 2,
                    Instruction::MovDn3 => 3,
                    Instruction::MovDn4 => 4,
                    Instruction::MovDn5 => 5,
                    Instruction::MovDn6 => 6,
                    Instruction::MovDn7 => 7,
                    Instruction::MovDn8 => 8,
                    Instruction::MovDn9 => 9,
                    Instruction::MovDn10 => 10,
                    Instruction::MovDn11 => 11,
                    Instruction::MovDn12 => 12,
                    Instruction::MovDn13 => 13,
                    Instruction::MovDn14 => 14,
                    Instruction::MovDn15 => 15,
                    _ => unreachable!(),
                };
                state.movdn(n);
                return None;
            }
            _ => {}
        }
        None
    }

    fn handle_arithmetic_ops(
        &mut self,
        inst: &miden_assembly_syntax::ast::Instruction,
    ) -> Option<PseudocodeTemplate> {
        use miden_assembly_syntax::ast::Instruction;
        let state = self.state.as_mut()?;

        fn format_imm<T: std::fmt::Display>(
            imm: &miden_assembly_syntax::ast::Immediate<T>,
        ) -> String {
            format!("{}", imm)
        }

        match inst {
            Instruction::Add => Some(binary_op(state, "+")),
            Instruction::Sub => Some(binary_op(state, "-")),
            Instruction::Mul => Some(binary_op(state, "*")),
            Instruction::Div => Some(binary_op(state, "/")),
            Instruction::AddImm(imm) => Some(binary_imm_op(state, "+", &format_imm(imm))),
            Instruction::SubImm(imm) => Some(binary_imm_op(state, "-", &format_imm(imm))),
            Instruction::MulImm(imm) => Some(binary_imm_op(state, "*", &format_imm(imm))),
            Instruction::DivImm(imm) => Some(binary_imm_op(state, "/", &format_imm(imm))),
            Instruction::Neg => Some(unary_op(state, "-")),
            Instruction::Inv => {
                let a = state.pop();
                let var = state.new_local();
                state.push(var);
                let mut out = PseudocodeBuilder::new();
                out.var(var);
                out.text(" = 1/");
                out.var(a);
                Some(out.build())
            }
            Instruction::Incr => {
                let a = state.pop();
                let var = state.new_local();
                state.push(var);
                let mut out = PseudocodeBuilder::new();
                out.var(var);
                out.text(" = ");
                out.var(a);
                out.text(" + 1");
                Some(out.build())
            }
            _ => None,
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

        let mut out = PseudocodeBuilder::new();
        out.var(var);
        out.text(" = ");
        out.var(src);
        Some(out.build())
    }

    /// Generate dupw template for word duplication.
    fn dupw_template(&mut self, word_idx: usize) -> Option<PseudocodeTemplate> {
        let state = self.state.as_mut()?;
        let base = word_idx * 4;

        // Collect source values
        let mut sources = Vec::new();
        for i in 0..4 {
            if let Some(src) = state.peek(base + i) {
                sources.push(src);
            }
        }
        if sources.len() != 4 {
            return None;
        }

        // Duplicate the collected values (preserve order)
        for src in sources.iter().rev() {
            state.push(*src);
        }

        // Create new variable names for the duplicated values
        let mut new_vars = Vec::new();
        for _ in 0..4 {
            let var = state.new_local();
            state.pop();
            state.push(var);
            new_vars.push(var);
        }
        new_vars.reverse();

        let mut out = PseudocodeBuilder::new();
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

    /// Swap two words on the stack.
    fn swapw(&mut self, a: usize, b: usize) {
        if let Some(ref mut state) = self.state {
            let base_a = a * 4;
            let base_b = b * 4;
            for i in 0..4 {
                state.swap(base_a + i, base_b + i);
            }
        }
    }

    /// Move word at position n to top.
    fn movupw(&mut self, word_n: usize) {
        if let Some(ref mut state) = self.state {
            // Move 4 elements up, starting from highest index
            let base = word_n * 4;
            for i in (0..4).rev() {
                state.movup(base + i);
            }
        }
    }

    /// Move top word to position n.
    fn movdnw(&mut self, word_n: usize) {
        if let Some(ref mut state) = self.state {
            // Move 4 elements down; adjust target as stack grows
            let target = word_n * 4;
            for i in 0..4 {
                state.movdn(target + i);
            }
        }
    }

    /// Visit all operations in a block, handling control flow.
    fn visit_block_ssa(&mut self, block: &Block) {
        for op in block.iter() {
            if self.state.as_ref().map_or(false, |s| s.tracking_failed) {
                break;
            }
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
            Op::If {
                then_blk, else_blk, ..
            } => {
                // Get condition variable SSA ID
                let condition_id = self.state.as_mut().and_then(|s| s.peek(0));

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
                    if let (Some(ref mut state), Some(ref saved)) = (&mut self.state, &entry_stack)
                    {
                        state.restore_stack(saved);
                    }

                    self.indent_level += 1;
                    self.visit_block_ssa(else_blk);
                    self.indent_level -= 1;

                    // Create phi nodes at merge point
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

                // Generate "end" hint
                if let Some(range) = span_to_range(self.sources, op.span()) {
                    self.add_literal_hint(range.end.line, "end");
                }
            }
            Op::While { body, .. } => {
                // Analyze loop with abstract interpretation
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

                // Get condition variable SSA ID
                let condition_id = self.state.as_mut().and_then(|s| s.peek(0));

                // Enter loop context (use consuming stride if known)
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
                if let (Some(ref mut state), Some(ref entry_stack)) =
                    (&mut self.state, &loop_entry_stack)
                {
                    if let Err(err) = state.create_loop_phis(entry_stack) {
                        self.fail_decompilation(
                            op.span(),
                            &format!("while loop phi creation failed: {err}"),
                        );
                        return;
                    }
                }

                // Exit loop context
                self.exit_loop_context();

                // Generate "end" hint
                if let Some(range) = span_to_range(self.sources, op.span()) {
                    self.add_literal_hint(range.end.line, "end");
                }
            }
            Op::Repeat { count, body, .. } => {
                // Analyze loop to get per-iteration net effect via abstract interpretation
                let loop_analysis = analyze_repeat_loop(body, *count as usize);
                let net_effect = loop_analysis.net_effect_per_iteration;
                let iterations_remaining = count.saturating_sub(1);

                // Fail decompilation if analysis failed or is inconsistent
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

                // Enter loop context (collector now manages loop stack)
                let local_base = self
                    .state
                    .as_ref()
                    .map(|s| s.ctx.next_local_index())
                    .unwrap_or(0);
                let counter = self.enter_loop_context(analyzed_stack, net_effect, local_base);

                // Save loop entry state
                let loop_entry_stack = self.state.as_ref().map(|s| s.save_stack());

                // Generate "for" hint
                if let Some(range) = span_to_range(self.sources, op.span()) {
                    let hint = format!(
                        "for {} = 0; {} < {}; {} = {} + 1:",
                        counter, counter, count, counter, counter
                    );
                    self.add_literal_hint(range.start.line, &hint);
                }

                // Visit body
                self.indent_level += 1;
                self.visit_block_ssa(body);
                self.indent_level -= 1;

                // For consuming loops, synthesize placeholders for outputs that would be
                // produced by the skipped iterations so return names don't reuse inputs.
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
                                state
                                    .ctx
                                    .get_value(**id)
                                    .map_or(false, |v| match v.kind {
                                        super::ssa::VarKind::Local(idx) => idx >= local_base,
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

                // Create phi nodes only for stack-neutral loops
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

                // Exit loop context
                self.exit_loop_context();

                // Apply remaining iterations' net effect to the SSA stack.
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

impl<'a> Visit for PseudocodeCollector<'a> {
    fn visit_procedure(&mut self, proc: &Procedure) -> core::ops::ControlFlow<()> {
        let proc_name = proc.name().as_str().to_string();
        self.current_proc_name = Some(proc_name.clone());

        let decl_range = span_to_range(self.sources, proc.name().span());
        let decl_line = decl_range.as_ref().map(|r| r.start.line);

        // Reset loop context for new procedure
        self.loop_stack.clear();
        self.next_counter_id = 0;

        // Get contract info
        let contract = self.contracts.and_then(|c| c.get_by_name(&proc_name));
        let contract_effect = contract.map(|c| c.stack_effect.clone());
        let contract_signature = contract.and_then(|c| c.signature.clone());

        // Pre-analyze procedure (only used as fallback when no contract is available)
        let pre_analysis = pre_analyze_procedure(proc.body());
        let pre_analyzed_inputs = pre_analysis.total_inputs_required;

        // Determine input/output counts
        // Prefer contract-declared inputs when available, as pre-analysis can over-estimate
        // for procedures with consuming loops (e.g., repeat.N with negative stack effect)
        let (initial_input_count, output_count) = match &contract_effect {
            Some(StackEffect::Known { inputs, outputs }) => {
                // Contract provides exact input count - use it
                (*inputs, Some(*outputs))
            }
            Some(StackEffect::KnownInputs { inputs }) => {
                // Contract provides input count - use it
                (*inputs, None)
            }
            _ => {
                // No contract - use pre-analysis
                (pre_analyzed_inputs, None)
            }
        };

        // Track procedure state
        self.proc_hint_start = self.hints.len();
        self.proc_outputs = output_count;
        self.proc_decl_line = decl_line;

        // Generate signature hint (as literal - no SSA refs needed)
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
            );
            self.add_literal_hint(line, &signature);
        }

        // Initialize SSA state
        self.state = Some(DecompilerState::new(initial_input_count));

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

        // Capture inputs discovered during SSA tracking before any reclassification.
        let final_input_count = self
            .state
            .as_ref()
            .map(|s| s.ctx.argument_count())
            .unwrap_or(initial_input_count);

        // Infer outputs (when not provided by a contract) from the final stack depth and
        // mark them as returns so they render as outputs.
        let mut final_output_count = output_count;
        let mut output_names: Option<Vec<String>> = None;
        if let Some(ref mut state) = self.state {
            let inferred = final_output_count.unwrap_or_else(|| state.depth());
            if inferred > 0 {
                state.mark_returns(inferred);
                final_output_count = Some(inferred);
                // Resolve names to capture return display names (preserving arguments).
                state.ctx.resolve_names();
                output_names = Some(state.ctx.return_names());
            }
        }
        self.proc_outputs = final_output_count;

        // If additional inputs were discovered, refresh the signature hint (unless contract overrides).
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

        // Resolve hints (Pass 2)
        self.resolve_all_hints();

        // Add "end" hint for the procedure
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

        // Clear state for next procedure
        self.state = None;
        self.proc_outputs = None;
        self.proc_decl_line = None;
        self.current_proc_name = None;

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
    let mut collector = PseudocodeCollector::new(module, sources, contracts, source_text);
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
        if line >= visible_range.start.line && line < visible_range.end.line {
            line_hints.entry(line).or_default().push(hint);
        }
    }

    // Convert to InlayHints
    let mut hints: Vec<InlayHint> = line_hints
        .into_iter()
        .map(|(line_num, pseudocodes)| {
            let line_end = line_lengths.get(line_num as usize).copied().unwrap_or(0);

            // Avoid duplicating indentation when multiple hints share a line.
            let mut normalized = pseudocodes.clone();
            for code in normalized.iter_mut().skip(1) {
                *code = code.trim_start().to_string();
            }
            let label = normalized.join("; ");

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
        let mut state = DecompilerState::new(2);

        // Simulate add instruction: pop 2, push 1
        let template = {
            use super::super::ssa::binary_op;
            binary_op(&mut state, "+")
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
        let mut state = DecompilerState::new(1);

        // Initial: [a_0] on stack
        let entry_stack = state.save_stack();

        // Loop body: pop a_0, push v_0 (new value)
        state.pop();
        let v0 = state.new_local();
        state.push(v0);

        // Create loop phis
        state.create_loop_phis(&entry_stack).unwrap();
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
        use super::super::ssa::{binary_imm_op, comparison_imm, unary_op};

        // Initialize with 3 inputs: [n, read_ptr, write_ptr] with n (a_0) on top
        let mut state = DecompilerState::new(3);

        // neg: v_0 = -a_0
        let neg_template: PseudocodeTemplate = unary_op(&mut state, "-");

        // dup: duplicate v_0 to top
        state.dup(0);

        // neq.0: v_1 = (dup_v_0 != 0) - this is the INITIAL loop condition
        let initial_cond_template: PseudocodeTemplate = comparison_imm(&mut state, "!=", "0");
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
        let _counter_update: PseudocodeTemplate = binary_imm_op(&mut state, "+", "1");
        // Stack: [v_3, a_1, a_2]

        // movup.2: bring a_2 to top
        state.movup(2);
        // Stack: [a_2, v_3, a_1]

        // add.1: v_4 = a_2 + 1 (update write_ptr)
        let _write_ptr_update: PseudocodeTemplate = binary_imm_op(&mut state, "+", "1");
        // Stack: [v_4, v_3, a_1]

        // movup.2: bring a_1 to top
        state.movup(2);
        // Stack: [a_1, v_4, v_3]

        // add.1: v_5 = a_1 + 1 (update read_ptr)
        let _read_ptr_update: PseudocodeTemplate = binary_imm_op(&mut state, "+", "1");
        // Stack: [v_5, v_4, v_3]

        // movup.2: bring v_3 (counter) to top
        state.movup(2);
        // Stack: [v_3, v_5, v_4]

        // dup: duplicate counter for condition check
        state.dup(0);

        // neq.0: v_6 = (v_3 != 0) - this is the END-OF-LOOP condition
        let end_cond_template: PseudocodeTemplate = comparison_imm(&mut state, "!=", "0");
        let end_condition = state.peek(0).unwrap();
        // Stack: [v_6 (new condition), v_3 (counter), v_5 (read_ptr), v_4 (write_ptr)]

        // Before phi resolution, the conditions have different SSA IDs
        assert_ne!(
            initial_condition, end_condition,
            "Before phi, conditions should have different SSA IDs"
        );

        // Create phi nodes at loop end
        state.create_loop_phis(&loop_entry_stack).unwrap();

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
        println!(
            "initial condition template: {}",
            initial_cond_template.resolve(&state.ctx)
        );
        println!(
            "end condition template: {}",
            end_cond_template.resolve(&state.ctx)
        );
    }
}
