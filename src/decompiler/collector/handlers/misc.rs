use miden_assembly_syntax::ast::Instruction;

use crate::decompiler::collector::pseudocode_collector::PseudocodeCollector;
use crate::decompiler::ssa::{
    binary_imm_op, binary_op, unary_fn, PseudocodeBuilder, PseudocodeTemplate,
};

impl<'a> PseudocodeCollector<'a> {
    pub(crate) fn handle_conditional_ops(
        &mut self,
        inst: &Instruction,
    ) -> Option<PseudocodeTemplate> {
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

    pub(crate) fn handle_assertions(&mut self, inst: &Instruction) -> Option<PseudocodeTemplate> {
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

    pub(crate) fn handle_misc_ops(&mut self, inst: &Instruction) -> Option<PseudocodeTemplate> {
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
                out.text(" ** (");
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
}
