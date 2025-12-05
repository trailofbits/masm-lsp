use miden_assembly_syntax::ast::Instruction;

use crate::collector::pseudocode_collector::PseudocodeCollector;
use crate::ssa::{
    binary_imm_op, binary_op, comparison, comparison_imm, ext2_binary_op, ext2_unary_fn,
    ext2_unary_op, unary_fn, unary_op, PseudocodeBuilder, PseudocodeTemplate,
};

impl<'a> PseudocodeCollector<'a> {
    pub(crate) fn handle_comparison_ops(
        &mut self,
        inst: &Instruction,
    ) -> Option<PseudocodeTemplate> {
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

    pub(crate) fn handle_boolean_ops(&mut self, inst: &Instruction) -> Option<PseudocodeTemplate> {
        let state = self.state.as_mut()?;

        match inst {
            Instruction::And => Some(binary_op(state, "&&")),
            Instruction::Or => Some(binary_op(state, "||")),
            Instruction::Xor => Some(binary_op(state, "^")),
            Instruction::Not => Some(unary_op(state, "!")),
            _ => None,
        }
    }

    pub(crate) fn handle_ext2_ops(&mut self, inst: &Instruction) -> Option<PseudocodeTemplate> {
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

    pub(crate) fn handle_u32_ops(&mut self, inst: &Instruction) -> Option<PseudocodeTemplate> {
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

    pub(crate) fn handle_arithmetic_ops(
        &mut self,
        inst: &Instruction,
    ) -> Option<PseudocodeTemplate> {
        let state = self.state.as_mut()?;

        match inst {
            Instruction::Add => Some(binary_op(state, "+")),
            Instruction::Sub => Some(binary_op(state, "-")),
            Instruction::Mul => Some(binary_op(state, "*")),
            Instruction::Div => Some(binary_op(state, "/")),
            Instruction::AddImm(imm) => Some(binary_imm_op(state, "+", &Self::format_imm(imm))),
            Instruction::SubImm(imm) => Some(binary_imm_op(state, "-", &Self::format_imm(imm))),
            Instruction::MulImm(imm) => Some(binary_imm_op(state, "*", &Self::format_imm(imm))),
            Instruction::DivImm(imm) => Some(binary_imm_op(state, "/", &Self::format_imm(imm))),
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

    #[inline]
    pub(crate) fn format_imm<T: std::fmt::Display>(
        imm: &miden_assembly_syntax::ast::Immediate<T>,
    ) -> String {
        format!("{}", imm)
    }
}
