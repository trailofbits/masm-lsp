use miden_assembly_syntax::ast::Instruction;
use miden_debug_types::SourceSpan;

use crate::decompiler::collector::pseudocode_collector::PseudocodeCollector;
use crate::decompiler::ssa::{PseudocodeBuilder, PseudocodeTemplate};

impl<'a> PseudocodeCollector<'a> {
    pub(crate) fn handle_push_ops(&mut self, inst: &Instruction) -> Option<PseudocodeTemplate> {
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

    pub(crate) fn handle_stack_ops(
        &mut self,
        inst: &Instruction,
        _span: SourceSpan,
    ) -> Option<PseudocodeTemplate> {
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

    pub(crate) fn handle_word_stack_ops(
        &mut self,
        inst: &Instruction,
        _span: SourceSpan,
    ) -> Option<PseudocodeTemplate> {
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
}
