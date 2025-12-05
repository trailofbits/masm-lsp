use miden_assembly_syntax::ast::Instruction;

use crate::collector::pseudocode_collector::PseudocodeCollector;
use crate::ssa::{PseudocodeBuilder, PseudocodeTemplate};

impl<'a> PseudocodeCollector<'a> {
    pub(crate) fn handle_memory_ops(&mut self, inst: &Instruction) -> Option<PseudocodeTemplate> {
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

    pub(crate) fn handle_local_memory_ops(
        &mut self,
        inst: &Instruction,
    ) -> Option<PseudocodeTemplate> {
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
}
