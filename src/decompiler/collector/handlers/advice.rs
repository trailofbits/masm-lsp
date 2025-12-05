use miden_assembly_syntax::ast::Instruction;

use crate::decompiler::collector::pseudocode_collector::PseudocodeCollector;
use crate::decompiler::ssa::{PseudocodeBuilder, PseudocodeTemplate};

impl<'a> PseudocodeCollector<'a> {
    pub(crate) fn handle_advice_ops(&mut self, inst: &Instruction) -> Option<PseudocodeTemplate> {
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
}
