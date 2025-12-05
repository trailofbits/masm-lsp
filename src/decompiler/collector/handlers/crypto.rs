use miden_assembly_syntax::ast::Instruction;

use crate::decompiler::collector::pseudocode_collector::PseudocodeCollector;
use crate::decompiler::ssa::{PseudocodeBuilder, PseudocodeTemplate};

impl<'a> PseudocodeCollector<'a> {
    pub(crate) fn handle_crypto_ops(&mut self, inst: &Instruction) -> Option<PseudocodeTemplate> {
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

    pub(crate) fn handle_merkle_ops(&mut self, inst: &Instruction) -> Option<PseudocodeTemplate> {
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

    pub(crate) fn handle_complex_stark_ops(
        &mut self,
        inst: &Instruction,
    ) -> Option<PseudocodeTemplate> {
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

                for _ in 0..16 {
                    let var = state.new_local();
                    state.push(var);
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
}
