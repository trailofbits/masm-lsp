use miden_assembly_syntax::ast::Instruction;
use miden_debug_types::SourceSpan;

use crate::collector::pseudocode_collector::PseudocodeCollector;
use crate::ssa::{PseudocodeBuilder, PseudocodeTemplate};
use masm_analysis::call_effect::{resolve_call_effect, CallEffect};

impl<'a> PseudocodeCollector<'a> {
    pub(crate) fn handle_memory_stream_op(
        &mut self,
        inst: &Instruction,
    ) -> Option<PseudocodeTemplate> {
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

    pub(crate) fn handle_proc_ref(&mut self, inst: &Instruction) -> Option<PseudocodeTemplate> {
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
                out.text(") = procref(");
                out.text(&target.to_string());
                out.text(")");
                Some(out.build())
            }
            _ => None,
        }
    }

    pub(crate) fn handle_calls(
        &mut self,
        inst: &Instruction,
        span: SourceSpan,
    ) -> Option<PseudocodeTemplate> {
        let state = self.state.as_mut()?;

        match inst {
            Instruction::Exec(target)
            | Instruction::Call(target)
            | Instruction::SysCall(target) => {
                match resolve_call_effect(Some(&self.resolver), self.contracts, target) {
                    CallEffect::Known { inputs, outputs } => {
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
                    CallEffect::KnownInputs { inputs } => {
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
                    CallEffect::Unknown => {
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

    pub(crate) fn handle_dynamic_calls(
        &mut self,
        inst: &Instruction,
        span: SourceSpan,
    ) -> Option<PseudocodeTemplate> {
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
}
