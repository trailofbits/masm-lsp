use masm_instructions::ToStackEffect;
use miden_assembly_syntax::ast::{Block, Instruction, Module, Op};
use miden_debug_types::{DefaultSourceManager, Span};
use tower_lsp::lsp_types::{CodeLens, Command};

use crate::diagnostics::span_to_range;

/// Collect stack-effect code lenses for a module.
pub fn collect_code_lenses(module: &Module, sources: &DefaultSourceManager) -> Vec<CodeLens> {
    let mut lenses = Vec::new();
    let mut instructions: Vec<Span<Instruction>> = Vec::new();
    for proc in module.procedures() {
        collect_instructions(proc.body(), &mut instructions);
    }

    for inst in instructions {
        let Some(range) = span_to_range(sources, inst.span()) else {
            continue;
        };
        let Some(stack_effect) = inst.to_stack_effect() else {
            continue;
        };
        lenses.push(CodeLens {
            range,
            command: Some(Command {
                title: stack_effect,
                command: String::new(),
                arguments: None,
            }),
            data: None,
        });
    }

    lenses
}

fn collect_instructions(block: &Block, out: &mut Vec<Span<Instruction>>) {
    for op in block.iter() {
        match op {
            Op::Inst(inst) => out.push(inst.clone()),
            Op::If {
                then_blk, else_blk, ..
            } => {
                collect_instructions(then_blk, out);
                collect_instructions(else_blk, out);
            }
            Op::While { body, .. } | Op::Repeat { body, .. } => {
                collect_instructions(body, out);
            }
        }
    }
}
