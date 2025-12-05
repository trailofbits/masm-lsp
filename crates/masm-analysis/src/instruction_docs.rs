//! Instruction reference lookup utilities.
//!
//! This module exposes the compile-time generated instruction metadata map and
//! helper lookup functions that can be shared across features (hovers, code
//! lenses, etc.).

use masm_instructions::{semantics_of, InstructionInfo, INSTRUCTION_MAP};
use miden_assembly_syntax::ast::{Instruction, Module, Op};
use miden_assembly_syntax::{Parse, ParseOptions};
use miden_debug_types::DefaultSourceManager;

/// Look up instruction info by name.
///
/// Handles both exact matches (e.g., "add") and parameterized instructions
/// (e.g., "push.1" falling back to "push").
pub fn get_instruction_info(name: &str) -> Option<&'static InstructionInfo> {
    // First try exact match
    if let Some(info) = INSTRUCTION_MAP.get(name) {
        return Some(info);
    }
    // Try base instruction (strip after first dot)
    let base = name.split('.').next()?;
    INSTRUCTION_MAP.get(base)
}

/// Look up hover text for an instruction by name.
///
/// Returns formatted markdown hover text if the instruction is found.
pub fn get_instruction_hover(name: &str) -> Option<String> {
    let info = get_instruction_info(name)?;
    let shapes = generic_shapes(name)
        .unwrap_or_else(|| (info.stack_input.to_string(), info.stack_output.to_string()));
    let desc = instruction_description(name, info);
    Some(format_hover_text(
        name,
        &desc,
        &shapes.0,
        &shapes.1,
        info.cycles,
    ))
}

/// Format instruction info into markdown hover text.
fn format_hover_text(
    name: &str,
    desc: &str,
    input_shape: &str,
    output_shape: &str,
    cycles: &str,
) -> String {
    // Use just the base instruction name for the code block
    let base_name = name.split('.').next().unwrap_or(name);
    format!(
        "```masm\n{}\n```\n\n---\n\n{}\n\n**Stack Input**\n\n`{}`\n\n**Stack Output**\n\n`{}`\n\n**Cycles**\n\n{}",
        base_name, desc, input_shape, output_shape, cycles
    )
}

/// Build generic stack shapes from the authoritative semantics map.
fn generic_shapes(name: &str) -> Option<(String, String)> {
    let inst = parse_instruction(name)?;
    let sem = semantics_of(&inst)?;

    let input = format_shape(sem.pops(), "x");
    let output = format_shape(sem.pushes(), "r");
    Some((input, output))
}

fn format_shape(count: usize, prefix: &str) -> String {
    if count == 0 {
        return "[...]".to_string();
    }
    let mut parts = Vec::new();
    for i in 0..count {
        parts.push(format!("{}{}", prefix, i));
    }
    format!("[{}, ...]", parts.join(", "))
}

/// Return a non-empty description for an instruction, synthesizing one if needed.
pub fn instruction_description(name: &str, info: &InstructionInfo) -> String {
    if !info.description.trim().is_empty() {
        return info.description.to_string();
    }
    let base = name.split('.').next().unwrap_or(name);
    format!("Instruction `{}`.", base)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_instruction_has_description() {
        for (name, info) in INSTRUCTION_MAP.entries() {
            let desc = instruction_description(name, info);
            assert!(
                !desc.trim().is_empty(),
                "description missing for instruction {name}"
            );
        }
    }
}

fn parse_instruction(name: &str) -> Option<Instruction> {
    let inst_text = normalize_instruction_text(name)?;
    let module_src = format!("proc _hover\n    {inst_text}\nend\n");

    let sources = DefaultSourceManager::default();
    let parsed = module_src
        .parse_with_options(&sources, ParseOptions::for_library())
        .ok()?;
    let module: Module = (*parsed).clone();
    let first_proc = module.procedures().next()?;
    match first_proc.body().iter().next()? {
        Op::Inst(span) => Some(span.clone().into_inner()),
        _ => None,
    }
}

/// Adjust doc identifiers so they can be parsed by the MASM parser.
///
/// - Adds placeholder immediates/targets where required.
/// - Replaces `.a` suffixes with `.0`.
fn normalize_instruction_text(name: &str) -> Option<String> {
    let mut text = name.replace(".a", ".0");

    // Add required targets for call-like ops.
    if matches!(text.as_str(), "exec" | "call" | "syscall" | "procref") {
        text.push_str(".foo");
    }

    // Locals require an index.
    if text.starts_with("loc_") && !text.contains('.') {
        text.push_str(".0");
    }

    // Advice push requires a count.
    if text == "adv_push" {
        text.push_str(".1");
    }

    // Push requires a value.
    if text == "push" {
        text.push_str(".0");
    }

    Some(text)
}
