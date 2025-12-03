//! Instruction reference lookup utilities.
//!
//! This module exposes the compile-time generated instruction metadata map and
//! helper lookup functions that can be shared across features (hovers, code
//! lenses, etc.).

// Include the compile-time generated instruction map
include!(concat!(env!("OUT_DIR"), "/instruction_map.rs"));

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
    Some(format_hover_text(name, info))
}

/// Format instruction info into markdown hover text.
fn format_hover_text(name: &str, info: &InstructionInfo) -> String {
    // Use just the base instruction name for the code block
    let base_name = name.split('.').next().unwrap_or(name);
    format!(
        "```masm\n{}\n```\n\n---\n\n{}\n\n**Stack Input**\n\n`{}`\n\n**Stack Output**\n\n`{}`\n\n**Cycles**\n\n{}",
        base_name, info.description, info.stack_input, info.stack_output, info.cycles
    )
}
