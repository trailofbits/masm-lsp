use std::collections::HashMap;

use miden_assembly_syntax::ast::visit::{self, Visit};
use miden_assembly_syntax::ast::{Instruction, Module};
use miden_debug_types::{DefaultSourceManager, Span};
use tower_lsp::lsp_types::{InlayHint, InlayHintKind, InlayHintLabel, Position, Range};

use crate::diagnostics::span_to_range;
use crate::instruction_hints::{format_push_immediate, ToInlayHint};

// Include the compile-time generated instruction map
include!(concat!(env!("OUT_DIR"), "/instruction_map.rs"));

/// Data collected for each line: all instructions and the rightmost column position.
struct LineData {
    instructions: Vec<Span<Instruction>>,
    rightmost_col: u32,
}

/// Collect inlay hints for all instructions in a module.
pub fn collect_inlay_hints(
    module: &Module,
    sources: &DefaultSourceManager,
    visible_range: &Range,
    tab_count: usize,
    source_text: &str,
) -> Vec<InlayHint> {
    let mut collector = InstructionCollector::default();
    let _ = visit::visit_module(&mut collector, module);

    // Fixed padding (in columns) between instruction end and hint.
    let padding = (tab_count.max(1)) as u32;

    // Pre-compute line lengths (trimmed of trailing whitespace) for positioning hints
    let line_lengths: Vec<u32> = source_text
        .lines()
        .map(|line| line.trim_end().len() as u32)
        .collect();

    // Group instructions by line. For each line, we keep:
    // - All instructions on that line (needed to aggregate push values like `push.0.0.0.0`)
    // - The rightmost end column (for hint positioning)
    let mut line_hints: HashMap<u32, LineData> = HashMap::new();

    for inst in collector.instructions {
        let Some(range) = span_to_range(sources, inst.span()) else {
            continue;
        };
        if !position_in_range(&range.end, visible_range) {
            continue;
        }
        let line = range.start.line;
        let end_col = range.end.character;

        line_hints
            .entry(line)
            .and_modify(|data| {
                data.instructions.push(inst.clone());
                if end_col > data.rightmost_col {
                    data.rightmost_col = end_col;
                }
            })
            .or_insert(LineData {
                instructions: vec![inst],
                rightmost_col: end_col,
            });
    }

    line_hints
        .into_iter()
        .filter_map(|(line_num, data)| {
            let label_text = render_line_hint(&data.instructions)?;
            // Position hint at the end of the line (after any comments)
            let line_end = line_lengths.get(line_num as usize).copied().unwrap_or(0);
            Some(InlayHint {
                position: Position {
                    line: line_num,
                    character: line_end.saturating_add(padding),
                },
                label: InlayHintLabel::String(label_text),
                kind: Some(InlayHintKind::TYPE),
                text_edits: None,
                tooltip: None,
                padding_left: None,
                padding_right: None,
                data: None,
            })
        })
        .collect()
}

fn position_in_range(pos: &Position, range: &Range) -> bool {
    !is_before(pos, &range.start) && !is_after(pos, &range.end)
}

fn is_before(a: &Position, b: &Position) -> bool {
    a.line < b.line || (a.line == b.line && a.character < b.character)
}

fn is_after(a: &Position, b: &Position) -> bool {
    a.line > b.line || (a.line == b.line && a.character > b.character)
}

/// Generate a hint for all instructions on a line.
/// Aggregates push values when multiple push instructions appear on the same line
/// (e.g., `push.0.0.0.0` is parsed as multiple `push.0` instructions).
fn render_line_hint(instructions: &[Span<Instruction>]) -> Option<String> {
    if instructions.is_empty() {
        return None;
    }

    // Check if all instructions are push instructions that can be aggregated
    if let Some(hint) = try_aggregate_push_hint(instructions) {
        return Some(format!("# {hint}"));
    }

    // Fall back to rendering hint for the first instruction
    render_note(&instructions[0])
}

/// Try to aggregate multiple push instructions into a single hint.
/// Returns None if the instructions cannot be aggregated (not all pushes, or only one push).
fn try_aggregate_push_hint(instructions: &[Span<Instruction>]) -> Option<String> {
    // Need at least 2 instructions to aggregate
    if instructions.len() < 2 {
        return None;
    }

    // Collect all push values
    let mut push_values: Vec<String> = Vec::new();
    for inst in instructions {
        match inst.inner() {
            Instruction::Push(imm) => {
                push_values.push(format_push_immediate(imm));
            }
            Instruction::PushFeltList(values) => {
                for v in values {
                    push_values.push(format!("{v}"));
                }
            }
            // If any instruction is not a push, we can't aggregate
            _ => return None,
        }
    }

    Some(format!(
        "Pushes the field elements [{}] onto the stack.",
        push_values.join(", ")
    ))
}

fn render_note(instruction: &Span<Instruction>) -> Option<String> {
    // Try dynamic hint first using the ToInlayHint trait
    if let Some(hint) = instruction.inner().to_inlay_hint() {
        return Some(format!("# {hint}"));
    }

    // Fall back to static lookup
    let rendered = instruction.inner().to_string();
    let note = if let Some(value) = INSTRUCTION_MAP.get(&rendered) {
        *value
    } else {
        let base = rendered.split('.').next().unwrap_or(rendered.as_str());
        *INSTRUCTION_MAP.get(base)?
    };
    Some(format!("# {note}"))
}

#[derive(Default)]
struct InstructionCollector {
    instructions: Vec<Span<Instruction>>,
}

impl Visit for InstructionCollector {
    fn visit_inst(&mut self, inst: &Span<Instruction>) -> core::ops::ControlFlow<()> {
        self.instructions.push(inst.clone());
        core::ops::ControlFlow::Continue(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use miden_assembly_syntax::ast::Immediate;
    use miden_assembly_syntax::parser::{IntValue, PushValue};
    use miden_assembly_syntax::Felt;

    #[test]
    fn position_comparison_before() {
        let a = Position::new(0, 5);
        let b = Position::new(0, 10);
        assert!(is_before(&a, &b));
        assert!(!is_before(&b, &a));

        let a = Position::new(0, 10);
        let b = Position::new(1, 0);
        assert!(is_before(&a, &b));
    }

    #[test]
    fn position_comparison_after() {
        let a = Position::new(1, 5);
        let b = Position::new(0, 10);
        assert!(is_after(&a, &b));
        assert!(!is_after(&b, &a));

        let a = Position::new(0, 10);
        let b = Position::new(0, 5);
        assert!(is_after(&a, &b));
    }

    #[test]
    fn position_comparison_equal() {
        let a = Position::new(5, 10);
        let b = Position::new(5, 10);
        assert!(!is_before(&a, &b));
        assert!(!is_after(&a, &b));
    }

    #[test]
    fn position_in_range_inside() {
        let pos = Position::new(5, 10);
        let range = Range::new(Position::new(0, 0), Position::new(10, 0));
        assert!(position_in_range(&pos, &range));
    }

    #[test]
    fn position_in_range_at_start() {
        let pos = Position::new(0, 0);
        let range = Range::new(Position::new(0, 0), Position::new(10, 0));
        assert!(position_in_range(&pos, &range));
    }

    #[test]
    fn position_in_range_at_end() {
        let pos = Position::new(10, 0);
        let range = Range::new(Position::new(0, 0), Position::new(10, 0));
        assert!(position_in_range(&pos, &range));
    }

    #[test]
    fn position_in_range_before() {
        let pos = Position::new(0, 0);
        let range = Range::new(Position::new(5, 0), Position::new(10, 0));
        assert!(!position_in_range(&pos, &range));
    }

    #[test]
    fn position_in_range_after() {
        let pos = Position::new(15, 0);
        let range = Range::new(Position::new(5, 0), Position::new(10, 0));
        assert!(!position_in_range(&pos, &range));
    }

    #[test]
    fn instruction_map_contains_common_instructions() {
        // Test that common instructions are in the map
        assert!(INSTRUCTION_MAP.contains_key("add"));
        assert!(INSTRUCTION_MAP.contains_key("push"));
        assert!(INSTRUCTION_MAP.contains_key("nop"));
        assert!(INSTRUCTION_MAP.contains_key("dup"));
    }

    #[test]
    fn instruction_map_lookup_returns_description() {
        let desc = INSTRUCTION_MAP.get("add");
        assert!(desc.is_some());
        let desc_str = *desc.unwrap();
        assert!(!desc_str.is_empty());
    }

    // === Tests for push aggregation ===

    #[test]
    fn aggregate_push_multiple_values() {
        // Simulate `push.0.0.0.0` which gets parsed as 4 separate Push instructions
        let instructions: Vec<Span<Instruction>> = vec![
            Span::unknown(Instruction::Push(Immediate::from(Span::unknown(
                PushValue::Int(IntValue::U8(0)),
            )))),
            Span::unknown(Instruction::Push(Immediate::from(Span::unknown(
                PushValue::Int(IntValue::U8(0)),
            )))),
            Span::unknown(Instruction::Push(Immediate::from(Span::unknown(
                PushValue::Int(IntValue::U8(0)),
            )))),
            Span::unknown(Instruction::Push(Immediate::from(Span::unknown(
                PushValue::Int(IntValue::U8(0)),
            )))),
        ];

        let hint = try_aggregate_push_hint(&instructions);
        assert_eq!(
            hint,
            Some("Pushes the field elements [0, 0, 0, 0] onto the stack.".into())
        );
    }

    #[test]
    fn aggregate_push_two_values() {
        // Simulate `push.1.2`
        let instructions: Vec<Span<Instruction>> = vec![
            Span::unknown(Instruction::Push(Immediate::from(Span::unknown(
                PushValue::Int(IntValue::U8(1)),
            )))),
            Span::unknown(Instruction::Push(Immediate::from(Span::unknown(
                PushValue::Int(IntValue::U8(2)),
            )))),
        ];

        let hint = try_aggregate_push_hint(&instructions);
        assert_eq!(
            hint,
            Some("Pushes the field elements [1, 2] onto the stack.".into())
        );
    }

    #[test]
    fn aggregate_push_single_instruction_no_aggregation() {
        // Single push should not be aggregated (use dynamic hint instead)
        let instructions: Vec<Span<Instruction>> = vec![Span::unknown(Instruction::Push(
            Immediate::from(Span::unknown(PushValue::Int(IntValue::U8(42)))),
        ))];

        let hint = try_aggregate_push_hint(&instructions);
        assert_eq!(hint, None);
    }

    #[test]
    fn aggregate_push_mixed_instructions_no_aggregation() {
        // Mixed push and non-push instructions should not be aggregated
        let instructions: Vec<Span<Instruction>> = vec![
            Span::unknown(Instruction::Push(Immediate::from(Span::unknown(
                PushValue::Int(IntValue::U8(1)),
            )))),
            Span::unknown(Instruction::Add),
        ];

        let hint = try_aggregate_push_hint(&instructions);
        assert_eq!(hint, None);
    }

    #[test]
    fn render_line_hint_aggregates_pushes() {
        let instructions: Vec<Span<Instruction>> = vec![
            Span::unknown(Instruction::Push(Immediate::from(Span::unknown(
                PushValue::Int(IntValue::U8(1)),
            )))),
            Span::unknown(Instruction::Push(Immediate::from(Span::unknown(
                PushValue::Int(IntValue::U8(2)),
            )))),
            Span::unknown(Instruction::Push(Immediate::from(Span::unknown(
                PushValue::Int(IntValue::U8(3)),
            )))),
        ];

        let hint = render_line_hint(&instructions);
        assert_eq!(
            hint,
            Some("# Pushes the field elements [1, 2, 3] onto the stack.".into())
        );
    }

    #[test]
    fn render_line_hint_single_instruction_uses_trait() {
        let instructions: Vec<Span<Instruction>> = vec![Span::unknown(Instruction::MovUp5)];

        let hint = render_line_hint(&instructions);
        assert_eq!(hint, Some("# Moves the 5th stack item to the top.".into()));
    }

    #[test]
    fn render_line_hint_empty_returns_none() {
        let instructions: Vec<Span<Instruction>> = vec![];
        let hint = render_line_hint(&instructions);
        assert_eq!(hint, None);
    }

    #[test]
    fn render_note_uses_trait_for_dynamic_hints() {
        let inst = Span::unknown(Instruction::Dup5);
        let hint = render_note(&inst);
        assert_eq!(hint, Some("# Pushes a copy of the 5th stack item.".into()));
    }

    #[test]
    fn render_note_falls_back_to_static_map() {
        let inst = Span::unknown(Instruction::Add);
        let hint = render_note(&inst);
        // Should get the static hint from INSTRUCTION_MAP
        assert!(hint.is_some());
        assert!(hint.unwrap().starts_with("# "));
    }

    #[test]
    fn render_note_push_felt_list() {
        let values = vec![Felt::new(1), Felt::new(2)];
        let inst = Span::unknown(Instruction::PushFeltList(values));
        let hint = render_note(&inst);
        assert_eq!(
            hint,
            Some("# Pushes the field elements [1, 2] onto the stack.".into())
        );
    }
}
