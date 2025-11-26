use std::collections::HashMap;

use miden_assembly_syntax::ast::visit::{self, Visit};
use miden_assembly_syntax::ast::{Instruction, InvocationTarget, Module};
use miden_debug_types::{DefaultSourceManager, Span, Spanned};
use tower_lsp::lsp_types::{InlayHint, InlayHintKind, InlayHintLabel, Position, Range};

use crate::diagnostics::span_to_range;
use crate::instruction_hints::{format_push_immediate, ToInlayHint};

// Include the compile-time generated instruction map
include!(concat!(env!("OUT_DIR"), "/instruction_map.rs"));

/// Look up instruction info by name.
///
/// Handles both exact matches (e.g., "add") and parameterized instructions
/// (e.g., "push.1" falling back to "push").
fn get_instruction_info(name: &str) -> Option<&'static InstructionInfo> {
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

/// The kind of invocation instruction.
#[derive(Clone, Copy)]
enum InvocationKind {
    Exec,
    Call,
    Syscall,
    Procref,
}

/// An invocation with its kind and target.
#[derive(Clone)]
struct Invocation {
    kind: InvocationKind,
    target: InvocationTarget,
}

/// Data collected for each line: all instructions/invocations and the rightmost column position.
struct LineData {
    instructions: Vec<Span<Instruction>>,
    invocations: Vec<Invocation>,
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

    // Group instructions and invocations by line. For each line, we keep:
    // - All instructions on that line (needed to aggregate push values like `push.0.0.0.0`)
    // - All invocations on that line (exec, call, syscall, procref)
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
                invocations: vec![],
                rightmost_col: end_col,
            });
    }

    // Process invocations
    for invocation in collector.invocations {
        let Some(range) = span_to_range(sources, invocation.target.span()) else {
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
                data.invocations.push(invocation.clone());
                if end_col > data.rightmost_col {
                    data.rightmost_col = end_col;
                }
            })
            .or_insert(LineData {
                instructions: vec![],
                invocations: vec![invocation],
                rightmost_col: end_col,
            });
    }

    let mut hints: Vec<InlayHint> = line_hints
        .into_iter()
        .flat_map(|(line_num, data)| {
            // Position hints at the end of the line (after any comments)
            let line_end = line_lengths.get(line_num as usize).copied().unwrap_or(0);

            // Generate hints for instructions
            let inst_hints = render_line_hints(&data.instructions);

            // Generate hints for invocations
            let invoc_hints: Vec<String> = data
                .invocations
                .iter()
                .map(|inv| render_invocation_hint(inv))
                .collect();

            inst_hints
                .into_iter()
                .chain(invoc_hints)
                .map(move |label_text| InlayHint {
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
        .collect();

    // Sort hints by position (line, then character) to ensure correct display order
    hints.sort_by(|a, b| {
        a.position
            .line
            .cmp(&b.position.line)
            .then_with(|| a.position.character.cmp(&b.position.character))
    });

    hints
}

/// Generate a hint for an invocation (exec, call, syscall, procref).
fn render_invocation_hint(invocation: &Invocation) -> String {
    let target_name = match &invocation.target {
        InvocationTarget::Symbol(ident) => ident.as_str().to_string(),
        InvocationTarget::Path(path) => path.inner().as_str().to_string(),
        InvocationTarget::MastRoot(root) => format!("0x{}", root.inner()),
    };

    match invocation.kind {
        InvocationKind::Exec => format!("Executes {target_name}."),
        InvocationKind::Call => format!("Calls {target_name}."),
        InvocationKind::Syscall => format!("Syscall to {target_name}."),
        InvocationKind::Procref => format!("Reference to {target_name}."),
    }
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

/// Generate hints for all instructions on a line.
/// Aggregates consecutive push instructions (e.g., `push.1.2` parsed as multiple pushes).
/// For non-push instructions (e.g., `add swap`), returns a hint for each.
fn render_line_hints(instructions: &[Span<Instruction>]) -> Vec<String> {
    if instructions.is_empty() {
        return vec![];
    }

    let mut hints = Vec::new();
    let mut i = 0;

    while i < instructions.len() {
        if is_push_instruction(instructions[i].inner()) {
            // Collect consecutive push instructions
            let start = i;
            while i < instructions.len() && is_push_instruction(instructions[i].inner()) {
                i += 1;
            }
            let push_group = &instructions[start..i];

            if push_group.len() >= 2 {
                // Aggregate multiple consecutive pushes
                if let Some(hint) = try_aggregate_push_hint(push_group) {
                    hints.push(hint);
                }
            } else {
                // Single push - render individually
                if let Some(hint) = render_note(&push_group[0]) {
                    hints.push(hint);
                }
            }
        } else {
            // Non-push instruction - render individually
            if let Some(hint) = render_note(&instructions[i]) {
                hints.push(hint);
            }
            i += 1;
        }
    }

    hints
}

/// Check if an instruction is a push instruction (Push or PushFeltList).
fn is_push_instruction(inst: &Instruction) -> bool {
    matches!(inst, Instruction::Push(_) | Instruction::PushFeltList(_))
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
        return Some(hint);
    }

    // Fall back to static lookup - use description for inlay hints
    let rendered = instruction.inner().to_string();
    let info = get_instruction_info(&rendered)?;
    Some(info.description.to_string())
}

#[derive(Default)]
struct InstructionCollector {
    instructions: Vec<Span<Instruction>>,
    invocations: Vec<Invocation>,
}

impl Visit for InstructionCollector {
    fn visit_inst(&mut self, inst: &Span<Instruction>) -> core::ops::ControlFlow<()> {
        self.instructions.push(inst.clone());
        // Continue default traversal to visit exec/call/syscall/procref targets
        visit::visit_inst(self, inst)
    }

    fn visit_exec(&mut self, target: &InvocationTarget) -> core::ops::ControlFlow<()> {
        self.invocations.push(Invocation {
            kind: InvocationKind::Exec,
            target: target.clone(),
        });
        core::ops::ControlFlow::Continue(())
    }

    fn visit_call(&mut self, target: &InvocationTarget) -> core::ops::ControlFlow<()> {
        self.invocations.push(Invocation {
            kind: InvocationKind::Call,
            target: target.clone(),
        });
        core::ops::ControlFlow::Continue(())
    }

    fn visit_syscall(&mut self, target: &InvocationTarget) -> core::ops::ControlFlow<()> {
        self.invocations.push(Invocation {
            kind: InvocationKind::Syscall,
            target: target.clone(),
        });
        core::ops::ControlFlow::Continue(())
    }

    fn visit_procref(&mut self, target: &InvocationTarget) -> core::ops::ControlFlow<()> {
        self.invocations.push(Invocation {
            kind: InvocationKind::Procref,
            target: target.clone(),
        });
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
    fn instruction_map_lookup_returns_info() {
        let info = INSTRUCTION_MAP.get("add");
        assert!(info.is_some());
        let info = info.unwrap();
        assert!(!info.description.is_empty());
        assert!(!info.stack_input.is_empty());
        assert!(!info.stack_output.is_empty());
        assert!(!info.cycles.is_empty());
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
    fn render_line_hints_aggregates_pushes() {
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

        let hints = render_line_hints(&instructions);
        assert_eq!(
            hints,
            vec!["Pushes the field elements [1, 2, 3] onto the stack."]
        );
    }

    #[test]
    fn render_line_hints_single_instruction_uses_trait() {
        let instructions: Vec<Span<Instruction>> = vec![Span::unknown(Instruction::MovUp5)];

        let hints = render_line_hints(&instructions);
        assert_eq!(hints, vec!["Moves the 5th stack item to the top."]);
    }

    #[test]
    fn render_line_hints_empty_returns_empty() {
        let instructions: Vec<Span<Instruction>> = vec![];
        let hints = render_line_hints(&instructions);
        assert!(hints.is_empty());
    }

    #[test]
    fn render_line_hints_multiple_non_push_instructions() {
        // Test multiple instructions on the same line like "add swap"
        let instructions: Vec<Span<Instruction>> = vec![
            Span::unknown(Instruction::Add),
            Span::unknown(Instruction::Swap1),
        ];

        let hints = render_line_hints(&instructions);
        assert_eq!(hints.len(), 2);
        // First hint is for Add: "c = (a + b) mod p."
        assert!(hints[0].contains("mod p"));
        // Second hint is for Swap: "Swaps the top two stack items."
        assert!(hints[1].contains("Swaps"));
    }

    #[test]
    fn render_line_hints_single_push_and_non_push() {
        // Test "push.1 add" - single push followed by non-push, should generate individual hints
        let instructions: Vec<Span<Instruction>> = vec![
            Span::unknown(Instruction::Push(Immediate::from(Span::unknown(
                PushValue::Int(IntValue::U8(1)),
            )))),
            Span::unknown(Instruction::Add),
        ];

        let hints = render_line_hints(&instructions);
        assert_eq!(hints.len(), 2);
        // First hint is for Push: "Pushes 1 onto the stack."
        assert!(hints[0].contains("Pushes"));
        // Second hint is for Add: "c = (a + b) mod p."
        assert!(hints[1].contains("mod p"));
    }

    #[test]
    fn render_line_hints_multi_push_then_non_push() {
        // Test "push.1.2 add" - multiple pushes (from push.1.2) followed by add
        // The consecutive pushes should be aggregated, add gets its own hint
        let instructions: Vec<Span<Instruction>> = vec![
            Span::unknown(Instruction::Push(Immediate::from(Span::unknown(
                PushValue::Int(IntValue::U8(1)),
            )))),
            Span::unknown(Instruction::Push(Immediate::from(Span::unknown(
                PushValue::Int(IntValue::U8(2)),
            )))),
            Span::unknown(Instruction::Add),
        ];

        let hints = render_line_hints(&instructions);
        assert_eq!(hints.len(), 2);
        // First hint is aggregated push: "Pushes the field elements [1, 2] onto the stack."
        assert!(hints[0].contains("field elements"));
        assert!(hints[0].contains("[1, 2]"));
        // Second hint is for Add: "c = (a + b) mod p."
        assert!(hints[1].contains("mod p"));
    }

    #[test]
    fn render_note_uses_trait_for_dynamic_hints() {
        let inst = Span::unknown(Instruction::Dup5);
        let hint = render_note(&inst);
        assert_eq!(hint, Some("Pushes a copy of the 5th stack item.".into()));
    }

    #[test]
    fn render_note_falls_back_to_static_map() {
        let inst = Span::unknown(Instruction::Add);
        let hint = render_note(&inst);
        // Should get the static hint from INSTRUCTION_MAP
        assert!(hint.is_some());
        assert!(!hint.unwrap().is_empty());
    }

    #[test]
    fn render_note_push_felt_list() {
        let values = vec![Felt::new(1), Felt::new(2)];
        let inst = Span::unknown(Instruction::PushFeltList(values));
        let hint = render_note(&inst);
        assert_eq!(
            hint,
            Some("Pushes the field elements [1, 2] onto the stack.".into())
        );
    }

    #[test]
    fn render_invocation_hint_exec() {
        use miden_assembly_syntax::ast::Ident;

        let target = InvocationTarget::Symbol(Ident::new("helper").unwrap());
        let invocation = Invocation {
            kind: InvocationKind::Exec,
            target,
        };
        let hint = render_invocation_hint(&invocation);
        assert_eq!(hint, "Executes helper.");
    }

    #[test]
    fn render_invocation_hint_call() {
        use miden_assembly_syntax::ast::Ident;

        let target = InvocationTarget::Symbol(Ident::new("foo").unwrap());
        let invocation = Invocation {
            kind: InvocationKind::Call,
            target,
        };
        let hint = render_invocation_hint(&invocation);
        assert_eq!(hint, "Calls foo.");
    }

    #[test]
    fn render_invocation_hint_syscall() {
        use miden_assembly_syntax::ast::Ident;

        let target = InvocationTarget::Symbol(Ident::new("kernel_func").unwrap());
        let invocation = Invocation {
            kind: InvocationKind::Syscall,
            target,
        };
        let hint = render_invocation_hint(&invocation);
        assert_eq!(hint, "Syscall to kernel_func.");
    }

    #[test]
    fn render_invocation_hint_procref() {
        use miden_assembly_syntax::ast::Ident;

        let target = InvocationTarget::Symbol(Ident::new("my_proc").unwrap());
        let invocation = Invocation {
            kind: InvocationKind::Procref,
            target,
        };
        let hint = render_invocation_hint(&invocation);
        assert_eq!(hint, "Reference to my_proc.");
    }
}
