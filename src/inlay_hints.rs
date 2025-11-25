use miden_assembly_syntax::ast::visit::{self, Visit};
use miden_assembly_syntax::ast::{Instruction, Module};
use miden_debug_types::{DefaultSourceManager, Span};
use tower_lsp::lsp_types::{InlayHint, InlayHintKind, InlayHintLabel, Position, Range};

use crate::diagnostics::span_to_range;

// Include the compile-time generated instruction map
include!(concat!(env!("OUT_DIR"), "/instruction_map.rs"));

/// Collect inlay hints for all instructions in a module.
pub fn collect_inlay_hints(
    module: &Module,
    sources: &DefaultSourceManager,
    visible_range: &Range,
    tab_count: usize,
) -> Vec<InlayHint> {
    let mut collector = InstructionCollector::default();
    let _ = visit::visit_module(&mut collector, module);

    // Fixed padding (in columns) between instruction end and hint.
    let padding = (tab_count.max(1)) as u32;

    collector
        .instructions
        .into_iter()
        .filter_map(|inst| {
            let range = span_to_range(sources, inst.span())?;
            if !position_in_range(&range.end, visible_range) {
                return None;
            }
            let label_text = render_note(&inst)?;
            Some(InlayHint {
                position: Position {
                    line: range.end.line,
                    character: range.end.character.saturating_add(padding),
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

fn render_note(instruction: &Span<Instruction>) -> Option<String> {
    let rendered = instruction.inner().to_string();
    // Try exact match first, then base instruction name
    let note = if let Some(value) = INSTRUCTION_MAP.get(&rendered) {
        *value
    } else {
        let base = rendered.split('.').next().unwrap_or(rendered.as_str());
        *INSTRUCTION_MAP.get(base)?
    };
    // Prefix with a comment marker; alignment is handled via the inlay position.
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

    #[test]
    fn instruction_collector_default() {
        let collector = InstructionCollector::default();
        assert!(collector.instructions.is_empty());
    }
}
