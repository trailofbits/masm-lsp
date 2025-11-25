use std::collections::HashMap;
use std::sync::OnceLock;

use miden_assembly_syntax::ast::visit::{self, Visit};
use miden_assembly_syntax::ast::{Instruction, Module};
use miden_debug_types::{DefaultSourceManager, Span};
use tower_lsp::lsp_types::{InlayHint, InlayHintKind, InlayHintLabel, Position, Range};

use crate::diagnostics::span_to_range;

/// Collect inlay hints for all instructions in a module.
pub fn collect_inlay_hints(
    module: &Module,
    sources: &DefaultSourceManager,
    visible_range: &Range,
    tab_count: usize,
) -> Vec<InlayHint> {
    let mut collector = InstructionCollector::default();
    let _ = visit::visit_module(&mut collector, module);

    let notes = instruction_note_map();

    // First collect the instructions that are visible along with their ranges so we can align
    // all hints on a line to the same column.
    let mut entries = Vec::new();
    for inst in collector.instructions.into_iter() {
        let range = match span_to_range(sources, inst.span()) {
            Some(range) => range,
            None => continue,
        };
        if !position_in_range(&range.end, visible_range) {
            continue;
        }
        entries.push((inst, range));
    }

    // Compute the maximum end column per line to align the hints.
    let mut max_end_per_line: HashMap<u32, u32> = HashMap::new();
    for (_, range) in entries.iter() {
        let end_col = range.end.character;
        max_end_per_line
            .entry(range.end.line)
            .and_modify(|m| *m = (*m).max(end_col))
            .or_insert(end_col);
    }

    entries
        .into_iter()
        .filter_map(|(inst, range)| {
            let base_col = *max_end_per_line
                .get(&range.end.line)
                .unwrap_or(&range.end.character);
            // Leave at least one column after the instruction and add configured padding.
            let padding = (tab_count.max(1)) as u32;
            let align_col = base_col.saturating_add(padding);
            let label_text = render_note(&inst, &notes)?;
            Some(InlayHint {
                position: Position {
                    line: range.end.line,
                    character: align_col,
                },
                label: InlayHintLabel::String(label_text),
                kind: Some(InlayHintKind::TYPE),
                text_edits: None,
                tooltip: None,
                padding_left: Some(true),
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

fn render_note(instruction: &Span<Instruction>, map: &HashMap<String, String>) -> Option<String> {
    let rendered = instruction.inner().to_string();
    let note = if let Some(value) = map.get(&rendered) {
        value.clone()
    } else {
        let base = rendered.split('.').next().unwrap_or(rendered.as_str());
        map.get(base).cloned()?
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

fn instruction_note_map() -> &'static HashMap<String, String> {
    static MAP: OnceLock<HashMap<String, String>> = OnceLock::new();
    MAP.get_or_init(parse_instruction_reference)
}

fn parse_instruction_reference() -> HashMap<String, String> {
    let mut map = HashMap::new();
    let content = include_str!("../data/instruction_reference.md");
    for line in content.lines() {
        let line = line.trim();
        if !line.starts_with('|') {
            continue;
        }
        if line.starts_with("| ---") || line.starts_with("| --------") {
            continue;
        }
        let columns: Vec<_> = line.split('|').map(str::trim).collect();
        if columns.len() < 6 {
            continue;
        }
        let inst_col = columns[1];
        let note_col = columns.get(5).copied().unwrap_or_default();
        if inst_col.eq_ignore_ascii_case("instruction") {
            continue;
        }

        let note = first_sentence(clean_html(note_col));
        if inst_col.is_empty() || note.is_empty() {
            continue;
        }

        for name in inst_col
            .split("<br />")
            .map(str::trim)
            .filter(|s| !s.is_empty())
        {
            let cleaned = clean_instruction_name(name);
            if cleaned.is_empty() || cleaned.starts_with('-') {
                continue;
            }
            map.insert(cleaned.to_string(), note.clone());
        }
    }
    map
}

fn first_sentence(note: String) -> String {
    let trimmed = note.trim();
    if trimmed.is_empty() {
        return String::new();
    }
    if let Some(idx) = trimmed.find('.') {
        return trimmed[..=idx].trim().to_string();
    }
    trimmed.to_string()
}

fn clean_instruction_name(raw: &str) -> String {
    raw.trim_matches('`')
        .replace('*', "")
        .replace("&lt;", "<")
        .replace("&gt;", ">")
        .trim()
        .to_string()
}

fn clean_html(raw: &str) -> String {
    raw.replace("<br />", " ")
        .replace("&lt;", "<")
        .replace("&gt;", ">")
        .replace("$$", "")
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
}
