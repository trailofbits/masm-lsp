use miden_debug_types::{Position as MidenPosition, Selection};
use tower_lsp::lsp_types::{Range, Url};

use miden_assembly_syntax::ast::ModuleKind;

/// Convert an LSP range to a Miden selection.
pub fn lsp_range_to_selection(range: Range) -> Selection {
    Selection::new(
        MidenPosition::new(range.start.line, range.start.character),
        MidenPosition::new(range.end.line, range.end.character),
    )
}

/// Heuristic module kind detection based on filename and presence of `begin`.
pub fn guess_module_kinds(uri: &Url, text: &str) -> Vec<ModuleKind> {
    let mut kinds = Vec::new();
    let path = uri.path().to_ascii_lowercase();
    let has_begin = text.lines().any(|line| line.trim_start().starts_with("begin"));

    if path.contains("kernel") {
        push_if_missing(&mut kinds, ModuleKind::Kernel);
    }
    if has_begin {
        push_if_missing(&mut kinds, ModuleKind::Executable);
    }
    push_if_missing(&mut kinds, ModuleKind::Library);
    if !has_begin {
        push_if_missing(&mut kinds, ModuleKind::Executable);
    }

    kinds
}

fn push_if_missing(kinds: &mut Vec<ModuleKind>, kind: ModuleKind) {
    if !kinds.contains(&kind) {
        kinds.push(kind);
    }
}
