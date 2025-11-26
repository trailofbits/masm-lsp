use miden_debug_types::{Position as MidenPosition, Selection, SourceFile, Uri};
use tower_lsp::lsp_types::{Position, Range, Url};

/// Convert an LSP range to a Miden selection.
pub fn lsp_range_to_selection(range: Range) -> Selection {
    Selection::new(
        MidenPosition::new(range.start.line, range.start.character),
        MidenPosition::new(range.end.line, range.end.character),
    )
}

/// Extract a MASM identifier-like token at the given LSP position.
pub fn extract_token_at_position(source: &SourceFile, pos: Position) -> Option<String> {
    let bytes = source.as_bytes();
    let offset = byte_offset_from_position(source, pos)?;
    let mut start = offset;
    let mut end = offset;

    while start > 0 && is_ident_char(bytes[start - 1] as char) {
        start -= 1;
    }
    while end < bytes.len() && is_ident_char(bytes[end] as char) {
        end += 1;
    }
    let slice = std::str::from_utf8(&bytes[start..end]).ok()?;
    if slice.is_empty() {
        None
    } else {
        Some(slice.to_string())
    }
}

fn is_ident_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || matches!(c, '_' | ':' | '$' | '.')
}

fn byte_offset_from_position(source: &SourceFile, pos: Position) -> Option<usize> {
    let mut offset = 0usize;
    for (i, line) in source.as_str().split_inclusive('\n').enumerate() {
        if i as u32 == pos.line {
            if (pos.character as usize) <= line.len() {
                offset += pos.character as usize;
                return Some(offset);
            } else {
                return None;
            }
        }
        offset += line.len();
    }
    None
}

/// Convert an LSP URL to a Miden URI.
pub fn to_miden_uri(uri: &Url) -> Uri {
    Uri::new(uri.as_str())
}
