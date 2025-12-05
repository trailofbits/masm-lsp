use miden_debug_types::{DefaultSourceManager, SourceManager, SourceSpan};
use tower_lsp::lsp_types::{Position, Range};

pub(crate) fn span_to_range(sources: &DefaultSourceManager, span: SourceSpan) -> Option<Range> {
    let range = span.into_range();
    let start = sources
        .file_line_col(SourceSpan::at(span.source_id(), range.start))
        .ok()?;
    let end = sources
        .file_line_col(SourceSpan::at(span.source_id(), range.end))
        .ok()?;
    Some(Range::new(
        Position::new(
            start.line.to_usize().saturating_sub(1) as u32,
            start.column.to_usize().saturating_sub(1) as u32,
        ),
        Position::new(
            end.line.to_usize().saturating_sub(1) as u32,
            end.column.to_usize().saturating_sub(1) as u32,
        ),
    ))
}
