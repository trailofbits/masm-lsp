use miden_debug_types::{DefaultSourceManager, SourceManager, SourceSpan, Uri};
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range, Url};

/// Convert a miden diagnostics report into LSP diagnostics.
///
/// Today we fall back to a single coarse diagnostic; future work can surface label spans once exposed.
pub fn diagnostics_from_report(
    sources: &DefaultSourceManager,
    uri: &Url,
    report: miden_assembly_syntax::Report,
) -> Vec<Diagnostic> {
    let _ = sources.get_by_uri(&to_miden_uri(uri));

    vec![Diagnostic {
        range: Range::new(Position::new(0, 0), Position::new(0, 0)),
        severity: Some(DiagnosticSeverity::ERROR),
        message: report.to_string(),
        ..Default::default()
    }]
}

/// Map a byte range in a source file to an LSP range.
#[allow(dead_code)]
pub fn byte_range_to_range(
    source: &miden_debug_types::SourceFile,
    start: usize,
    end: usize,
) -> Option<Range> {
    if start > u32::MAX as usize || end > u32::MAX as usize {
        return None;
    }
    let start = source.location(SourceSpan::at(source.id(), start as u32));
    let end = source.location(SourceSpan::at(source.id(), end as u32));
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

fn to_miden_uri(uri: &Url) -> Uri {
    Uri::new(uri.as_str())
}
