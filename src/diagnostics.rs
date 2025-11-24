use miden_debug_types::{DefaultSourceManager, SourceManager, SourceSpan, Uri};
use miden_utils_diagnostics::{Diagnostic as Midiag, Severity};
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range, Url};

/// Convert a miden diagnostics report into LSP diagnostics.
pub fn diagnostics_from_report(
    sources: &DefaultSourceManager,
    uri: &Url,
    report: miden_assembly_syntax::Report,
) -> Vec<Diagnostic> {
    let Some(source) = sources.get_by_uri(&to_miden_uri(uri)) else {
        return vec![fallback_diag(report.to_string())];
    };

    let diag: &dyn Midiag = report.as_ref();
    let mut out = Vec::new();

    if let Some(labels) = diag.labels() {
        for label in labels {
            let sp = label.inner();
            let range = byte_range_to_range(&source, sp.offset(), sp.len())
                .unwrap_or_else(|| Range::new(Position::new(0, 0), Position::new(0, 0)));
            out.push(Diagnostic {
                range,
                severity: diag
                    .severity()
                    .map(map_severity)
                    .or(Some(DiagnosticSeverity::ERROR)),
                message: diag.to_string(),
                ..Default::default()
            });
        }
    }

    if out.is_empty() {
        out.push(fallback_diag(report.to_string()));
    }

    out
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

pub fn span_to_range(
    sources: &DefaultSourceManager,
    span: SourceSpan,
) -> Option<Range> {
    let range = span.into_range();
    let start = sources.file_line_col(SourceSpan::at(span.source_id(), range.start)).ok()?;
    let end = sources.file_line_col(SourceSpan::at(span.source_id(), range.end)).ok()?;
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

fn map_severity(sev: Severity) -> DiagnosticSeverity {
    match sev {
        Severity::Error => DiagnosticSeverity::ERROR,
        Severity::Warning => DiagnosticSeverity::WARNING,
        Severity::Advice => DiagnosticSeverity::HINT,
    }
}

fn fallback_diag(message: String) -> Diagnostic {
    Diagnostic {
        range: Range::new(Position::new(0, 0), Position::new(0, 0)),
        severity: Some(DiagnosticSeverity::ERROR),
        message,
        ..Default::default()
    }
}
