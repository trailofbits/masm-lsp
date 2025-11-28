use crate::util::to_miden_uri;
use miden_debug_types::{DefaultSourceManager, SourceManager, SourceSpan};
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

    // When the parser provides no labels, show a generic parse error.
    // Unresolved invocations are tracked separately via AST traversal in index.rs.
    if out.is_empty() {
        out.push(fallback_diag(report.to_string()));
    }

    out
}

/// Compute unresolved reference diagnostics on-demand.
///
/// This iterates over all references in the document and checks if each can be
/// resolved via the workspace index. References that cannot be found are reported
/// as unresolved invocation targets.
pub fn unresolved_to_diagnostics(
    _uri: &Url,
    doc: &crate::index::DocumentSymbols,
    workspace: &crate::index::WorkspaceIndex,
) -> Vec<Diagnostic> {
    doc.references
        .iter()
        .filter(|reference| {
            let path = reference.path.as_str();
            let name = reference.path.name();

            // Check if this reference can be resolved via the workspace index
            // using the same logic as goto_definition
            let can_resolve = workspace.definition(path).is_some()
                || workspace.definition_by_suffix(path).is_some()
                || workspace.definition_by_name(name).is_some();

            !can_resolve // Only keep unresolved references
        })
        .map(|reference| Diagnostic {
            range: reference.range,
            severity: Some(DiagnosticSeverity::ERROR),
            message: format!("unresolved invocation target `{}`", reference.path.name()),
            ..Default::default()
        })
        .collect()
}

/// Map a byte range in a source file to an LSP range.
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

pub fn span_to_range(sources: &DefaultSourceManager, span: SourceSpan) -> Option<Range> {
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
