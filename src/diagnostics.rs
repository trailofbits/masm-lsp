use crate::util::to_miden_uri;
use miden_debug_types::{DefaultSourceManager, SourceManager, SourceSpan};
use miden_utils_diagnostics::{Diagnostic as Midiag, Severity};
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range, Url};

/// Diagnostic source for syntax errors (parsing, unresolved references).
pub const SOURCE_SYNTAX: &str = "masm-lsp/syntax";

/// Diagnostic source for analysis findings (taint analysis, uninitialized locals, etc.).
pub const SOURCE_ANALYSIS: &str = "masm-lsp/analysis";

/// Diagnostic source for decompilation failures.
/// Note: The VS Code extension depends on this exact value.
pub const SOURCE_DECOMPILATION: &str = "masm-lsp/decompilation";

/// Normalize a diagnostic message to title case and ensure it ends with a single period.
pub fn normalize_message(msg: &str) -> String {
    let msg = msg.trim();
    if msg.is_empty() {
        return String::new();
    }

    // Make first character uppercase
    let mut chars = msg.chars();
    let first = chars.next().unwrap().to_uppercase().to_string();
    let rest: String = chars.collect();
    let mut result = first + &rest;

    // Ensure exactly one trailing period
    while result.ends_with('.') {
        result.pop();
    }
    result.push('.');

    result
}

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

    // The main diagnostic message often contains the best error description
    let main_message = diag.to_string();

    // Try to get help text which may contain more useful information
    let help_text = diag.help().map(|h| h.to_string());

    if let Some(labels) = diag.labels() {
        for label in labels {
            let sp = label.inner();
            let range = byte_range_to_range(&source, sp.offset(), sp.offset() + sp.len())
                .unwrap_or_else(|| Range::new(Position::new(0, 0), Position::new(0, 0)));

            let label_msg = label.label().map(String::from);

            // Build the best possible error message from available information
            let raw_message = build_error_message(
                label_msg.as_deref(),
                &main_message,
                help_text.as_deref(),
                &source,
                sp.offset(),
            );
            let message = normalize_message(&raw_message);
            out.push(Diagnostic {
                range,
                severity: diag
                    .severity()
                    .map(map_severity)
                    .or(Some(DiagnosticSeverity::ERROR)),
                source: Some(SOURCE_SYNTAX.to_string()),
                message,
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

/// Build a better error message from available diagnostic information.
fn build_error_message(
    label_msg: Option<&str>,
    main_message: &str,
    help_text: Option<&str>,
    source: &miden_debug_types::SourceFile,
    offset: usize,
) -> String {
    // Check if label message is unhelpful (LALRPOP-generated)
    let label_is_unhelpful = label_msg
        .map(is_unhelpful_label_message)
        .unwrap_or(true);

    // If we have a helpful label message, use it
    if !label_is_unhelpful {
        if let Some(msg) = label_msg {
            return msg.to_string();
        }
    }

    // Try to extract token information from the unhelpful message
    let found_token = label_msg.and_then(extract_found_token);

    // The help text contains what was expected - this is the most useful info
    if let Some(help) = help_text {
        // Simplify the expected tokens list if it's too verbose
        let simplified_expected = simplify_expected_tokens(help);

        // Build a message with what was found and what was expected
        if let Some(ref token) = found_token {
            // Try to get the actual text at the error location
            if let Some(actual_text) = get_text_at_offset(source, offset) {
                if actual_text != *token && !actual_text.is_empty() {
                    return format!(
                        "Unexpected '{}'; {}",
                        actual_text, simplified_expected
                    );
                }
            }
            return format!("Unexpected {}; {}", token, simplified_expected);
        } else {
            return simplified_expected;
        }
    }

    // If main message is informative, use it
    let main_lower = main_message.to_lowercase();
    if main_lower != "invalid syntax" && !main_lower.is_empty() {
        // Capitalize and clean up the main message
        return main_message.to_string();
    }

    // Fall back to context-based message
    if let Some(context_msg) = get_context_error_message(source, offset, found_token.as_deref()) {
        return context_msg;
    }

    // Last resort
    if let Some(token) = found_token {
        return format!("Unexpected {}", token);
    }

    "Syntax error".to_string()
}

/// Simplify the expected tokens list to be more readable.
fn simplify_expected_tokens(help: &str) -> String {
    // The help text looks like: expected "X", or Y, or Z
    let help = help.trim();

    // If it's already short, return as-is
    if help.len() < 80 {
        return capitalize_first(help);
    }

    // Count the number of alternatives
    let alt_count = help.matches(", or ").count() + 1;

    // If there are many alternatives, summarize them
    if alt_count > 4 {
        // Extract the first few alternatives
        let mut parts: Vec<&str> = help.split(", or ").take(3).collect();

        // Clean up "expected" prefix if present
        if let Some(first) = parts.first_mut() {
            if first.starts_with("expected ") {
                *first = &first[9..];
            }
        }

        let remaining = alt_count - 3;
        return format!(
            "Expected {}, or {} other options",
            parts.join(", "),
            remaining
        );
    }

    capitalize_first(help)
}

/// Capitalize the first character of a string.
fn capitalize_first(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        Some(c) => c.to_uppercase().collect::<String>() + chars.as_str(),
        None => String::new(),
    }
}

/// Get the text at a specific offset in the source.
fn get_text_at_offset(source: &miden_debug_types::SourceFile, offset: usize) -> Option<String> {
    let content = source.as_str();
    if offset >= content.len() {
        return None;
    }

    // Find word boundaries
    let word_start = content[..offset]
        .rfind(|c: char| c.is_whitespace() || c == '.' || c == '(' || c == ')')
        .map(|i| i + 1)
        .unwrap_or(0);
    let word_end = content[offset..]
        .find(|c: char| c.is_whitespace() || c == '.' || c == '(' || c == ')')
        .map(|i| offset + i)
        .unwrap_or(content.len());

    let word = content[word_start..word_end].trim();
    if word.is_empty() || word.len() > 30 {
        return None;
    }

    Some(word.to_string())
}

/// Check if a label message is one of the unhelpful LALRPOP-generated messages.
fn is_unhelpful_label_message(msg: &str) -> bool {
    let msg_lower = msg.to_lowercase();
    // LALRPOP generates messages like "found a X here" or "found an X here"
    msg_lower.starts_with("found a ") || msg_lower.starts_with("found an ")
}

/// Extract the token name from a "found a X here" message.
fn extract_found_token(msg: &str) -> Option<String> {
    let msg_lower = msg.to_lowercase();
    if msg_lower.starts_with("found a ") {
        let rest = &msg[8..]; // Skip "found a "
        if let Some(idx) = rest.find(" here") {
            return Some(rest[..idx].to_string());
        }
    } else if msg_lower.starts_with("found an ") {
        let rest = &msg[9..]; // Skip "found an "
        if let Some(idx) = rest.find(" here") {
            return Some(rest[..idx].to_string());
        }
    }
    None
}

/// Try to get a context-aware error message based on source location.
fn get_context_error_message(
    source: &miden_debug_types::SourceFile,
    offset: usize,
    found_token: Option<&str>,
) -> Option<String> {
    let content = source.as_str();
    if offset >= content.len() {
        return Some("Unexpected end of file".to_string());
    }

    // Find the line containing the error
    let line_start = content[..offset].rfind('\n').map(|i| i + 1).unwrap_or(0);
    let line_end = content[offset..]
        .find('\n')
        .map(|i| offset + i)
        .unwrap_or(content.len());
    let line = content[line_start..line_end].trim();

    // Try to determine context from the line content
    if line.starts_with("proc") && found_token == Some("nop") {
        return Some("Expected procedure name after 'proc'".to_string());
    }
    if line.starts_with("proc") && found_token == Some("identifier") {
        // Check if it looks like an unknown instruction
        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.len() >= 2 {
            return Some(format!("Unknown instruction or invalid syntax: '{}'", parts[1]));
        }
    }
    if found_token == Some("identifier") {
        // Try to extract the actual identifier
        let word_start = content[..offset]
            .rfind(|c: char| c.is_whitespace())
            .map(|i| i + 1)
            .unwrap_or(offset);
        let word_end = content[offset..]
            .find(|c: char| c.is_whitespace())
            .map(|i| offset + i)
            .unwrap_or(content.len().min(offset + 20));
        let word = content[word_start..word_end].trim();
        if !word.is_empty() && word.len() < 50 {
            return Some(format!("Unknown instruction or symbol '{}'", word));
        }
    }

    None
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
            source: Some(SOURCE_SYNTAX.to_string()),
            message: format!("Unresolved invocation target `{}`.", reference.path.name()),
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
        source: Some(SOURCE_SYNTAX.to_string()),
        message: normalize_message(&message),
        ..Default::default()
    }
}
