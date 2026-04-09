//! Clippy-style terminal diagnostic rendering.

use std::fmt::Write;

use masm_decompiler::SymbolPath;
use miden_debug_types::{SourceManager, SourceSpan};
use yansi::Paint as _;

/// A unified diagnostic ready for rendering.
#[derive(Debug)]
pub struct LintDiagnostic {
    /// Human-readable warning message.
    pub message: String,
    /// Primary source span.
    pub span: SourceSpan,
    /// Procedure in which the diagnostic was emitted.
    pub procedure: SymbolPath,
    /// Related source locations (e.g., advice origins).
    pub related: Vec<RelatedSpan>,
}

/// A related source location with an explanatory message.
#[derive(Debug)]
pub struct RelatedSpan {
    /// Source span of the related location.
    pub span: SourceSpan,
    /// Human-readable explanation.
    pub message: String,
}

/// Render a single diagnostic to stdout in clippy/rustc style.
pub fn render_diagnostic(diag: &LintDiagnostic, sources: &dyn SourceManager) {
    print!("{}", render_diagnostic_to_string(diag, sources));
}

/// Render a single diagnostic to a string.
///
/// This produces the same output as [`render_diagnostic`] but returns it
/// instead of printing to stdout, which is useful for testing.
pub fn render_diagnostic_to_string(diag: &LintDiagnostic, sources: &dyn SourceManager) -> String {
    let mut out = String::new();
    write_diagnostic(&mut out, diag, sources);
    out
}

/// Write a single diagnostic in clippy/rustc style.
///
/// Output format:
/// ```text
/// warning: <message>
///   --> file:line:col
///    |
/// NN | source line
///    | ^^^^^^^^^^^
///    |
///    ::: file:line:col
///    |
/// NN | source line
///    | ^^^
///    = help: <related message>
///    = note: in procedure `name`
/// ```
fn write_diagnostic(out: &mut impl Write, diag: &LintDiagnostic, sources: &dyn SourceManager) {
    // Heading: warning message.
    writeln!(
        out,
        "{}: {}",
        "warning".yellow().bold(),
        diag.message.bold()
    )
    .unwrap();

    // Primary location.
    if let Some(loc) = location_string(diag.span, sources) {
        writeln!(out, "  {} {}", "-->".cyan().bold(), loc).unwrap();
        write_snippet(out, diag.span, sources, "    ");
    }

    // Related spans.
    for related in &diag.related {
        if let Some(loc) = location_string(related.span, sources) {
            writeln!(out, "    {} {}", ":::".cyan().bold(), loc).unwrap();
            write_snippet(out, related.span, sources, "    ");
            writeln!(out, "    {} help: {}", "=".cyan().bold(), related.message).unwrap();
        }
    }

    // Procedure note.
    writeln!(
        out,
        "    {} note: in procedure `{}`",
        "=".cyan().bold(),
        diag.procedure.as_str()
    )
    .unwrap();
    writeln!(out).unwrap();
}

/// Format a span as `file:line:col`, stripping the `file://` prefix.
///
/// Returns `None` when the span cannot be resolved.
fn location_string(span: SourceSpan, sources: &dyn SourceManager) -> Option<String> {
    if span == SourceSpan::UNKNOWN {
        return None;
    }
    let flc = sources.file_line_col(span).ok()?;
    let raw_uri = flc.uri.as_str();
    let path = strip_file_scheme(raw_uri);
    Some(format!(
        "{}:{}:{}",
        path,
        flc.line.to_usize(),
        flc.column.to_usize()
    ))
}

/// Strip the `file://` (or `file:`) scheme prefix from a URI for display.
fn strip_file_scheme(uri: &str) -> &str {
    if let Some(rest) = uri.strip_prefix("file://") {
        return rest;
    }
    if let Some(rest) = uri.strip_prefix("file:") {
        return rest;
    }
    uri
}

/// Write a source snippet for `span` with `|` gutter and `^^^` carets.
///
/// `indent` is prepended to every line of output (typically four spaces so
/// the snippet aligns with the `-->` arrow).
fn write_snippet(
    out: &mut impl Write,
    span: SourceSpan,
    sources: &dyn SourceManager,
    indent: &str,
) {
    let Some((line_text, line_number, col_zero)) = resolve_line(span, sources) else {
        return;
    };

    // Width of the gutter: enough digits for the line number.
    let gutter_width = decimal_width(line_number);
    let gutter_pad = " ".repeat(gutter_width);

    // Blank gutter separator.
    writeln!(out, "{}{} {}", indent, gutter_pad, "|".cyan().bold()).unwrap();

    // Source line.
    writeln!(
        out,
        "{}{} {} {}",
        indent,
        line_number.cyan().bold(),
        "|".cyan().bold(),
        line_text
    )
    .unwrap();

    // Carets.
    let span_len = span_display_len(span, sources);
    let col_spaces = " ".repeat(col_zero);
    let carets = "^".repeat(span_len.max(1));
    writeln!(
        out,
        "{}{} {} {}{}",
        indent,
        gutter_pad,
        "|".cyan().bold(),
        col_spaces,
        carets.yellow().bold()
    )
    .unwrap();
}

/// Resolve the source line text, one-indexed line number, and zero-indexed column
/// offset for the start of `span`.
///
/// Returns `None` if the span cannot be resolved.
fn resolve_line(span: SourceSpan, sources: &dyn SourceManager) -> Option<(String, usize, usize)> {
    let flc = sources.file_line_col(span).ok()?;

    // line is one-indexed; lines().nth() is zero-indexed.
    let line_idx = flc.line.to_usize().checked_sub(1)?;
    let col_zero = flc.column.to_usize().saturating_sub(1);

    let source_file = sources.get(span.source_id()).ok()?;
    let line_text = source_file.as_str().lines().nth(line_idx)?.to_owned();

    Some((line_text, flc.line.to_usize(), col_zero))
}

/// Return the display length (in chars) of the source text covered by `span`.
///
/// Falls back to 1 when the slice cannot be resolved.
fn span_display_len(span: SourceSpan, sources: &dyn SourceManager) -> usize {
    sources
        .source_slice(span)
        .ok()
        .map(|s| s.chars().count())
        .unwrap_or(1)
}

/// Return the number of decimal digits needed to represent `n`.
fn decimal_width(n: usize) -> usize {
    if n == 0 {
        return 1;
    }
    let mut width = 0;
    let mut v = n;
    while v > 0 {
        width += 1;
        v /= 10;
    }
    width
}

#[cfg(test)]
mod tests {
    use super::*;
    use miden_debug_types::{DefaultSourceManager, SourceLanguage, Uri};

    /// Load a MASM source string into a fresh source manager and return
    /// the manager along with a helper to create spans by substring.
    fn setup(content: &str) -> (DefaultSourceManager, SpanFinder) {
        yansi::disable();
        let sm = DefaultSourceManager::default();
        let uri = Uri::from("file:///test/example.masm");
        let sf = sm.load(SourceLanguage::Masm, uri, content.to_string());
        let source_id = sf.id();
        let owned = content.to_string();
        (
            sm,
            SpanFinder {
                content: owned,
                source_id,
            },
        )
    }

    /// Helper to create [`SourceSpan`] values by searching for a substring.
    struct SpanFinder {
        content: String,
        source_id: miden_debug_types::SourceId,
    }

    impl SpanFinder {
        /// Return a [`SourceSpan`] covering the first occurrence of `needle`.
        fn span_of(&self, needle: &str) -> SourceSpan {
            let start = self
                .content
                .find(needle)
                .unwrap_or_else(|| panic!("needle {needle:?} not found in source"))
                as u32;
            let end = start + needle.len() as u32;
            SourceSpan::new(self.source_id, start..end)
        }
    }

    #[test]
    fn render_signature_mismatch() {
        let source = "\
proc.foo.2
    push.1
    push.2
    add
end
";
        let (sm, finder) = setup(source);
        let diag = LintDiagnostic {
            message: "the definition declares 2 inputs, but the inferred input count is 3"
                .to_string(),
            span: finder.span_of("proc.foo.2"),
            procedure: SymbolPath::new("foo"),
            related: Vec::new(),
        };
        let output = render_diagnostic_to_string(&diag, &sm);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn render_type_diagnostic() {
        let source = "\
proc.bar.1
    push.1
    u32checked_add
end
";
        let (sm, finder) = setup(source);
        let diag = LintDiagnostic {
            message: "operand type mismatch: expected u32, found felt".to_string(),
            span: finder.span_of("u32checked_add"),
            procedure: SymbolPath::new("bar"),
            related: Vec::new(),
        };
        let output = render_diagnostic_to_string(&diag, &sm);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn render_advice_with_related_spans() {
        let source = "\
proc.baz.1
    adv_push.1
    add
end
";
        let (sm, finder) = setup(source);
        let diag = LintDiagnostic {
            message: "unconstrained advice value flows into arithmetic operation".to_string(),
            span: finder.span_of("add"),
            procedure: SymbolPath::new("baz"),
            related: vec![RelatedSpan {
                span: finder.span_of("adv_push.1"),
                message: "unconstrained advice introduced here".to_string(),
            }],
        };
        let output = render_diagnostic_to_string(&diag, &sm);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn render_unknown_span() {
        let source = "proc.qux.1\n    nop\nend\n";
        let (sm, _finder) = setup(source);
        let diag = LintDiagnostic {
            message: "could not determine stack effect".to_string(),
            span: SourceSpan::UNKNOWN,
            procedure: SymbolPath::new("qux"),
            related: Vec::new(),
        };
        let output = render_diagnostic_to_string(&diag, &sm);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn render_multi_digit_line_number() {
        // Build source where the target instruction is on line 12.
        let mut lines: Vec<String> = Vec::new();
        lines.push("proc.deep.1".to_string());
        for _ in 0..10 {
            lines.push("    nop".to_string());
        }
        lines.push("    push.42".to_string());
        lines.push("end".to_string());
        let source = lines.join("\n") + "\n";

        let (sm, finder) = setup(&source);
        let diag = LintDiagnostic {
            message: "suspicious constant on line 12".to_string(),
            span: finder.span_of("push.42"),
            procedure: SymbolPath::new("deep"),
            related: Vec::new(),
        };
        let output = render_diagnostic_to_string(&diag, &sm);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn render_memory_address_diagnostic() {
        let source = "\
proc.unsafe_load.1
    adv_push.1
    mem_load
end
";
        let (sm, finder) = setup(source);
        let diag = LintDiagnostic {
            message: "unconstrained advice used as memory address".to_string(),
            span: finder.span_of("mem_load"),
            procedure: SymbolPath::new("unsafe_load"),
            related: vec![RelatedSpan {
                span: finder.span_of("adv_push.1"),
                message: "unconstrained advice introduced here".to_string(),
            }],
        };
        let output = render_diagnostic_to_string(&diag, &sm);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn render_local_init_diagnostic() {
        let source = "\
@locals(1)
proc.bad.0
    loc_load.0
    drop
end
";
        let (sm, finder) = setup(source);
        let diag = LintDiagnostic {
            message: "local 0 may be read before initialization".to_string(),
            span: finder.span_of("loc_load.0"),
            procedure: SymbolPath::new("bad"),
            related: Vec::new(),
        };
        let output = render_diagnostic_to_string(&diag, &sm);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn render_merkle_root_diagnostic() {
        let source = "\
proc.unsafe_verify.1
    adv_push.4
    push.0
    push.0
    mtree_get
end
";
        let (sm, finder) = setup(source);
        let diag = LintDiagnostic {
            message: "unconstrained advice used as Merkle tree root".to_string(),
            span: finder.span_of("mtree_get"),
            procedure: SymbolPath::new("unsafe_verify"),
            related: vec![RelatedSpan {
                span: finder.span_of("adv_push.4"),
                message: "unconstrained advice introduced here".to_string(),
            }],
        };
        let output = render_diagnostic_to_string(&diag, &sm);
        insta::assert_snapshot!(output);
    }
}
