//! Comprehensive decompilation analysis for stdlib files.
//!
//! This test module analyzes decompilation quality across all stdlib files
//! and outputs detailed reports about failures and coverage.

mod common;
use common::fixtures::collect_labels_and_diags;
use common::fixtures::load_example;
use masm_lsp::decompiler::collect_decompilation_hints;
use miden_assembly_syntax::ast::ModuleKind;
use miden_assembly_syntax::{Parse, ParseOptions};
use miden_debug_types::{DefaultSourceManager, SourceLanguage, SourceManager};
use std::collections::HashMap;
use tower_lsp::lsp_types::{InlayHintLabel, Position, Range, Url};

/// All stdlib files to analyze
const STDLIB_FILES: &[&str] = &[
    "stdlib/collections/sorted_array.masm",
    "stdlib/collections/mmr.masm",
    "stdlib/collections/smt.masm",
    "stdlib/crypto/hashes/sha256.masm",
    "stdlib/crypto/hashes/keccak256.masm",
    "stdlib/crypto/hashes/blake3.masm",
    "stdlib/crypto/hashes/rpo.masm",
    "stdlib/crypto/dsa/ecdsa/secp256k1.masm",
    "stdlib/crypto/dsa/rpo_falcon512.masm",
    "stdlib/crypto/elgamal_ecgfp5.masm",
    "stdlib/math/ecgfp5/base_field.masm",
    "stdlib/math/ecgfp5/scalar_field.masm",
    "stdlib/math/ecgfp5/group.masm",
    "stdlib/math/secp256k1/base_field.masm",
    "stdlib/math/secp256k1/scalar_field.masm",
    "stdlib/math/secp256k1/group.masm",
    "stdlib/math/u64.masm",
    "stdlib/math/u256.masm",
    "stdlib/mem.masm",
    "stdlib/pcs/fri/frie2f4.masm",
    "stdlib/pcs/fri/helper.masm",
    "stdlib/stark/constants.masm",
    "stdlib/stark/mod.masm",
    "stdlib/stark/utils.masm",
    "stdlib/stark/ood_frames.masm",
    "stdlib/stark/random_coin.masm",
    "stdlib/stark/deep_queries.masm",
    "stdlib/stark/public_inputs.masm",
    "stdlib/stark/verifier.masm",
    "stdlib/sys/mod.masm",
    "stdlib/sys/vm/mod.masm",
    "stdlib/sys/vm/constraints_eval.masm",
    "stdlib/sys/vm/ood_frames.masm",
    "stdlib/sys/vm/deep_queries.masm",
    "stdlib/sys/vm/public_inputs.masm",
    "stdlib/word.masm",
];

/// Result of decompiling a single file
#[derive(Debug)]
struct FileAnalysis {
    path: String,
    procedure_count: usize,
    total_instructions: usize,
    hint_count: usize,
    diagnostic_count: usize,
    diagnostics: Vec<String>,
    /// Sample hints (first 10)
    sample_hints: Vec<(u32, String)>, // (line, hint_text)
    /// Lines with no hints that had instructions
    #[allow(dead_code)]
    unhinted_lines: Vec<u32>,
}

/// Result of analyzing all files
#[derive(Debug)]
struct AnalysisReport {
    files: Vec<FileAnalysis>,
    total_procedures: usize,
    total_hints: usize,
    total_diagnostics: usize,
    files_with_failures: usize,
    diagnostic_categories: HashMap<String, usize>,
}

fn analyze_file(path: &str) -> Option<FileAnalysis> {
    let content = match std::panic::catch_unwind(|| load_example(path)) {
        Ok(c) => c,
        Err(_) => return None,
    };

    let source_manager = DefaultSourceManager::default();
    let uri_str = format!("file:///test/{}", path);
    let uri = miden_debug_types::Uri::from(uri_str.as_str());
    source_manager.load(SourceLanguage::Masm, uri.clone(), content.clone());

    let source_file = source_manager.get_by_uri(&uri)?;

    let opts = ParseOptions {
        kind: ModuleKind::Library,
        path: None,
        ..Default::default()
    };

    let module = source_file.parse_with_options(&source_manager, opts).ok()?;

    // Count procedures
    let procedure_count = module.items().count();

    // Count instructions (approximate by counting instruction lines)
    let instruction_lines: Vec<u32> = content
        .lines()
        .enumerate()
        .filter(|(_, line)| {
            let trimmed = line.trim();
            !trimmed.is_empty()
                && !trimmed.starts_with('#')
                && !trimmed.starts_with("proc")
                && !trimmed.starts_with("pub")
                && !trimmed.starts_with("begin")
                && !trimmed.starts_with("end")
                && !trimmed.starts_with("use")
                && !trimmed.starts_with("const")
                && !trimmed.starts_with("if")
                && !trimmed.starts_with("else")
                && !trimmed.starts_with("while")
                && !trimmed.starts_with("repeat")
        })
        .map(|(i, _)| i as u32)
        .collect();

    let total_instructions = instruction_lines.len();

    let line_count = content.lines().count() as u32;
    let last_line_len = content.lines().last().map(|l| l.len()).unwrap_or(0) as u32;
    let visible_range = Range {
        start: Position {
            line: 0,
            character: 0,
        },
        end: Position {
            line: line_count,
            character: last_line_len,
        },
    };

    let url = Url::parse(&uri_str).unwrap();

    // Use an empty contract store to avoid per-file contract inference in this perf-heavy test.
    let empty_contracts = masm_lsp::analysis::ContractStore::new();
    let result = collect_decompilation_hints(
        &module,
        &source_manager,
        &url,
        &visible_range,
        4, // indent
        &content,
        Some(&empty_contracts),
    );

    // Collect hints
    let mut hints_by_line: HashMap<u32, Vec<String>> = HashMap::new();
    for hint in &result.hints {
        let line = hint.position.line;
        let text = match &hint.label {
            InlayHintLabel::String(s) => s.clone(),
            InlayHintLabel::LabelParts(parts) => parts
                .iter()
                .map(|p| p.value.as_str())
                .collect::<Vec<_>>()
                .join(""),
        };
        hints_by_line.entry(line).or_default().push(text);
    }

    let sample_hints: Vec<(u32, String)> = result
        .hints
        .iter()
        .take(10)
        .filter_map(|h| {
            let text = match &h.label {
                InlayHintLabel::String(s) => s.clone(),
                InlayHintLabel::LabelParts(parts) => parts
                    .iter()
                    .map(|p| p.value.as_str())
                    .collect::<Vec<_>>()
                    .join(""),
            };
            Some((h.position.line, text))
        })
        .collect();

    // Find instruction lines without hints
    let unhinted_lines: Vec<u32> = instruction_lines
        .iter()
        .filter(|line| !hints_by_line.contains_key(line))
        .take(20) // Limit to first 20 unhinted lines
        .copied()
        .collect();

    let diagnostics: Vec<String> = result
        .diagnostics
        .iter()
        .map(|d| d.message.clone())
        .collect();

    Some(FileAnalysis {
        path: path.to_string(),
        procedure_count,
        total_instructions,
        hint_count: result.hints.len(),
        diagnostic_count: result.diagnostics.len(),
        diagnostics,
        sample_hints,
        unhinted_lines,
    })
}

fn categorize_diagnostic(msg: &str) -> &'static str {
    if msg.contains("while") && msg.contains("stack effect") {
        "while_loop_stack_effect"
    } else if msg.contains("while") {
        "while_loop_other"
    } else if msg.contains("repeat") {
        "repeat_loop"
    } else if msg.contains("unknown") || msg.contains("unavailable") {
        "unknown_effect"
    } else if msg.contains("call") {
        "call_resolution"
    } else if msg.contains("stack") {
        "stack_tracking"
    } else if msg.contains("loop") {
        "loop_other"
    } else {
        "other"
    }
}

fn analyze_all_files() -> AnalysisReport {
    let mut files = Vec::new();
    let mut total_procedures = 0;
    let mut total_hints = 0;
    let mut total_diagnostics = 0;
    let mut files_with_failures = 0;
    let mut diagnostic_categories: HashMap<String, usize> = HashMap::new();

    for path in STDLIB_FILES {
        eprint!("Analyzing {}... ", path);
        if let Some(analysis) = analyze_file(path) {
            total_procedures += analysis.procedure_count;
            total_hints += analysis.hint_count;
            total_diagnostics += analysis.diagnostic_count;

            if analysis.diagnostic_count > 0 {
                files_with_failures += 1;
            }

            for diag in &analysis.diagnostics {
                let category = categorize_diagnostic(diag);
                *diagnostic_categories
                    .entry(category.to_string())
                    .or_default() += 1;
            }

            eprintln!(
                "{} procs, {} hints, {} diagnostics",
                analysis.procedure_count, analysis.hint_count, analysis.diagnostic_count
            );
            files.push(analysis);
        } else {
            eprintln!("SKIPPED (parse failed or not found)");
        }
    }

    AnalysisReport {
        files,
        total_procedures,
        total_hints,
        total_diagnostics,
        files_with_failures,
        diagnostic_categories,
    }
}

#[test]
fn analyze_decompilation_coverage() {
    let report = analyze_all_files();

    eprintln!("\n══════════════════════════════════════════════════════════════════");
    eprintln!("                    DECOMPILATION ANALYSIS REPORT                  ");
    eprintln!("══════════════════════════════════════════════════════════════════\n");

    eprintln!("SUMMARY");
    eprintln!("───────");
    eprintln!("Files analyzed:       {}", report.files.len());
    eprintln!("Total procedures:     {}", report.total_procedures);
    eprintln!("Total hints:          {}", report.total_hints);
    eprintln!("Total diagnostics:    {}", report.total_diagnostics);
    eprintln!("Files with failures:  {}", report.files_with_failures);

    eprintln!("\nDIAGNOSTIC CATEGORIES");
    eprintln!("─────────────────────");
    let mut categories: Vec<_> = report.diagnostic_categories.iter().collect();
    categories.sort_by(|a, b| b.1.cmp(a.1));
    for (cat, count) in categories {
        eprintln!("  {}: {}", cat, count);
    }

    eprintln!("\nFILES WITH DIAGNOSTICS");
    eprintln!("──────────────────────");
    for analysis in &report.files {
        if !analysis.diagnostics.is_empty() {
            eprintln!(
                "\n  {} ({} diagnostics)",
                analysis.path, analysis.diagnostic_count
            );
            for (i, diag) in analysis.diagnostics.iter().enumerate().take(5) {
                eprintln!("    {}. {}", i + 1, diag);
            }
            if analysis.diagnostics.len() > 5 {
                eprintln!("    ... and {} more", analysis.diagnostics.len() - 5);
            }
        }
    }

    eprintln!("\nDECOMPILATION COVERAGE BY FILE");
    eprintln!("──────────────────────────────");
    for analysis in &report.files {
        let coverage = if analysis.total_instructions > 0 {
            (analysis.hint_count as f64 / analysis.total_instructions as f64 * 100.0).min(100.0)
        } else {
            100.0
        };
        let status = if analysis.diagnostic_count == 0 {
            "✓"
        } else {
            "✗"
        };
        eprintln!(
            "  {} {} - {:>3} procs, {:>4} hints, {:>4} instrs ({:.0}% coverage)",
            status,
            analysis.path,
            analysis.procedure_count,
            analysis.hint_count,
            analysis.total_instructions,
            coverage
        );
    }

    eprintln!("\nSAMPLE HINTS FROM FILES WITH ISSUES");
    eprintln!("────────────────────────────────────");
    for analysis in &report.files {
        if !analysis.diagnostics.is_empty() && !analysis.sample_hints.is_empty() {
            eprintln!("\n  {}:", analysis.path);
            for (line, hint) in analysis.sample_hints.iter().take(5) {
                eprintln!("    L{}: {}", line + 1, hint);
            }
        }
    }

    eprintln!("\nDETAILED DIAGNOSTIC ANALYSIS");
    eprintln!("────────────────────────────");
    let mut all_diags: Vec<(&str, &str)> = Vec::new();
    for analysis in &report.files {
        for diag in &analysis.diagnostics {
            all_diags.push((&analysis.path, diag));
        }
    }

    // Group by unique diagnostic messages
    let mut unique_diags: HashMap<&str, Vec<&str>> = HashMap::new();
    for (path, diag) in &all_diags {
        unique_diags.entry(diag.as_ref()).or_default().push(path);
    }

    let mut sorted_diags: Vec<_> = unique_diags.iter().collect();
    sorted_diags.sort_by(|a, b| b.1.len().cmp(&a.1.len()));

    for (diag, paths) in sorted_diags.iter().take(10) {
        eprintln!("\n  [{}x] {}", paths.len(), diag);
        for path in paths.iter().take(3) {
            eprintln!("       - {}", path);
        }
        if paths.len() > 3 {
            eprintln!("       ... and {} more files", paths.len() - 3);
        }
    }

    eprintln!("\n══════════════════════════════════════════════════════════════════\n");
}

#[test]
fn test_base_legendre_decompiles() {
    let source = r#"
proc base_legendre
    repeat.31
        dup
        mul
    end

    dup

    repeat.32
        dup
        mul
    end

    swap
    dup
    eq.0
    add

    div
end

"#;

    let (labels, diags) = collect_labels_and_diags(source);

    // should decompile without errors
    assert!(diags.is_empty(), "expected no diagnostics, got {:?}", diags);
    assert!(!labels.is_empty(), "expected decompilation, got none");
}
