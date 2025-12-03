//! Focused analysis of loop variable indexing in decompiled output.
//!
//! This test examines how the decompiler handles parametric variable indices
//! in loops and whether they accurately reflect the semantics of the assembly.

mod common;

use common::fixtures::load_example;
use masm_lsp::decompiler::collect_decompilation_hints;
use miden_assembly_syntax::ast::ModuleKind;
use miden_assembly_syntax::{Parse, ParseOptions};
use miden_debug_types::{DefaultSourceManager, SourceLanguage, SourceManager};
use std::collections::HashMap;
use tower_lsp::lsp_types::{InlayHintLabel, Position, Range, Url};

/// Analysis result for a single procedure with loops
#[derive(Debug)]
#[allow(dead_code)]
struct LoopProcAnalysis {
    proc_name: String,
    hints: Vec<(u32, String)>,
    has_parametric_indices: bool,
    index_patterns: Vec<String>,
    issue: Option<String>,
}

fn collect_hints_for_source(source: &str) -> Vec<(u32, String)> {
    let source_manager = DefaultSourceManager::default();
    let uri = miden_debug_types::Uri::from("file:///test.masm");
    source_manager.load(SourceLanguage::Masm, uri.clone(), source.to_string());

    let source_file = match source_manager.get_by_uri(&uri) {
        Some(f) => f,
        None => return Vec::new(),
    };

    let opts = ParseOptions {
        kind: ModuleKind::Library,
        path: None,
        ..Default::default()
    };

    let module = match source_file.parse_with_options(&source_manager, opts) {
        Ok(m) => m,
        Err(_) => return Vec::new(),
    };

    let line_count = source.lines().count() as u32;
    let last_line_len = source.lines().last().map(|l| l.len()).unwrap_or(0) as u32;
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

    let url = Url::parse("file:///test.masm").unwrap();

    let result = collect_decompilation_hints(
        &module,
        &source_manager,
        &url,
        &visible_range,
        4,
        source,
        None,
    );

    result
        .hints
        .iter()
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
        .collect()
}

fn extract_index_patterns(hints: &[(u32, String)]) -> Vec<String> {
    let mut patterns = Vec::new();
    for (_, hint) in hints {
        // Find parametric indices like a_(i), a_(3-i), a_(i+1), etc.
        let mut idx = 0;
        while let Some(start) = hint[idx..].find("a_(") {
            let abs_start = idx + start;
            if let Some(end) = hint[abs_start..].find(')') {
                let pattern = &hint[abs_start..abs_start + end + 1];
                if !patterns.contains(&pattern.to_string()) {
                    patterns.push(pattern.to_string());
                }
            }
            idx = abs_start + 3;
        }
        // Also find simple a_i patterns
        let mut idx = 0;
        while let Some(start) = hint[idx..].find("a_i") {
            let abs_start = idx + start;
            // Make sure it's not part of a longer pattern like a_(...)
            if abs_start == 0 || hint.chars().nth(abs_start - 1) != Some('(') {
                if !patterns.contains(&"a_i".to_string()) {
                    patterns.push("a_i".to_string());
                }
            }
            idx = abs_start + 3;
        }
    }
    patterns
}

#[allow(dead_code)]
fn analyze_loop_proc(source: &str, proc_name: &str) -> Option<LoopProcAnalysis> {
    let hints = collect_hints_for_source(source);

    // Find hints for this procedure (approximate by looking for proc declaration)
    let proc_hints: Vec<_> = hints
        .iter()
        .filter(|(_, h)| h.contains(&format!("proc {}", proc_name)) || !h.contains("proc "))
        .cloned()
        .collect();

    if proc_hints.is_empty() {
        return None;
    }

    let index_patterns = extract_index_patterns(&proc_hints);
    let has_parametric_indices = index_patterns
        .iter()
        .any(|p| p.contains('i') || p.contains('j'));

    Some(LoopProcAnalysis {
        proc_name: proc_name.to_string(),
        hints: proc_hints,
        has_parametric_indices,
        index_patterns,
        issue: None,
    })
}

/// Test the basic add loop from the example
#[test]
fn analyze_add_loop_indexing() {
    let source = r#"
proc add_loop
    repeat.5
        movup.5
        add
        movdn.4
    end
end
"#;

    let hints = collect_hints_for_source(source);

    eprintln!("\n═══════════════════════════════════════════════════════════════");
    eprintln!("ADD_LOOP ANALYSIS");
    eprintln!("═══════════════════════════════════════════════════════════════");
    eprintln!("\nAssembly semantics:");
    eprintln!("  Input: [a0, a1, a2, a3, a4, b0, b1, b2, b3, b4, ...]");
    eprintln!("  Output: [a0+b0, a1+b1, a2+b2, a3+b3, a4+b4, ...]");
    eprintln!("  Each iteration i: result[i] = a[i] + b[i]");

    eprintln!("\nDecompiled output:");
    for (line, hint) in &hints {
        eprintln!("  L{}: {}", line + 1, hint);
    }

    let patterns = extract_index_patterns(&hints);
    eprintln!("\nParametric patterns found: {:?}", patterns);

    // Check if the indices make sense
    // For this loop: iteration i should access a_i and a_(5+i)
    let has_base_idx = patterns
        .iter()
        .any(|p| p.contains("a_i") || p.contains("a_(i)"));
    let has_offset_idx = patterns.iter().any(|p| p.contains("5") && p.contains("i"));

    eprintln!("\nSemantic check:");
    eprintln!("  Has base index (a_i): {}", has_base_idx);
    eprintln!(
        "  Has offset index (a_(5+i) or similar): {}",
        has_offset_idx
    );

    if has_base_idx && has_offset_idx {
        eprintln!("  ✓ Index patterns match expected semantics");
    } else if hints.iter().any(|(_, h)| h.contains("for i")) {
        eprintln!("  ✓ Loop structure recognized");
    } else {
        eprintln!("  ✗ Index patterns may not fully reflect semantics");
    }
}

/// Test the GF(p^5) addition from base_field.masm
#[test]
fn analyze_ecgfp5_add_indexing() {
    let source = load_example("stdlib/math/ecgfp5/base_field.masm");
    let hints = collect_hints_for_source(&source);

    eprintln!("\n═══════════════════════════════════════════════════════════════");
    eprintln!("ECGFP5 ADD ANALYSIS");
    eprintln!("═══════════════════════════════════════════════════════════════");
    eprintln!("\nAssembly semantics (proc add):");
    eprintln!("  Input: [a0, a1, a2, a3, a4, b0, b1, b2, b3, b4, ...]");
    eprintln!("  Output: [c0, c1, c2, c3, c4, ...] where c = a + b (element-wise)");
    eprintln!("  Each iteration i: c[i] = a[i] + b[i]");

    eprintln!("\nDecompiled hints (first 20):");
    for (line, hint) in hints.iter().take(20) {
        eprintln!("  L{}: {}", line + 1, hint);
    }

    let patterns = extract_index_patterns(&hints);
    eprintln!("\nAll parametric patterns found: {:?}", patterns);
}

/// Test the word comparison operations
#[test]
fn analyze_word_comparison_indexing() {
    let source = load_example("stdlib/word.masm");
    let hints = collect_hints_for_source(&source);

    eprintln!("\n═══════════════════════════════════════════════════════════════");
    eprintln!("WORD COMPARISON ANALYSIS");
    eprintln!("═══════════════════════════════════════════════════════════════");

    // Find hints related to the eq procedure
    let eq_hints: Vec<_> = hints
        .iter()
        .filter(|(_, h)| h.contains("eqw"))
        .cloned()
        .collect();

    eprintln!("\nHints containing eqw:");
    for (line, hint) in &eq_hints {
        eprintln!("  L{}: {}", line + 1, hint);
    }

    // Find hints with loop patterns
    let loop_hints: Vec<_> = hints
        .iter()
        .filter(|(_, h)| h.contains("for i") || h.contains("a_i") || h.contains("a_("))
        .cloned()
        .collect();

    eprintln!("\nHints with loop patterns:");
    for (line, hint) in &loop_hints {
        eprintln!("  L{}: {}", line + 1, hint);
    }

    let patterns = extract_index_patterns(&hints);
    eprintln!("\nAll parametric patterns found: {:?}", patterns);
}

/// Test u256 wrapping_add with repeat.7 loop
#[test]
fn analyze_u256_add_indexing() {
    let source = load_example("stdlib/math/u256.masm");
    let hints = collect_hints_for_source(&source);

    eprintln!("\n═══════════════════════════════════════════════════════════════");
    eprintln!("U256 OPERATIONS ANALYSIS");
    eprintln!("═══════════════════════════════════════════════════════════════");

    eprintln!("\nDecompiled hints (first 30):");
    for (line, hint) in hints.iter().take(30) {
        eprintln!("  L{}: {}", line + 1, hint);
    }

    let patterns = extract_index_patterns(&hints);
    eprintln!("\nParametric patterns found: {:?}", patterns);

    // Check for expected pattern - u256 uses 8 limbs (32-bit each)
    let has_loop_idx = hints
        .iter()
        .any(|(_, h)| h.contains("for i") || h.contains("a_i"));
    eprintln!("\nHas loop variable indexing: {}", has_loop_idx);
}

/// Comprehensive test of all stdlib files with loops
#[test]
fn analyze_all_loop_indexing() {
    eprintln!("\n═══════════════════════════════════════════════════════════════");
    eprintln!("             COMPREHENSIVE LOOP INDEXING ANALYSIS              ");
    eprintln!("═══════════════════════════════════════════════════════════════\n");

    // Files known to contain repeat loops
    let files_with_loops = [
        (
            "stdlib/math/ecgfp5/base_field.masm",
            "repeat.5 element-wise ops",
        ),
        ("stdlib/math/u256.masm", "repeat.7 limb operations"),
        ("stdlib/word.masm", "repeat.3/4 word comparisons"),
        ("stdlib/stark/random_coin.masm", "repeat.3"),
        (
            "stdlib/crypto/dsa/rpo_falcon512.masm",
            "repeat.64/128/511 large loops",
        ),
        ("stdlib/sys/vm/ood_frames.masm", "repeat.24"),
        ("stdlib/sys/vm/deep_queries.masm", "repeat.10/2"),
        ("stdlib/sys/vm/constraints_eval.masm", "repeat.14"),
        ("stdlib/add_loop.masm", "simple repeat.5"),
    ];

    let mut total_with_parametric = 0;
    let mut total_analyzed = 0;
    let mut all_patterns: HashMap<String, usize> = HashMap::new();

    for (path, description) in &files_with_loops {
        let content = match std::panic::catch_unwind(|| load_example(path)) {
            Ok(c) => c,
            Err(_) => {
                eprintln!("  {} - SKIPPED (not found)", path);
                continue;
            }
        };

        let hints = collect_hints_for_source(&content);
        let patterns = extract_index_patterns(&hints);

        let has_parametric = !patterns.is_empty();
        let has_loop_structure = hints.iter().any(|(_, h)| h.contains("for i"));

        total_analyzed += 1;
        if has_parametric || has_loop_structure {
            total_with_parametric += 1;
        }

        for pattern in &patterns {
            *all_patterns.entry(pattern.clone()).or_default() += 1;
        }

        let status = if has_parametric || has_loop_structure {
            "✓"
        } else {
            "○"
        };
        eprintln!("  {} {} - {}", status, path, description);
        eprintln!(
            "      Hints: {}, Parametric patterns: {:?}",
            hints.len(),
            patterns
        );
        if has_loop_structure {
            eprintln!("      Has explicit 'for i' loop structure");
        }
    }

    eprintln!("\n───────────────────────────────────────────────────────────────");
    eprintln!("SUMMARY");
    eprintln!("───────────────────────────────────────────────────────────────");
    eprintln!("Files analyzed: {}", total_analyzed);
    eprintln!("Files with parametric indexing: {}", total_with_parametric);

    eprintln!("\nAll unique index patterns found:");
    let mut sorted_patterns: Vec<_> = all_patterns.iter().collect();
    sorted_patterns.sort_by(|a, b| b.1.cmp(a.1));
    for (pattern, count) in sorted_patterns {
        eprintln!("  {} ({}x)", pattern, count);
    }
}

/// Investigate the j+j issue in nested loops
#[test]
fn investigate_nested_loop_doubling() {
    eprintln!("\n═══════════════════════════════════════════════════════════════");
    eprintln!("NESTED LOOP COEFFICIENT INVESTIGATION");
    eprintln!("═══════════════════════════════════════════════════════════════\n");

    // From the existing test - nested loops with add
    let nested_add = r#"
proc nested_repeat
    repeat.2
        repeat.3
            add
        end
    end
end
"#;

    let hints = collect_hints_for_source(nested_add);
    eprintln!("Test: nested repeat.2 {{ repeat.3 {{ add }} }}");
    eprintln!("  Expected: Outer loop runs 2x, inner loop runs 3x = 6 adds total");
    eprintln!("  Each add consumes 2 elements, so total inputs = 2 + 6*2 = 14? Or 7?");
    eprintln!("\nDecompiled output:");
    for (line, hint) in &hints {
        eprintln!("  L{}: {}", line + 1, hint);
    }

    let patterns = extract_index_patterns(&hints);
    eprintln!("\nParametric patterns: {:?}", patterns);

    // Check for the j+j issue
    let has_double_j = hints
        .iter()
        .any(|(_, h)| h.contains("j+j") || h.contains("2*j"));
    if has_double_j {
        eprintln!("\n*** ISSUE FOUND: Doubled inner counter (j+j or 2*j) ***");
        eprintln!("This suggests the inner loop coefficient is being applied twice.");
    }

    // Also test a simpler nested case
    let nested_simple = r#"
proc nested_dup
    repeat.2
        repeat.2
            dup
        end
    end
end
"#;

    let hints2 = collect_hints_for_source(nested_simple);
    eprintln!("\n───────────────────────────────────────────────────────────────");
    eprintln!("Test: nested repeat.2 {{ repeat.2 {{ dup }} }}");
    eprintln!("\nDecompiled output:");
    for (line, hint) in &hints2 {
        eprintln!("  L{}: {}", line + 1, hint);
    }
    let patterns2 = extract_index_patterns(&hints2);
    eprintln!("\nParametric patterns: {:?}", patterns2);

    // Test with different loop sizes
    let nested_asymmetric = r#"
proc nested_asymm
    repeat.3
        repeat.2
            add
        end
    end
end
"#;

    let hints3 = collect_hints_for_source(nested_asymmetric);
    eprintln!("\n───────────────────────────────────────────────────────────────");
    eprintln!("Test: nested repeat.3 {{ repeat.2 {{ add }} }}");
    eprintln!("\nDecompiled output:");
    for (line, hint) in &hints3 {
        eprintln!("  L{}: {}", line + 1, hint);
    }
    let patterns3 = extract_index_patterns(&hints3);
    eprintln!("\nParametric patterns: {:?}", patterns3);

    // Analyze the coefficient calculation
    eprintln!("\n═══════════════════════════════════════════════════════════════");
    eprintln!("COEFFICIENT ANALYSIS");
    eprintln!("═══════════════════════════════════════════════════════════════");
    eprintln!("\nFor nested repeat.M {{ repeat.N {{ add }} }}:");
    eprintln!("  - Outer loop i: 0..M");
    eprintln!("  - Inner loop j: 0..N");
    eprintln!("  - Each inner iteration consumes 2 elements");
    eprintln!("  - After N iterations of inner loop: consumed 2*N elements");
    eprintln!("  - Expected index for inner loop: 2*N*i + 2*j");
    eprintln!("\nFor repeat.2 {{ repeat.3 {{ add }} }}:");
    eprintln!("  - Expected: a_(6*i+2*j+1) + a_(6*i+2*j) per iteration");
    eprintln!("  - Or simplified: a_(1+6*i+2*j) + a_(6*i+2*j)");
}

/// Test specific semantic correctness of loop indexing
#[test]
fn test_loop_indexing_semantics() {
    eprintln!("\n═══════════════════════════════════════════════════════════════");
    eprintln!("             LOOP INDEXING SEMANTIC CORRECTNESS                ");
    eprintln!("═══════════════════════════════════════════════════════════════\n");

    // Test case 1: Simple consuming loop (repeat.N { add })
    let consuming_loop = r#"
proc sum_pairs
    repeat.3
        add
    end
end
"#;

    let hints = collect_hints_for_source(consuming_loop);
    eprintln!("Test 1: Consuming loop (repeat.3 {{ add }})");
    eprintln!("  Expected: a_(2*i) + a_(2*i+1) pattern (pairs being summed)");
    for (line, hint) in &hints {
        eprintln!("    L{}: {}", line + 1, hint);
    }
    let patterns = extract_index_patterns(&hints);
    eprintln!("  Patterns: {:?}\n", patterns);

    // Test case 2: Stack-neutral loop (repeat.N { swap })
    let neutral_loop = r#"
proc shuffle
    repeat.3
        swap
        movup.2
    end
end
"#;

    let hints = collect_hints_for_source(neutral_loop);
    eprintln!("Test 2: Stack-neutral loop (repeat.3 {{ swap; movup.2 }})");
    eprintln!("  Expected: Parametric indices showing permutation");
    for (line, hint) in &hints {
        eprintln!("    L{}: {}", line + 1, hint);
    }
    let patterns = extract_index_patterns(&hints);
    eprintln!("  Patterns: {:?}\n", patterns);

    // Test case 3: Producing loop (repeat.N { dup })
    let producing_loop = r#"
proc duplicate_all
    repeat.3
        dup.2
    end
end
"#;

    let hints = collect_hints_for_source(producing_loop);
    eprintln!("Test 3: Producing loop (repeat.3 {{ dup.2 }})");
    eprintln!("  Expected: Each iteration duplicates a different element");
    for (line, hint) in &hints {
        eprintln!("    L{}: {}", line + 1, hint);
    }
    let patterns = extract_index_patterns(&hints);
    eprintln!("  Patterns: {:?}\n", patterns);

    // Test case 4: Nested loops
    let nested_loops = r#"
proc nested
    repeat.2
        repeat.3
            add
        end
    end
end
"#;

    let hints = collect_hints_for_source(nested_loops);
    eprintln!("Test 4: Nested loops (repeat.2 {{ repeat.3 {{ add }} }})");
    eprintln!("  Expected: Two counter variables (i, j) in indices");
    for (line, hint) in &hints {
        eprintln!("    L{}: {}", line + 1, hint);
    }
    let patterns = extract_index_patterns(&hints);
    let has_nested = patterns.iter().any(|p| p.contains('j'));
    eprintln!("  Patterns: {:?}", patterns);
    eprintln!("  Has nested counter (j): {}\n", has_nested);
}
