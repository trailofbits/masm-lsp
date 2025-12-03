use masm_lsp::decompiler::collect_decompilation_hints;
use miden_assembly_syntax::ast::ModuleKind;
use miden_assembly_syntax::{Parse, ParseOptions};
use miden_debug_types::{DefaultSourceManager, SourceLanguage, SourceManager};
use tower_lsp::lsp_types::{InlayHintLabel, Position, Range, Url};

fn collect_labels_and_diags(source: &str) -> (Vec<String>, Vec<String>) {
    // Prepare source manager and parse module
    let source_manager = DefaultSourceManager::default();
    let uri = miden_debug_types::Uri::from("file:///test.masm");
    source_manager.load(SourceLanguage::Masm, uri.clone(), source.to_string());

    let source_file = source_manager
        .get_by_uri(&uri)
        .expect("failed to load source");

    let opts = ParseOptions {
        kind: ModuleKind::Library,
        path: None,
        ..Default::default()
    };

    let module = source_file
        .parse_with_options(&source_manager, opts)
        .expect("failed to parse MASM");

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

    let labels = result
        .hints
        .iter()
        .filter_map(|h| match &h.label {
            InlayHintLabel::String(s) => Some(s.clone()),
            _ => None,
        })
        .collect::<Vec<_>>();

    let diags = result
        .diagnostics
        .into_iter()
        .map(|d| d.message)
        .collect::<Vec<_>>();

    (labels, diags)
}

#[test]
fn repeat_consuming_produces_parametric_indices() {
    let source = r#"
proc repeat_add
    repeat.2
        add
    end
end
"#;

    let (labels, diags) = collect_labels_and_diags(source);
    assert!(diags.is_empty(), "expected no diagnostics, got {:?}", diags);
    let joined = labels.join(" ");
    // Should reference arguments with loop counter (a_i or a_(...i...))
    assert!(
        joined.contains("a_(") || joined.contains("a_i"),
        "expected parametric arg names in hints, got: {}",
        joined
    );
}

#[test]
fn while_with_known_effect_succeeds() {
    // Body effect: dup pushes condition replacement ⇒ net_effect 0
    let source = r#"
proc while_dup
    push.1
    while.true
        dup
    end
end
"#;

    let (_labels, diags) = collect_labels_and_diags(source);
    assert!(
        diags.is_empty(),
        "expected while loop to decompile, got diagnostics: {:?}",
        diags
    );
}

#[test]
fn unknown_loop_effect_fails_decompilation() {
    // dynexec has unknown stack effect → analysis failure
    let source = r#"
proc repeat_unknown
    repeat.2
        dynexec
    end
end
"#;

    let (labels, diags) = collect_labels_and_diags(source);
    assert!(
        labels.is_empty(),
        "expected no hints due to failure, got {:?}",
        labels
    );
    assert!(
        diags
            .iter()
            .any(|d| d.contains("unknown") || d.contains("unavailable")),
        "expected diagnostic about unknown loop effect, got {:?}",
        diags
    );
}

#[test]
fn nested_loops_show_compound_parametric_index() {
    let source = r#"
proc nested_repeat
    repeat.2
        repeat.3
            add
        end
    end
end
"#;

    let (labels, diags) = collect_labels_and_diags(source);
    assert!(
        diags.is_empty(),
        "expected nested loops to decompile, got diagnostics: {:?}",
        diags
    );
    // Expect both outer and inner counters to appear in at least one hint
    let joined = labels.join(" ");
    assert!(
        labels.iter().any(|l| l.contains("i") && l.contains("j"))
            || (joined.contains("i") && joined.contains("j")),
        "expected nested counters in parametric names, got: {}",
        joined
    );
}

#[test]
fn neutral_repeat_emits_parametric_indices() {
    // Stack-neutral loop should still surface loop counter usage.
    let source = r#"
proc repeat_neutral
    repeat.3
        movup.2
        dup.0
        drop
    end
end
"#;

    let (labels, diags) = collect_labels_and_diags(source);
    assert!(
        diags.is_empty(),
        "expected neutral loop to decompile, got diagnostics: {:?}",
        diags
    );
    let joined = labels.join(" ");
    assert!(
        joined.contains("a_(") || joined.contains("a_i"),
        "expected parametric names for neutral loop, got: {}",
        joined
    );
}

#[test]
fn producing_repeat_emits_parametric_indices() {
    // Stack-producing loop should annotate produced values with loop counter.
    let source = r#"
proc repeat_producing
    repeat.2
        dup.1
    end
end
"#;

    let (labels, diags) = collect_labels_and_diags(source);
    assert!(
        diags.is_empty(),
        "expected producing loop to decompile, got diagnostics: {:?}",
        diags
    );
    let joined = labels.join(" ");
    assert!(
        joined.contains("a_(") || joined.contains("a_i"),
        "expected parametric names for producing loop, got: {}",
        joined
    );
}

#[test]
fn while_with_non_zero_effect_fails_decompilation() {
    let source = r#"
proc while_grows
    push.1
    while.true
        dup
        dup
    end
end
"#;

    let (labels, diags) = collect_labels_and_diags(source);
    assert!(
        labels.is_empty(),
        "expected while loop failure to suppress hints, got {:?}",
        labels
    );
    assert!(
        diags
            .iter()
            .any(|d| d.contains("while") || d.contains("stack effect")),
        "expected diagnostic about while loop effect, got {:?}",
        diags
    );
}

/// Test that element-wise subtraction loop produces correct parametric indices.
///
/// This test asserts the EXACT expected output format.
///
#[test]
fn elementwise_sub_loop_produces_correct_decompilation() {
    let source = r#"
pub proc sub
    repeat.5
        movup.5
        sub
        movdn.4
    end
end
"#;

    let (labels, diags) = collect_labels_and_diags(source);

    // Should decompile without errors
    assert!(diags.is_empty(), "expected no diagnostics, got {:?}", diags);

    // Expected output - verbatim
    let expected = vec![
        "pub proc sub(a_0: felt, a_1: felt, a_2: felt, a_3: felt, a_4: felt, a_5: felt, a_6: felt, a_7: felt, a_8: felt, a_9: felt) -> (r_0: felt, r_1: felt, r_2: felt, r_3: felt, r_4: felt):",
        "    for i = 0; i < 5; i = i + 1:",
        "        r_i = a_i - a_(i+5)",
        "    end",
        "end",
    ];

    assert_eq!(
        labels.len(),
        expected.len(),
        "expected {} lines, got {}.\nActual output:\n{}",
        expected.len(),
        labels.len(),
        labels.join("\n")
    );

    for (i, (actual, expected_line)) in labels.iter().zip(expected.iter()).enumerate() {
        assert_eq!(
            actual,
            *expected_line,
            "line {} mismatch.\nExpected: {}\nActual:   {}",
            i + 1,
            expected_line,
            actual
        );
    }
}

/// test that testz procedure with dup.3 eq.0 loop decompiles correctly.
///
/// this test asserts the exact expected output format.
#[test]
fn testz_loop_produces_correct_decompilation() {
    let source = r#"
pub proc testz(input: word) -> (i1, word)
    repeat.4
        dup.3 eq.0
    end
    and and and
end
"#;

    let (labels, diags) = collect_labels_and_diags(source);

    // should decompile without errors
    assert!(diags.is_empty(), "expected no diagnostics, got {:?}", diags);

    // expected output - verbatim
    let expected = vec![
        "pub proc testz(a_0: felt, a_1: felt, a_2: felt, a_3: felt) -> (r_0: felt, a_0: felt, a_1: felt, a_2: felt, a_3: felt):",
        "    for i = 0; i < 4; i = i + 1:",
        "        v_0 = a_(3-i); v_(i+1) = (v_0 == 0)",
        "    end",
        "    v_5 = v_3 && v_4; v_6 = v_2 && v_5; r_0 = v_1 && v_6",
        "end",
    ];

    assert_eq!(
        labels.len(),
        expected.len(),
        "expected {} lines, got {}.\nactual output:\n{}",
        expected.len(),
        labels.len(),
        labels.join("\n")
    );

    for (i, (actual, expected_line)) in labels.iter().zip(expected.iter()).enumerate() {
        assert_eq!(
            actual,
            *expected_line,
            "line {} mismatch.\nexpected: {}\nactual:   {}",
            i + 1,
            expected_line,
            actual
        );
    }
}
