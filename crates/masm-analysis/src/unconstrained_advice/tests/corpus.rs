//! Labeled regression fixtures for `u32` advice-analysis behavior.

use std::collections::BTreeSet;

use masm_decompiler::frontend::testing::workspace_from_modules;
use miden_debug_types::SourceManager;

use crate::{
    infer_unconstrained_advice_in_workspace,
    unconstrained_advice::{AdviceDiagnosticsMap, AdviceSinkKind, AdviceSummaryMap},
    SymbolPath,
};

/// Expected diagnostic details for one corpus fixture.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ExpectedDiagnostic {
    /// Sink kind that should be reported.
    sink: AdviceSinkKind,
    /// Expected callee when the sink is a call argument.
    callee: Option<&'static str>,
    /// Expected call-argument index when the sink is a call argument.
    arg_index: Option<usize>,
    /// Optional message substring used to distinguish diagnostics of the same kind.
    message_substring: Option<&'static str>,
    /// Optional 1-based source line used to pin the diagnostic to one sink.
    span_line: Option<u32>,
    /// Exact distinct advice-origin locations expected for this diagnostic.
    origins: &'static [ExpectedOrigin],
    /// Minimum number of matching diagnostics expected for this pattern.
    count: usize,
}

/// One expected advice-origin location recorded in the regression fixtures.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct ExpectedOrigin {
    /// Source file that should contain the origin.
    file: &'static str,
    /// One-indexed source line.
    line: u32,
    /// One-indexed source column.
    column: u32,
}

/// One resolved advice-origin location used for exact corpus matching.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct OriginLocation {
    /// Source file that contains the origin.
    file: String,
    /// One-indexed source line.
    line: u32,
    /// One-indexed source column.
    column: u32,
}

/// One labeled regression fixture.
#[derive(Debug, Clone, Copy)]
struct CorpusCase {
    /// Stable display name for the fixture.
    name: &'static str,
    /// Workspace modules used to build the fixture.
    modules: &'static [(&'static str, &'static str)],
    /// Procedure whose diagnostics classify the fixture.
    procedure: &'static str,
    /// Exact number of diagnostics expected for the fixture.
    expected_count: usize,
    /// Whether diagnostics outside this `u32` regression suite are allowed.
    allow_other_diagnostics: bool,
    /// Diagnostic patterns that must appear in the fixture output.
    required_diagnostics: &'static [ExpectedDiagnostic],
}

/// Return diagnostics for one procedure from the combined advice map.
fn diagnostics_for<'a>(
    diags: &'a AdviceDiagnosticsMap,
    procedure: &SymbolPath,
) -> &'a [crate::AdviceDiagnostic] {
    diags.get(procedure).map(Vec::as_slice).unwrap_or_default()
}

/// Return whether the target procedure was analyzed without falling back to an opaque summary.
fn procedure_was_analyzed(summaries: &AdviceSummaryMap, procedure: &SymbolPath) -> bool {
    summaries
        .get(procedure)
        .is_some_and(|summary| !summary.is_unknown())
}

/// Return whether a sink belongs to this `u32` regression suite.
const fn is_u32_corpus_sink(sink: AdviceSinkKind) -> bool {
    matches!(sink, AdviceSinkKind::U32Expression | AdviceSinkKind::U32Intrinsic)
}

/// Return whether one actual diagnostic matches the expected diagnostic contract.
fn matches_expected(
    sources: &impl SourceManager,
    actual: &crate::AdviceDiagnostic,
    expected: ExpectedDiagnostic,
) -> bool {
    let expected_origins = expected
        .origins
        .iter()
        .copied()
        .map(OriginLocation::from)
        .collect::<BTreeSet<_>>();
    actual.sink == expected.sink
        && expected.callee.is_none_or(|callee| {
            actual
                .callee
                .as_ref()
                .is_some_and(|actual_callee| actual_callee == &procedure_path(callee))
        })
        && actual.arg_index == expected.arg_index
        && expected.span_line.is_none_or(|line| {
            sources
                .file_line_col(actual.span)
                .ok()
                .is_some_and(|location| location.line.to_usize() as u32 == line)
        })
        && expected
            .message_substring
            .is_none_or(|message| actual.message.contains(message))
        && diagnostic_origins(sources, actual) == expected_origins
}

/// Assert that the actual diagnostics exactly match the expected corpus contract.
fn assert_diagnostics_match(
    sources: &impl SourceManager,
    case: &CorpusCase,
    actual: &[crate::AdviceDiagnostic],
) {
    let required_count: usize = case.required_diagnostics.iter().map(|expected| expected.count).sum();
    assert_eq!(
        required_count,
        case.expected_count,
        "fixture `{}` under-specifies diagnostics: expected_count={}, required_count={required_count}",
        case.name,
        case.expected_count
    );

    assert_eq!(
        actual.len(),
        case.expected_count,
        "fixture `{}` expected {} diagnostics, got: {actual:?}",
        case.name,
        case.expected_count
    );

    let mut matched = vec![false; actual.len()];
    for expected in case.required_diagnostics {
        for _ in 0..expected.count {
            let next_match = actual
                .iter()
                .enumerate()
                .find(|(index, diag)| !matched[*index] && matches_expected(sources, diag, *expected))
                .map(|(index, _)| index);

            let index = next_match.unwrap_or_else(|| {
                panic!(
                    "fixture `{}` expected a distinct diagnostic matching {expected:?}, got: {actual:?}",
                    case.name
                )
            });
            matched[index] = true;
        }
    }

    let unmatched = actual
        .iter()
        .zip(matched.iter())
        .filter_map(|(diag, was_matched)| (!*was_matched).then_some(diag))
        .collect::<Vec<_>>();
    assert!(
        unmatched.is_empty(),
        "fixture `{}` had unmatched diagnostics after exact matching: {unmatched:?}",
        case.name
    );
}

/// Return the canonical symbol path for a corpus procedure.
fn procedure_path(procedure: &str) -> SymbolPath {
    SymbolPath::new(procedure.to_string())
}

/// Return a simple expected diagnostic contract.
const fn expected_diagnostic(
    sink: AdviceSinkKind,
    callee: Option<&'static str>,
    arg_index: Option<usize>,
    message_substring: Option<&'static str>,
    span_line: Option<u32>,
    origins: &'static [ExpectedOrigin],
) -> ExpectedDiagnostic {
    ExpectedDiagnostic {
        sink,
        callee,
        arg_index,
        message_substring,
        span_line,
        origins,
        count: 1,
    }
}

/// Return an expected diagnostic contract with a custom minimum count.
const fn repeated_diagnostic(
    sink: AdviceSinkKind,
    callee: Option<&'static str>,
    arg_index: Option<usize>,
    message_substring: Option<&'static str>,
    span_line: Option<u32>,
    origins: &'static [ExpectedOrigin],
    count: usize,
) -> ExpectedDiagnostic {
    ExpectedDiagnostic {
        sink,
        callee,
        arg_index,
        message_substring,
        span_line,
        origins,
        count,
    }
}

/// Return diagnostics for one procedure from the combined advice map.
fn diagnostics_for_case(
    sources: &impl SourceManager,
    case: &CorpusCase,
    diags: &AdviceDiagnosticsMap,
    procedure: &str,
) -> Vec<crate::AdviceDiagnostic> {
    diagnostics_for(diags, &procedure_path(procedure))
        .iter()
        .filter(|diag| {
            is_u32_corpus_sink(diag.sink)
                || case.required_diagnostics.iter().any(|expected| {
                    expected.sink == AdviceSinkKind::CallArgument
                        && matches_expected(sources, diag, *expected)
                })
        })
        .cloned()
        .collect()
}

/// Return the exact origin locations present in one diagnostic.
fn diagnostic_origins(
    sources: &impl SourceManager,
    diagnostic: &crate::AdviceDiagnostic,
) -> BTreeSet<OriginLocation> {
    diagnostic
        .origins
        .iter()
        .copied()
        .map(|origin| {
            sources.file_line_col(origin).unwrap_or_else(|_| {
                panic!(
                    "expected origin span {:?} to resolve for diagnostic {:?}",
                    origin, diagnostic
                )
            })
        })
        .map(OriginLocation::from)
        .collect()
}

impl From<ExpectedOrigin> for OriginLocation {
    fn from(value: ExpectedOrigin) -> Self {
        Self {
            file: value.file.to_string(),
            line: value.line,
            column: value.column,
        }
    }
}

impl From<miden_debug_types::FileLineCol> for OriginLocation {
    fn from(value: miden_debug_types::FileLineCol) -> Self {
        Self {
            file: value.uri.to_string(),
            line: value.line.to_usize() as u32,
            column: value.column.to_usize() as u32,
        }
    }
}

/// Return one expected origin location for the regression suite.
const fn expected_origin(file: &'static str, line: u32, column: u32) -> ExpectedOrigin {
    ExpectedOrigin { file, line, column }
}

/// Required diagnostics for the `mod_12289`-style unchecked remainder fixture.
const MOD_12289_REQUIRED_DIAGNOSTICS: [ExpectedDiagnostic; 1] = [repeated_diagnostic(
    AdviceSinkKind::U32Intrinsic,
    None,
    None,
    None,
    Some(6),
    &[expected_origin("falcon", 4, 5)],
    1,
)];

/// Required diagnostics for the `U32` call-argument fixture.
const CALL_ARGUMENT_REQUIRED_DIAGNOSTICS: [ExpectedDiagnostic; 1] = [expected_diagnostic(
    AdviceSinkKind::CallArgument,
    Some("calls::needs_u32"),
    Some(0),
    Some("expects U32"),
    Some(8),
    &[expected_origin("calls", 7, 5)],
)];

/// Required diagnostics for a fixture where one origin fans out into two `u32` warnings.
const FANOUT_REQUIRED_DIAGNOSTICS: [ExpectedDiagnostic; 2] = [
    expected_diagnostic(
        AdviceSinkKind::U32Intrinsic,
        None,
        None,
        None,
        Some(5),
        &[expected_origin("noise", 2, 5)],
    ),
    expected_diagnostic(
        AdviceSinkKind::U32Intrinsic,
        None,
        None,
        None,
        Some(8),
        &[expected_origin("noise", 2, 5)],
    ),
];

/// Required diagnostics for a fixture where one origin fans out into two `U32` call warnings.
const CALL_FANOUT_REQUIRED_DIAGNOSTICS: [ExpectedDiagnostic; 2] = [
    expected_diagnostic(
        AdviceSinkKind::CallArgument,
        Some("calls::needs_u32"),
        Some(0),
        Some("expects U32"),
        Some(9),
        &[expected_origin("calls", 7, 5)],
    ),
    expected_diagnostic(
        AdviceSinkKind::CallArgument,
        Some("calls::needs_u32"),
        Some(0),
        Some("expects U32"),
        Some(11),
        &[expected_origin("calls", 7, 5)],
    ),
];

/// No diagnostics are expected for sanitized fixtures.
const NO_REQUIRED_DIAGNOSTICS: [ExpectedDiagnostic; 0] = [];

/// Labeled `u32` regression fixtures.
const CORPUS_CASES: [CorpusCase; 7] = [
    CorpusCase {
        name: "mod_12289_like_missing_remainder_validation",
        modules: &[(
            "falcon",
            "proc mod_12289_like\n    adv_push.2\n    u32assert2\n    adv_push.1\n    push.12289\n    u32overflowing_sub\n    drop\n    push.1\n    u32overflowing_add\n    drop\nend\n",
        )],
        procedure: "falcon::mod_12289_like",
        expected_count: 1,
        allow_other_diagnostics: false,
        required_diagnostics: &MOD_12289_REQUIRED_DIAGNOSTICS,
    },
    CorpusCase {
        name: "call_argument_requires_u32",
        modules: &[(
            "calls",
            "proc needs_u32\n    push.1\n    u32wrapping_add\nend\n\nproc caller\n    adv_push.1\n    exec.needs_u32\nend\n",
        )],
        procedure: "calls::caller",
        expected_count: 1,
        allow_other_diagnostics: false,
        required_diagnostics: &CALL_ARGUMENT_REQUIRED_DIAGNOSTICS,
    },
    CorpusCase {
        name: "single_origin_fans_out_to_two_u32_warnings",
        modules: &[(
            "noise",
            "proc noisy\n    adv_push.1\n    dup\n    push.1\n    u32overflowing_add\n    drop\n    push.2\n    u32wrapping_add\nend\n",
        )],
        procedure: "noise::noisy",
        expected_count: 2,
        allow_other_diagnostics: false,
        required_diagnostics: &FANOUT_REQUIRED_DIAGNOSTICS,
    },
    CorpusCase {
        name: "single_origin_fans_out_to_two_u32_call_warnings",
        modules: &[(
            "calls",
            "proc needs_u32\n    push.1\n    u32wrapping_add\nend\n\nproc noisy\n    adv_push.1\n    dup\n    exec.needs_u32\n    drop\n    exec.needs_u32\nend\n",
        )],
        procedure: "calls::noisy",
        expected_count: 2,
        allow_other_diagnostics: false,
        required_diagnostics: &CALL_FANOUT_REQUIRED_DIAGNOSTICS,
    },
    CorpusCase {
        name: "validated_once_then_reused",
        modules: &[(
            "validated",
            "proc ok\n    adv_push.1\n    u32assert\n    dup\n    push.1\n    u32overflowing_add\n    drop\n    push.2\n    u32wrapping_add\nend\n",
        )],
        procedure: "validated::ok",
        expected_count: 0,
        allow_other_diagnostics: false,
        required_diagnostics: &NO_REQUIRED_DIAGNOSTICS,
    },
    CorpusCase {
        name: "validated_local_round_trip",
        modules: &[(
            "validated",
            "@locals(1)\nproc ok\n    adv_push.1\n    u32assert\n    loc_store.0\n    loc_load.0\n    push.1\n    u32wrapping_add\nend\n",
        )],
        procedure: "validated::ok",
        expected_count: 0,
        allow_other_diagnostics: false,
        required_diagnostics: &NO_REQUIRED_DIAGNOSTICS,
    },
    CorpusCase {
        name: "validated_once_then_reused_across_call",
        modules: &[(
            "validated",
            "proc needs_u32\n    push.1\n    u32wrapping_add\nend\n\nproc ok\n    adv_push.1\n    u32assert\n    dup\n    exec.needs_u32\n    drop\n    exec.needs_u32\nend\n",
        )],
        procedure: "validated::ok",
        expected_count: 0,
        allow_other_diagnostics: false,
        required_diagnostics: &NO_REQUIRED_DIAGNOSTICS,
    },
];

/// Return the labeled `u32` regression fixtures.
fn corpus_cases() -> &'static [CorpusCase] {
    &CORPUS_CASES
}

#[test]
fn u32_regression_corpus_matches_current_labels() {
    for case in corpus_cases() {
        let workspace = workspace_from_modules(case.modules);
        let (summaries, diagnostics) = infer_unconstrained_advice_in_workspace(&workspace);
        let procedure = procedure_path(case.procedure);
        let all_diagnostics = diagnostics_for(&diagnostics, &procedure);

        assert!(
            procedure_was_analyzed(&summaries, &procedure),
            "fixture `{}` expected procedure `{}` to be analyzed without an opaque summary",
            case.name,
            case.procedure
        );

        let procedure_diags =
            diagnostics_for_case(&workspace.source_manager(), case, &diagnostics, case.procedure);
        if !case.allow_other_diagnostics {
            assert_eq!(
                procedure_diags.len(),
                all_diagnostics.len(),
                "fixture `{}` emitted non-u32 diagnostics alongside the corpus diagnostics: {all_diagnostics:?}",
                case.name
            );
        }
        assert_diagnostics_match(&workspace.source_manager(), case, &procedure_diags);
    }
}
