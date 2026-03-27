//! Ignored stdlib evaluation harness for the `u32` advice analysis.

use std::{
    collections::{BTreeMap, BTreeSet},
    io,
    path::{Path, PathBuf},
    sync::Arc,
};

use masm_decompiler::frontend::{LibraryRoot, Workspace};
use miden_debug_types::{DefaultSourceManager, SourceManager};
use serde_json::json;

use crate::{AnalysisSnapshot, AdviceDiagnostic, AdviceSinkKind};

/// Environment variable pointing at the stdlib ASM root to analyze.
const STDLIB_ROOT_ENV: &str = "MASM_PHASE2_STDLIB_ROOT";

/// One resolved origin location used in the stdlib noise report.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct OriginLocation {
    /// Source file URI.
    file: String,
    /// One-indexed source line.
    line: usize,
    /// One-indexed source column.
    column: usize,
}

impl From<miden_debug_types::FileLineCol> for OriginLocation {
    fn from(value: miden_debug_types::FileLineCol) -> Self {
        Self {
            file: value.uri.to_string(),
            line: value.line.to_usize(),
            column: value.column.to_usize(),
        }
    }
}

/// Recursively collect `.masm` files under `dir`.
fn collect_masm_files(dir: &Path, out: &mut BTreeSet<PathBuf>) -> io::Result<()> {
    for entry in std::fs::read_dir(dir)? {
        let path = entry?.path();
        if path.is_dir() {
            collect_masm_files(&path, out)?;
        } else if path.extension().and_then(|ext| ext.to_str()) == Some("masm") {
            out.insert(std::fs::canonicalize(&path)?);
        }
    }
    Ok(())
}

/// Build a workspace for the stdlib root using the same library-root layout as `masm-lint`.
fn workspace_from_stdlib_root(root: &Path) -> Workspace {
    let sources: Arc<DefaultSourceManager> = Arc::new(DefaultSourceManager::default());
    let canonical_root = std::fs::canonicalize(root).unwrap_or_else(|err| {
        panic!("failed to resolve stdlib root {}: {err}", root.display())
    });
    assert!(
        canonical_root.is_dir(),
        "stdlib root {} is not a directory",
        canonical_root.display()
    );
    let roots = vec![
        LibraryRoot::new("miden::core", canonical_root.clone()),
        LibraryRoot::new("", canonical_root.clone()),
    ];
    let mut workspace = Workspace::with_source_manager(roots, sources);

    let mut files = BTreeSet::new();
    collect_masm_files(&canonical_root, &mut files).unwrap_or_else(|err| {
        panic!(
            "failed to traverse stdlib root {}: {err}",
            canonical_root.display()
        )
    });
    assert!(
        !files.is_empty(),
        "stdlib root {} does not contain any .masm files",
        canonical_root.display()
    );
    for file in files {
        workspace
            .load_entry(&file)
            .unwrap_or_else(|err| panic!("failed to load {}: {err}", file.display()));
    }
    workspace.load_dependencies();
    workspace
}

/// Return whether one advice diagnostic belongs to the `u32`-focused report.
fn is_u32_relevant_diagnostic(diag: &AdviceDiagnostic) -> bool {
    matches!(
        diag.sink,
        AdviceSinkKind::U32Expression | AdviceSinkKind::U32Intrinsic
    ) || matches!(diag.call_requirement, Some(crate::CallArgumentRequirement::U32))
}

/// Return `true` when one procedure name refers to the Falcon `mod_12289` reduction helper.
fn is_falcon_mod_12289(procedure: &str) -> bool {
    let mut segments = procedure.rsplit("::");
    matches!(
        (segments.next(), segments.next()),
        (Some("mod_12289"), Some("falcon512_poseidon2" | "falcon512poseidon2"))
    )
}

/// Resolve all origin locations attached to a diagnostic.
fn origin_locations(
    sources: &dyn SourceManager,
    diag: &AdviceDiagnostic,
) -> BTreeSet<OriginLocation> {
    diag.origins
        .iter()
        .copied()
        .filter_map(|origin| sources.file_line_col(origin).ok())
        .map(OriginLocation::from)
        .collect()
}

/// Render a sink kind for the JSON report.
const fn sink_name(sink: AdviceSinkKind) -> &'static str {
    match sink {
        AdviceSinkKind::U32Expression => "u32_expression",
        AdviceSinkKind::U32Intrinsic => "u32_intrinsic",
        AdviceSinkKind::NonZeroOperand => "nonzero_operand",
        AdviceSinkKind::CallArgument => "call_argument",
        AdviceSinkKind::MemoryAddress => "memory_address",
        AdviceSinkKind::MerkleRoot => "merkle_root",
    }
}

/// Return the average number of times one distinct origin participates in a diagnostic.
fn fanout_ratio(origin_counts: &BTreeMap<OriginLocation, usize>) -> f64 {
    let distinct_origins = origin_counts.len();
    if distinct_origins == 0 {
        0.0
    } else {
        origin_counts.values().sum::<usize>() as f64 / distinct_origins as f64
    }
}

/// Print a structured stdlib noise report for the current advice analysis.
#[test]
#[ignore = "manual stdlib evaluation; set MASM_PHASE2_STDLIB_ROOT and run with --ignored --nocapture"]
fn stdlib_u32_noise_report() {
    let stdlib_root = std::env::var_os(STDLIB_ROOT_ENV)
        .map(PathBuf::from)
        .unwrap_or_else(|| panic!("set {STDLIB_ROOT_ENV} to the stdlib asm root"));
    let workspace = workspace_from_stdlib_root(&stdlib_root);
    let unresolved_modules = workspace.unresolved_module_paths();
    let snapshot = AnalysisSnapshot::from_workspace(&workspace);

    let u32_diagnostics = snapshot
        .advice_diagnostics
        .values()
        .flat_map(|diags| diags.iter())
        .filter(|diag| is_u32_relevant_diagnostic(diag))
        .collect::<Vec<_>>();

    let sources = workspace.source_manager();
    let mut sink_counts = BTreeMap::<String, usize>::new();
    let mut procedure_counts = BTreeMap::<String, usize>::new();
    let mut origin_counts = BTreeMap::<OriginLocation, usize>::new();
    let mut falcon_mod_12289 = Vec::new();

    for diag in &u32_diagnostics {
        *sink_counts.entry(sink_name(diag.sink).to_string()).or_default() += 1;
        *procedure_counts
            .entry(diag.procedure.as_str().to_string())
            .or_default() += 1;

        let origins = origin_locations(sources.as_ref(), diag);
        for origin in origins {
            *origin_counts.entry(origin).or_default() += 1;
        }

        if is_falcon_mod_12289(diag.procedure.as_str()) {
            falcon_mod_12289.push(json!({
                "procedure": diag.procedure.as_str(),
                "sink": sink_name(diag.sink),
                "message": diag.message,
            }));
        }
    }

    let mut top_procedures = procedure_counts.into_iter().collect::<Vec<_>>();
    top_procedures.sort_by(|(left_name, left_count), (right_name, right_count)| {
        right_count
            .cmp(left_count)
            .then_with(|| left_name.cmp(right_name))
    });
    let top_procedures = top_procedures
        .into_iter()
        .map(|(procedure, count)| json!({ "procedure": procedure, "count": count }))
        .collect::<Vec<_>>();

    let distinct_origin_count = origin_counts.len();
    let average_origin_fanout = fanout_ratio(&origin_counts);
    let max_origin_fanout = origin_counts.values().copied().max().unwrap_or(0);
    let mut top_origins = origin_counts.into_iter().collect::<Vec<_>>();
    top_origins.sort_by(|(left_origin, left_count), (right_origin, right_count)| {
        right_count
            .cmp(left_count)
            .then_with(|| left_origin.cmp(right_origin))
    });
    let top_origins = top_origins
        .into_iter()
        .map(|(origin, count)| {
            json!({
                "origin": {
                    "file": origin.file,
                    "line": origin.line,
                    "column": origin.column,
                },
                "count": count,
            })
        })
        .collect::<Vec<_>>();

    let report = json!({
        "stdlib_root": stdlib_root.display().to_string(),
        "unresolved_modules": unresolved_modules.iter().map(|module| module.as_str()).collect::<Vec<_>>(),
        "u32_diagnostics": {
            "total": u32_diagnostics.len(),
            "distinct_origins": distinct_origin_count,
            "fanout_ratio": average_origin_fanout,
            "max_origin_fanout": max_origin_fanout,
            "sink_counts": sink_counts,
            "top_procedures": top_procedures,
            "top_origins": top_origins,
        },
        "known_findings": {
            "mod_12289": {
                "reproduced": !falcon_mod_12289.is_empty(),
                "diagnostics": falcon_mod_12289,
            }
        }
    });

    println!(
        "{}",
        serde_json::to_string_pretty(&report).expect("stdlib report should serialize")
    );
}

#[test]
fn falcon_mod_12289_matcher_accepts_current_and_legacy_module_spellings() {
    assert!(is_falcon_mod_12289(
        "miden::core::crypto::dsa::falcon512_poseidon2::mod_12289"
    ));
    assert!(is_falcon_mod_12289(
        "miden::core::crypto::dsa::falcon512poseidon2::mod_12289"
    ));
    assert!(!is_falcon_mod_12289(
        "miden::core::crypto::dsa::falcon512_poseidon2::other_proc"
    ));
}

#[test]
fn fanout_ratio_handles_empty_and_non_empty_origin_sets() {
    let mut origin_counts = BTreeMap::new();

    assert_eq!(fanout_ratio(&origin_counts), 0.0);

    origin_counts.insert(
        OriginLocation {
            file: "fixture".to_string(),
            line: 1,
            column: 1,
        },
        1,
    );
    origin_counts.insert(
        OriginLocation {
            file: "fixture".to_string(),
            line: 2,
            column: 1,
        },
        1,
    );
    assert_eq!(fanout_ratio(&origin_counts), 1.0);

    *origin_counts
        .get_mut(&OriginLocation {
            file: "fixture".to_string(),
            line: 1,
            column: 1,
        })
        .expect("expected first origin") = 3;
    assert_eq!(fanout_ratio(&origin_counts), 2.0);
}
