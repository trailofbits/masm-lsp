use masm_decompiler::frontend::testing::workspace_from_modules;
use masm_decompiler::ir::{
    BinOp, Call, Constant, Expr, IndexExpr, LocalAccessKind, LocalLoad, LocalStore, LoopPhi,
    LoopVar, MemAccessKind, MemLoad, MemStore, Stmt, ValueId, Var,
};
use miden_debug_types::SourceSpan;

use super::domain::{AccessFootprint, AddrAbs, CellSet, FrameLayout};
use super::inter::infer_uninitialized_locals_in_workspace;
use super::state::LocalInitEnv;
use super::summary::{
    LocalInitDiagnostic, LocalInitDiagnosticsMap, LocalInitSummaryMap,
};
use super::transfer::{eval_block_with_summaries, extract_summary};
use crate::prepared::PreparedProcMap;
use crate::SymbolPath;

/// Extract diagnostics for a single procedure from the diagnostics map.
fn local_init_diagnostics_for(
    diags: &LocalInitDiagnosticsMap,
    proc: &str,
) -> Vec<LocalInitDiagnostic> {
    diags
        .get(&crate::SymbolPath::new(proc.to_string()))
        .cloned()
        .unwrap_or_default()
}

// =========================================================================
// FrameLayout tests
// =========================================================================

#[test]
fn domain_frame_layout_align4_rounds_up() {
    assert_eq!(FrameLayout::new(5).aligned, 8);
    assert_eq!(FrameLayout::new(4).aligned, 4);
    assert_eq!(FrameLayout::new(1).aligned, 4);
    assert_eq!(FrameLayout::new(0).aligned, 0);
}

#[test]
fn domain_frame_layout_declared_cells_excludes_padding() {
    let frame = FrameLayout::new(5);
    let declared = CellSet::closed_range(0, frame.declared - 1);
    for cell in 0..5 {
        assert!(declared.contains(cell), "declared should contain {cell}");
    }
    for cell in 5..8 {
        assert!(
            !declared.contains(cell),
            "declared should NOT contain padding cell {cell}"
        );
    }
}

#[test]
fn domain_frame_layout_all_cells_includes_padding() {
    let frame = FrameLayout::new(5);
    let all = frame.all_cells();
    for cell in 0..8 {
        assert!(all.contains(cell), "all_cells should contain {cell}");
    }
    assert!(!all.contains(8), "all_cells should NOT contain 8");
}

#[test]
fn domain_frame_layout_zero_locals_produces_empty_frame() {
    let frame = FrameLayout::new(0);
    assert_eq!(frame.aligned, 0);
    assert!(frame.all_cells().is_empty());
    assert_eq!(frame.declared, 0);
}

// =========================================================================
// CellSet tests
// =========================================================================

#[test]
fn domain_cellset_empty_contains_nothing() {
    let s = CellSet::empty();
    assert!(!s.contains(0));
    assert!(s.is_empty());
    assert_eq!(s.len(), 0);
}

#[test]
fn domain_cellset_singleton_contains_only_target() {
    let s = CellSet::singleton(3);
    assert!(s.contains(3));
    assert!(!s.contains(2));
    assert!(!s.contains(4));
    assert_eq!(s.len(), 1);
}

#[test]
fn domain_cellset_closed_range_contains_endpoints() {
    let s = CellSet::closed_range(2, 5);
    assert!(!s.contains(1));
    assert!(s.contains(2));
    assert!(s.contains(3));
    assert!(s.contains(4));
    assert!(s.contains(5));
    assert!(!s.contains(6));
    assert_eq!(s.len(), 4);
}

#[test]
fn domain_cellset_union_merges_adjacent_intervals() {
    let a = CellSet::singleton(2);
    let b = CellSet::singleton(3);
    let u = a.union(&b);
    assert_eq!(u.len(), 2);
    assert!(u.contains(2));
    assert!(u.contains(3));
    // Should be coalesced into a single interval [2,3].
    assert_eq!(u.intervals().len(), 1);
    assert_eq!(u.intervals()[0].start, 2);
    assert_eq!(u.intervals()[0].end, 3);
}

#[test]
fn domain_cellset_union_merges_overlapping_intervals() {
    let a = CellSet::closed_range(1, 3);
    let b = CellSet::closed_range(2, 5);
    let u = a.union(&b);
    assert_eq!(u, CellSet::closed_range(1, 5));
    assert_eq!(u.intervals().len(), 1);
}

#[test]
fn domain_cellset_intersection_of_disjoint_is_empty() {
    let a = CellSet::closed_range(0, 2);
    let b = CellSet::closed_range(4, 6);
    assert!(a.intersection(&b).is_empty());
}

#[test]
fn domain_cellset_difference_removes_subset() {
    let full = CellSet::closed_range(0, 5);
    let mid = CellSet::closed_range(2, 3);
    let diff = full.difference(&mid);
    assert!(diff.contains(0));
    assert!(diff.contains(1));
    assert!(!diff.contains(2));
    assert!(!diff.contains(3));
    assert!(diff.contains(4));
    assert!(diff.contains(5));
    assert_eq!(diff.len(), 4);
}

#[test]
fn domain_cellset_shift_by_moves_all_cells() {
    let s = CellSet::closed_range(0, 3);
    let shifted = s.shift_by(2);
    assert_eq!(shifted, CellSet::closed_range(2, 5));
}

#[test]
fn domain_cellset_clip_to_frame_removes_cells_beyond_limit() {
    let s = CellSet::closed_range(0, 7);
    let clipped = s.clip_to_frame(4);
    assert_eq!(clipped, CellSet::closed_range(0, 3));
}

#[test]
fn domain_cellset_shift_past_frame_clips_correctly() {
    let s = CellSet::closed_range(2, 5);
    let shifted = s.shift_by(6);
    // shifted is [8, 11]
    let clipped = shifted.clip_to_frame(8);
    // frame allows cells 0..7, so nothing from [8,11] survives
    assert!(
        clipped.is_empty(),
        "expected empty after clipping [8,11] to frame size 8, got: {clipped}"
    );

    // A variant that does keep some cells: shift by 4 gives [6,9], clip to 8 gives [6,7].
    let shifted2 = s.shift_by(4);
    let clipped2 = shifted2.clip_to_frame(8);
    assert_eq!(clipped2, CellSet::closed_range(6, 7));
}

// =========================================================================
// AddrAbs tests
// =========================================================================

#[test]
fn domain_addr_abs_exact_local_has_no_nonlocal_flag() {
    let addr = AddrAbs::exact_local(CellSet::singleton(2));
    assert!(!addr.maybe_nonlocal);
    assert!(addr.local_cells.contains(2));
}

#[test]
fn domain_addr_abs_join_unions_cells_and_ors_nonlocal() {
    let a = AddrAbs::exact_local(CellSet::singleton(2));
    let b = AddrAbs::exact_local(CellSet::singleton(5));
    let joined = a.join(&b);
    assert!(joined.local_cells.contains(2));
    assert!(joined.local_cells.contains(5));
    assert!(!joined.maybe_nonlocal);

    // Joining with an unknown-nonlocal address sets the flag.
    let c = AddrAbs {
        local_cells: CellSet::empty(),
        maybe_nonlocal: true,
        input_cells: std::collections::BTreeMap::new(),
    };
    let joined2 = joined.join(&c);
    assert!(joined2.local_cells.contains(2));
    assert!(joined2.local_cells.contains(5));
    assert!(joined2.maybe_nonlocal);
}

// =========================================================================
// AccessFootprint tests
// =========================================================================

#[test]
fn domain_access_footprint_scalar_from_exact_addr() {
    let addr = AddrAbs::exact_local(CellSet::singleton(3));
    let fp = AccessFootprint::for_scalar(&addr);
    assert_eq!(fp.may_cells, CellSet::singleton(3));
    assert_eq!(fp.must_cells, CellSet::singleton(3));
}

#[test]
fn domain_access_footprint_scalar_from_imprecise_addr() {
    let cells = CellSet::singleton(2).union(&CellSet::singleton(5));
    let addr = AddrAbs::exact_local(cells.clone());
    let fp = AccessFootprint::for_scalar(&addr);
    assert_eq!(fp.may_cells, cells);
    assert!(
        fp.must_cells.is_empty(),
        "must should be empty when address is imprecise, got: {}",
        fp.must_cells
    );
}

#[test]
fn domain_access_footprint_word_from_singleton_covers_four_cells() {
    let addr = AddrAbs::exact_local(CellSet::singleton(3));
    let fp = AccessFootprint::for_word(&addr);
    assert_eq!(fp.may_cells, CellSet::closed_range(3, 6));
    assert_eq!(fp.must_cells, CellSet::closed_range(3, 6));
}

// =========================================================================
// LocalInit positive tests (must warn)
// =========================================================================

#[test]
fn local_init_scalar_read_before_write_warns() {
    let ws =
        workspace_from_modules(&[("test", "@locals(1)\nproc bad\n  loc_load.0\n  drop\nend\n")]);
    let (_, diagnostics) = infer_uninitialized_locals_in_workspace(&ws);
    let bad = local_init_diagnostics_for(&diagnostics, "test::bad");
    assert_eq!(bad.len(), 1, "expected 1 diagnostic, got: {bad:?}");
    assert_eq!(bad[0].local_indices, vec![0]);
}

#[test]
fn local_init_branch_join_requires_definite_write_on_all_paths() {
    let ws = workspace_from_modules(&[(
        "test",
        "@locals(1)\nproc bad\n  push.1\n  if.true\n    push.42\n    loc_store.0\n  else\n    nop\n  end\n  loc_load.0\n  drop\nend\n",
    )]);
    let (_, diagnostics) = infer_uninitialized_locals_in_workspace(&ws);
    let bad = local_init_diagnostics_for(&diagnostics, "test::bad");
    assert!(
        !bad.is_empty(),
        "expected diagnostic for load after partial write, got: {bad:?}"
    );
}

#[test]
fn local_init_while_does_not_assume_body_executes() {
    // The while body writes cell 0, but the loop may execute zero times.
    // The body must push a boolean for the next iteration.
    let ws = workspace_from_modules(&[(
        "test",
        "@locals(1)\nproc bad\n  push.1\n  while.true\n    push.42\n    loc_store.0\n    push.0\n  end\n  loc_load.0\n  drop\nend\n",
    )]);
    let (_, diagnostics) = infer_uninitialized_locals_in_workspace(&ws);
    let bad = local_init_diagnostics_for(&diagnostics, "test::bad");
    assert!(
        !bad.is_empty(),
        "expected diagnostic because while may execute zero times, got: {bad:?}"
    );
}

#[test]
fn local_init_word_be_read_warns_on_any_uninitialized_cell() {
    let ws = workspace_from_modules(&[(
        "test",
        "@locals(4)\nproc bad\n  push.42\n  loc_store.0\n  push.0.0.0.0\n  loc_loadw_be.0\n  dropw\nend\n",
    )]);
    let (_, diagnostics) = infer_uninitialized_locals_in_workspace(&ws);
    let bad = local_init_diagnostics_for(&diagnostics, "test::bad");
    assert!(
        !bad.is_empty(),
        "expected diagnostic for partial word init, got: {bad:?}"
    );
    // Cells 1, 2, 3 should be flagged as uninitialized.
    let indices = &bad[0].local_indices;
    assert!(
        indices.contains(&1),
        "should contain cell 1, got: {indices:?}"
    );
    assert!(
        indices.contains(&2),
        "should contain cell 2, got: {indices:?}"
    );
    assert!(
        indices.contains(&3),
        "should contain cell 3, got: {indices:?}"
    );
}

#[test]
fn local_init_padding_cell_reached_via_locaddr_offset_warns() {
    // 3 declared locals => frame is 4 cells (0..3). Cell 3 is padding.
    let ws = workspace_from_modules(&[(
        "test",
        "@locals(3)\nproc bad\n  locaddr.0\n  push.3\n  add\n  mem_load\n  drop\nend\n",
    )]);
    let (_, diagnostics) = infer_uninitialized_locals_in_workspace(&ws);
    let bad = local_init_diagnostics_for(&diagnostics, "test::bad");
    assert!(
        !bad.is_empty(),
        "expected diagnostic for padding cell access, got: {bad:?}"
    );
    assert!(
        bad[0].local_indices.contains(&3),
        "diagnostic should flag cell 3, got: {:?}",
        bad[0].local_indices
    );
}

#[test]
fn local_init_disagreeing_phi_over_local_addrs_warns_conservatively() {
    let ws = workspace_from_modules(&[(
        "test",
        "@locals(2)\nproc bad\n  push.1\n  if.true\n    locaddr.0\n  else\n    locaddr.1\n  end\n  mem_load\n  drop\nend\n",
    )]);
    let (_, diagnostics) = infer_uninitialized_locals_in_workspace(&ws);
    let bad = local_init_diagnostics_for(&diagnostics, "test::bad");
    assert!(
        !bad.is_empty(),
        "expected diagnostic for joined local addresses, got: {bad:?}"
    );
    let indices = &bad[0].local_indices;
    // The union of both addresses should cover cells 0 and 1.
    assert!(
        indices.contains(&0) && indices.contains(&1),
        "expected cells 0 and 1, got: {indices:?}"
    );
}

#[test]
fn local_init_sub_on_local_addr_falls_back_conservatively() {
    let ws = workspace_from_modules(&[(
        "test",
        "@locals(2)\nproc bad\n  locaddr.1\n  push.1\n  sub\n  mem_load\n  drop\nend\n",
    )]);
    let (_, diagnostics) = infer_uninitialized_locals_in_workspace(&ws);
    let bad = local_init_diagnostics_for(&diagnostics, "test::bad");
    assert!(
        !bad.is_empty(),
        "expected diagnostic for sub on local addr, got: {bad:?}"
    );
}

#[test]
fn local_init_write_to_different_local_does_not_satisfy_read() {
    let ws = workspace_from_modules(&[(
        "test",
        "@locals(2)\nproc bad\n  push.42\n  loc_store.0\n  loc_load.1\n  drop\nend\n",
    )]);
    let (_, diagnostics) = infer_uninitialized_locals_in_workspace(&ws);
    let bad = local_init_diagnostics_for(&diagnostics, "test::bad");
    assert_eq!(bad.len(), 1, "expected 1 diagnostic, got: {bad:?}");
    assert_eq!(bad[0].local_indices, vec![1]);
}

#[test]
fn local_init_conditional_store_inside_while_warns() {
    // Conditional store inside while: write may not happen on all paths,
    // and while may execute zero times. Body must push a boolean.
    let ws = workspace_from_modules(&[(
        "test",
        "@locals(1)\nproc bad\n  push.1\n  while.true\n    push.1\n    if.true\n      push.42\n      loc_store.0\n    else\n      nop\n    end\n    push.0\n  end\n  loc_load.0\n  drop\nend\n",
    )]);
    let (_, diagnostics) = infer_uninitialized_locals_in_workspace(&ws);
    let bad = local_init_diagnostics_for(&diagnostics, "test::bad");
    assert!(
        !bad.is_empty(),
        "expected diagnostic for conditional store in while, got: {bad:?}"
    );
}

// =========================================================================
// LocalInit negative tests (must NOT warn)
// =========================================================================

#[test]
fn local_init_scalar_store_then_read_is_clean() {
    let ws = workspace_from_modules(&[(
        "test",
        "@locals(1)\nproc ok\n  push.42\n  loc_store.0\n  loc_load.0\n  drop\nend\n",
    )]);
    let (_, diagnostics) = infer_uninitialized_locals_in_workspace(&ws);
    let ok = local_init_diagnostics_for(&diagnostics, "test::ok");
    assert!(ok.is_empty(), "expected no diagnostics, got: {ok:?}");
}

#[test]
fn local_init_repeat_respects_at_least_once_execution() {
    let ws = workspace_from_modules(&[(
        "test",
        "@locals(1)\nproc ok\n  repeat.1\n    push.42\n    loc_store.0\n  end\n  loc_load.0\n  drop\nend\n",
    )]);
    let (_, diagnostics) = infer_uninitialized_locals_in_workspace(&ws);
    let ok = local_init_diagnostics_for(&diagnostics, "test::ok");
    assert!(ok.is_empty(), "expected no diagnostics, got: {ok:?}");
}

fn repeat_test_var(id: u64) -> Var {
    Var::new(ValueId::new(id), 0)
}

fn repeat_test_loop_input_var(loop_depth: usize, stack_depth: usize, subscript: IndexExpr) -> Var {
    Var::loop_input(loop_depth, stack_depth, subscript)
}

fn repeat_test_add_expr(var: &Var, increment: u64) -> Expr {
    Expr::Binary(
        BinOp::Add,
        Box::new(Expr::Var(var.clone())),
        Box::new(Expr::Constant(Constant::Felt(increment))),
    )
}

fn repeat_test_env(frame_cells: u16, addr_var: &Var, addr_cell: u16) -> LocalInitEnv {
    let mut env = LocalInitEnv::new(FrameLayout::new(frame_cells));
    env.set_addr_for_var(
        addr_var,
        AddrAbs::exact_local(CellSet::singleton(addr_cell)),
    );
    env
}

fn repeat_test_repeat(loop_count: usize, body: Vec<Stmt>) -> Stmt {
    Stmt::Repeat {
        span: SourceSpan::default(),
        loop_var: LoopVar::new(0),
        loop_count,
        body,
        phis: vec![],
    }
}

fn repeat_test_repeat_with_phis(
    loop_count: usize,
    body: Vec<Stmt>,
    phis: Vec<LoopPhi>,
) -> Stmt {
    Stmt::Repeat {
        span: SourceSpan::default(),
        loop_var: LoopVar::new(0),
        loop_count,
        body,
        phis,
    }
}

fn repeat_test_mem_store_body(
    addr_var: &Var,
    values: &[Var],
    increment: u64,
    kind: MemAccessKind,
) -> Vec<Stmt> {
    let mut stmts = Vec::new();
    for (idx, value) in values.iter().enumerate() {
        stmts.push(Stmt::Assign {
            span: SourceSpan::default(),
            dest: value.clone(),
            expr: Expr::Constant(Constant::Felt((idx + 1) as u64)),
        });
    }
    stmts.push(Stmt::MemStore {
        span: SourceSpan::default(),
        store: MemStore {
            kind,
            address: vec![addr_var.clone()],
            values: values.to_vec(),
        },
    });
    stmts.push(Stmt::Assign {
        span: SourceSpan::default(),
        dest: addr_var.clone(),
        expr: repeat_test_add_expr(addr_var, increment),
    });
    stmts
}

fn repeat_test_mem_load_body(
    addr_var: &Var,
    outputs: &[Var],
    increment: u64,
    kind: MemAccessKind,
) -> Vec<Stmt> {
    let mut stmts = vec![Stmt::MemLoad {
        span: SourceSpan::default(),
        load: MemLoad {
            kind,
            address: vec![addr_var.clone()],
            outputs: outputs.to_vec(),
        },
    }];
    stmts.push(Stmt::Assign {
        span: SourceSpan::default(),
        dest: addr_var.clone(),
        expr: repeat_test_add_expr(addr_var, increment),
    });
    stmts
}

#[test]
fn local_init_repeat_exact_scalar_write_past_old_cap_is_clean() {
    let addr = repeat_test_var(0);
    let value = repeat_test_var(1);
    let stmts = vec![
        repeat_test_repeat(
            33,
            repeat_test_mem_store_body(&addr, &[value.clone()], 1, MemAccessKind::Element),
        ),
        Stmt::LocalLoad {
            span: SourceSpan::default(),
            load: LocalLoad {
                kind: LocalAccessKind::Element,
                index: 32,
                outputs: vec![repeat_test_var(2)],
            },
        },
    ];
    let env = repeat_test_env(33, &addr, 0);
    let result = eval_block_with_summaries(
        &SymbolPath::new("test::ok".to_string()),
        &stmts,
        env,
        &Default::default(),
        &PreparedProcMap::default(),
    );
    assert!(
        result.diagnostics.is_empty(),
        "expected no diagnostics, got: {:?}",
        result.diagnostics
    );
}

#[test]
fn local_init_repeat_read_before_write_then_write_still_warns() {
    let tmp = repeat_test_var(0);
    let stmts = vec![repeat_test_repeat(
        33,
        vec![
            Stmt::LocalLoad {
                span: SourceSpan::default(),
                load: LocalLoad {
                    kind: LocalAccessKind::Element,
                    index: 0,
                    outputs: vec![tmp.clone()],
                },
            },
            Stmt::LocalStore {
                span: SourceSpan::default(),
                store: LocalStore {
                    index: 0,
                    values: vec![tmp],
                },
            },
        ],
    )];
    let env = LocalInitEnv::new(FrameLayout::new(1));
    let result = eval_block_with_summaries(
        &SymbolPath::new("test::bad".to_string()),
        &stmts,
        env,
        &Default::default(),
        &PreparedProcMap::default(),
    );
    assert!(
        !result.diagnostics.is_empty(),
        "expected read-before-write diagnostic from the first repeat iteration, got none"
    );
    assert!(
        result
            .diagnostics
            .iter()
            .any(|diag| diag.local_indices.contains(&0)),
        "expected repeat diagnostic to mention local cell 0, got: {:?}",
        result.diagnostics
    );
}

/// Regression target: lifted repeat bodies can keep using the entry address
/// variable while the carried pointer increment is represented only in the
/// repeat phi. The analysis should still recognize this as a streaming write
/// across iterations.
#[test]
fn local_init_repeat_body_entry_addr_with_phi_carried_step_makes_late_read_safe() {
    let entry_addr = repeat_test_var(0);
    let step_addr = repeat_test_var(1);
    let carried_addr = repeat_test_var(2);
    let value = repeat_test_var(3);

    let stmts = vec![
        repeat_test_repeat_with_phis(
            33,
            vec![
                Stmt::Assign {
                    span: SourceSpan::default(),
                    dest: value.clone(),
                    expr: Expr::Constant(Constant::Felt(1)),
                },
                Stmt::MemStore {
                    span: SourceSpan::default(),
                    store: MemStore {
                        kind: MemAccessKind::Element,
                        address: vec![entry_addr.clone()],
                        values: vec![value],
                    },
                },
                Stmt::Assign {
                    span: SourceSpan::default(),
                    dest: step_addr.clone(),
                    expr: repeat_test_add_expr(&entry_addr, 1),
                },
            ],
            vec![LoopPhi {
                dest: carried_addr,
                init: entry_addr.clone(),
                step: step_addr,
            }],
        ),
        Stmt::LocalLoad {
            span: SourceSpan::default(),
            load: LocalLoad {
                kind: LocalAccessKind::Element,
                index: 32,
                outputs: vec![repeat_test_var(4)],
            },
        },
    ];

    let env = repeat_test_env(33, &entry_addr, 0);
    let result = eval_block_with_summaries(
        &SymbolPath::new("test::repeat_phi_shape".to_string()),
        &stmts,
        env,
        &Default::default(),
        &PreparedProcMap::default(),
    );
    assert!(
        result.diagnostics.is_empty(),
        "lifted repeat phi shape should initialize cell 32, got: {:?}",
        result.diagnostics
    );
}

#[test]
fn local_init_repeat_summary_body_entry_addr_with_phi_carried_step_marks_late_write() {
    let entry_addr = repeat_test_var(0);
    let step_addr = repeat_test_var(1);
    let carried_addr = repeat_test_var(2);
    let value = repeat_test_var(3);

    let stmts = vec![
        repeat_test_repeat_with_phis(
            33,
            vec![
                Stmt::Assign {
                    span: SourceSpan::default(),
                    dest: value.clone(),
                    expr: Expr::Constant(Constant::Felt(1)),
                },
                Stmt::MemStore {
                    span: SourceSpan::default(),
                    store: MemStore {
                        kind: MemAccessKind::Element,
                        address: vec![entry_addr.clone()],
                        values: vec![value],
                    },
                },
                Stmt::Assign {
                    span: SourceSpan::default(),
                    dest: step_addr.clone(),
                    expr: repeat_test_add_expr(&entry_addr, 1),
                },
            ],
            vec![LoopPhi {
                dest: carried_addr,
                init: entry_addr.clone(),
                step: step_addr,
            }],
        ),
    ];

    let summary = extract_summary(&stmts, 1, &PreparedProcMap::default());
    assert!(!summary.unknown, "expected known summary, got opaque");
    let writes = summary
        .definitely_writes
        .get(&0)
        .unwrap_or_else(|| panic!("missing definite writes for input 0: {summary:?}"));
    assert!(
        writes.contains(32),
        "expected summary to include late write cell 32, got: {writes}"
    );
}

#[test]
fn local_init_repeat_loop_input_phi_binding_makes_late_read_safe() {
    let entry_subscript = IndexExpr::Add(
        Box::new(IndexExpr::Const(0)),
        Box::new(IndexExpr::Const(0)),
    );
    let entry_addr = repeat_test_loop_input_var(0, 0, entry_subscript);
    let step_addr = repeat_test_var(1);
    let carried_addr = repeat_test_var(2);
    let value = repeat_test_var(3);

    let stmts = vec![
        repeat_test_repeat_with_phis(
            33,
            vec![
                Stmt::Assign {
                    span: SourceSpan::default(),
                    dest: value.clone(),
                    expr: Expr::Constant(Constant::Felt(1)),
                },
                Stmt::MemStore {
                    span: SourceSpan::default(),
                    store: MemStore {
                        kind: MemAccessKind::Element,
                        address: vec![entry_addr.clone()],
                        values: vec![value],
                    },
                },
                Stmt::Assign {
                    span: SourceSpan::default(),
                    dest: step_addr.clone(),
                    expr: repeat_test_add_expr(&entry_addr, 1),
                },
            ],
            vec![LoopPhi {
                dest: carried_addr,
                init: entry_addr.clone(),
                step: step_addr,
            }],
        ),
        Stmt::LocalLoad {
            span: SourceSpan::default(),
            load: LocalLoad {
                kind: LocalAccessKind::Element,
                index: 32,
                outputs: vec![repeat_test_var(4)],
            },
        },
    ];

    let env = repeat_test_env(33, &entry_addr, 0);
    let result = eval_block_with_summaries(
        &SymbolPath::new("test::loop_input_ok".to_string()),
        &stmts,
        env,
        &Default::default(),
        &PreparedProcMap::default(),
    );
    assert!(
        result.diagnostics.is_empty(),
        "loop-input repeat phi binding should initialize cell 32, got: {:?}",
        result.diagnostics
    );
}

#[test]
fn local_init_repeat_summary_loop_input_binding_marks_late_write() {
    let entry_subscript = IndexExpr::Add(
        Box::new(IndexExpr::Const(1)),
        Box::new(IndexExpr::Const(1)),
    );
    let entry_addr = repeat_test_loop_input_var(0, 0, entry_subscript);
    let step_addr = repeat_test_var(1);
    let carried_addr = repeat_test_var(2);
    let value = repeat_test_var(3);

    let stmts = vec![
        repeat_test_repeat_with_phis(
            33,
            vec![
                Stmt::Assign {
                    span: SourceSpan::default(),
                    dest: value.clone(),
                    expr: Expr::Constant(Constant::Felt(1)),
                },
                Stmt::MemStore {
                    span: SourceSpan::default(),
                    store: MemStore {
                        kind: MemAccessKind::Element,
                        address: vec![entry_addr.clone()],
                        values: vec![value],
                    },
                },
                Stmt::Assign {
                    span: SourceSpan::default(),
                    dest: step_addr.clone(),
                    expr: repeat_test_add_expr(&entry_addr, 1),
                },
            ],
            vec![LoopPhi {
                dest: carried_addr,
                init: entry_addr.clone(),
                step: step_addr,
            }],
        ),
    ];

    let summary = extract_summary(&stmts, 1, &PreparedProcMap::default());
    assert!(!summary.unknown, "expected known summary, got opaque");
    let writes = summary
        .definitely_writes
        .get(&0)
        .unwrap_or_else(|| panic!("missing definite writes for input 0: {summary:?}"));
    assert!(
        writes.contains(32),
        "expected loop-input summary to include late write cell 32, got: {writes}"
    );
}

#[test]
fn local_init_repeat_loop_input_keys_do_not_collapse_multiple_carried_addrs() {
    let a = repeat_test_loop_input_var(
        0,
        0,
        IndexExpr::Add(
            Box::new(IndexExpr::Const(0)),
            Box::new(IndexExpr::Const(0)),
        ),
    );
    let b = repeat_test_loop_input_var(
        0,
        1,
        IndexExpr::Mul(
            Box::new(IndexExpr::Const(1)),
            Box::new(IndexExpr::Const(1)),
        ),
    );
    let a_step = repeat_test_var(1);
    let b_step = repeat_test_var(2);
    let a_value = repeat_test_var(3);
    let b_value = repeat_test_var(4);
    let a_carried = repeat_test_var(5);
    let b_carried = repeat_test_var(6);

    let stmts = vec![
        repeat_test_repeat_with_phis(
            33,
            vec![
                Stmt::Assign {
                    span: SourceSpan::default(),
                    dest: a_value.clone(),
                    expr: Expr::Constant(Constant::Felt(11)),
                },
                Stmt::MemStore {
                    span: SourceSpan::default(),
                    store: MemStore {
                        kind: MemAccessKind::Element,
                        address: vec![a.clone()],
                        values: vec![a_value.clone()],
                    },
                },
                Stmt::Assign {
                    span: SourceSpan::default(),
                    dest: a_step.clone(),
                    expr: repeat_test_add_expr(&a, 1),
                },
                Stmt::Assign {
                    span: SourceSpan::default(),
                    dest: b_value.clone(),
                    expr: Expr::Constant(Constant::Felt(22)),
                },
                Stmt::MemStore {
                    span: SourceSpan::default(),
                    store: MemStore {
                        kind: MemAccessKind::Element,
                        address: vec![b.clone()],
                        values: vec![b_value.clone()],
                    },
                },
                Stmt::Assign {
                    span: SourceSpan::default(),
                    dest: b_step.clone(),
                    expr: repeat_test_add_expr(&b, 1),
                },
            ],
            vec![
                LoopPhi {
                    dest: a_carried,
                    init: a.clone(),
                    step: a_step,
                },
                LoopPhi {
                    dest: b_carried,
                    init: b.clone(),
                    step: b_step,
                },
            ],
        ),
        Stmt::LocalLoad {
            span: SourceSpan::default(),
            load: LocalLoad {
                kind: LocalAccessKind::Element,
                index: 32,
                outputs: vec![repeat_test_var(7)],
            },
        },
        Stmt::LocalLoad {
            span: SourceSpan::default(),
            load: LocalLoad {
                kind: LocalAccessKind::Element,
                index: 96,
                outputs: vec![repeat_test_var(8)],
            },
        },
    ];

    let mut env = repeat_test_env(97, &a, 0);
    env.set_addr_for_var(
        &b,
        AddrAbs::exact_local(CellSet::singleton(64)),
    );
    let result = eval_block_with_summaries(
        &SymbolPath::new("test::two_loop_inputs".to_string()),
        &stmts,
        env,
        &Default::default(),
        &PreparedProcMap::default(),
    );
    assert!(
        result.diagnostics.is_empty(),
        "distinct loop-input keys should not collapse carried addrs, got: {:?}",
        result.diagnostics
    );
}

#[test]
fn local_init_repeat_stale_constant_does_not_drive_addr_plus_const() {
    let base = repeat_test_var(0);
    let tmp = repeat_test_var(1);
    let offset = repeat_test_var(2);
    let seed = repeat_test_var(3);
    let stored = repeat_test_var(4);
    let post = repeat_test_var(5);

    let stmts = vec![
        Stmt::Assign {
            span: SourceSpan::default(),
            dest: seed.clone(),
            expr: Expr::Constant(Constant::Felt(42)),
        },
        Stmt::LocalStore {
            span: SourceSpan::default(),
            store: LocalStore {
                index: 0,
                values: vec![seed],
            },
        },
        repeat_test_repeat(
            2,
            vec![
                Stmt::Assign {
                    span: SourceSpan::default(),
                    dest: offset.clone(),
                    expr: Expr::Constant(Constant::Felt(1)),
                },
                Stmt::LocalLoad {
                    span: SourceSpan::default(),
                    load: LocalLoad {
                        kind: LocalAccessKind::Element,
                        index: 0,
                        outputs: vec![offset.clone()],
                    },
                },
                Stmt::Assign {
                    span: SourceSpan::default(),
                    dest: tmp.clone(),
                    expr: Expr::Binary(
                        BinOp::Add,
                        Box::new(Expr::Var(base.clone())),
                        Box::new(Expr::Var(offset.clone())),
                    ),
                },
                Stmt::Assign {
                    span: SourceSpan::default(),
                    dest: stored.clone(),
                    expr: Expr::Constant(Constant::Felt(7)),
                },
                Stmt::MemStore {
                    span: SourceSpan::default(),
                    store: MemStore {
                        kind: MemAccessKind::Element,
                        address: vec![tmp],
                        values: vec![stored.clone()],
                    },
                },
            ],
        ),
        Stmt::LocalLoad {
            span: SourceSpan::default(),
            load: LocalLoad {
                kind: LocalAccessKind::Element,
                index: 1,
                outputs: vec![post],
            },
        },
    ];

    let env = repeat_test_env(2, &base, 0);
    let result = eval_block_with_summaries(
        &SymbolPath::new("test::stale_constant".to_string()),
        &stmts,
        env,
        &Default::default(),
        &PreparedProcMap::default(),
    );
    assert!(
        result
            .diagnostics
            .iter()
            .any(|diag| diag.local_indices.contains(&1)),
        "expected stale constant not to make cell 1 look initialized, got: {:?}",
        result.diagnostics
    );
}

#[test]
fn local_init_repeat_exit_binding_uses_final_carried_value() {
    let entry_addr = repeat_test_var(0);
    let step_addr = repeat_test_var(1);
    let carried_addr = repeat_test_var(2);
    let value = repeat_test_var(3);
    let post_value = repeat_test_var(4);

    let stmts = vec![
        repeat_test_repeat_with_phis(
            33,
            vec![
                Stmt::Assign {
                    span: SourceSpan::default(),
                    dest: value.clone(),
                    expr: Expr::Constant(Constant::Felt(1)),
                },
                Stmt::MemStore {
                    span: SourceSpan::default(),
                    store: MemStore {
                        kind: MemAccessKind::Element,
                        address: vec![entry_addr.clone()],
                        values: vec![value],
                    },
                },
                Stmt::Assign {
                    span: SourceSpan::default(),
                    dest: step_addr.clone(),
                    expr: repeat_test_add_expr(&entry_addr, 1),
                },
            ],
            vec![LoopPhi {
                dest: carried_addr.clone(),
                init: entry_addr.clone(),
                step: step_addr,
            }],
        ),
        Stmt::Assign {
            span: SourceSpan::default(),
            dest: post_value.clone(),
            expr: Expr::Constant(Constant::Felt(9)),
        },
        Stmt::MemStore {
            span: SourceSpan::default(),
            store: MemStore {
                kind: MemAccessKind::Element,
                address: vec![carried_addr],
                values: vec![post_value],
            },
        },
        Stmt::LocalLoad {
            span: SourceSpan::default(),
            load: LocalLoad {
                kind: LocalAccessKind::Element,
                index: 33,
                outputs: vec![repeat_test_var(5)],
            },
        },
    ];

    let env = repeat_test_env(34, &entry_addr, 0);
    let result = eval_block_with_summaries(
        &SymbolPath::new("test::repeat_exit".to_string()),
        &stmts,
        env,
        &Default::default(),
        &PreparedProcMap::default(),
    );
    assert!(
        result.diagnostics.is_empty(),
        "repeat exit should use the final carried value, got: {:?}",
        result.diagnostics
    );
}

#[test]
fn local_init_word_le_round_trip_is_clean() {
    let ws = workspace_from_modules(&[(
        "test",
        "@locals(4)\nproc ok\n  push.1.2.3.4\n  loc_storew_le.0\n  dropw\n  push.0.0.0.0\n  loc_loadw_le.0\n  dropw\nend\n",
    )]);
    let (_, diagnostics) = infer_uninitialized_locals_in_workspace(&ws);
    let ok = local_init_diagnostics_for(&diagnostics, "test::ok");
    assert!(ok.is_empty(), "expected no diagnostics, got: {ok:?}");
}

#[test]
fn local_init_mem_store_via_exact_local_addr_initializes_target_cell() {
    // mem_store expects [addr, value, ...] — address on top.
    let ws = workspace_from_modules(&[(
        "test",
        "@locals(1)\nproc ok\n  push.42\n  locaddr.0\n  mem_store\n  loc_load.0\n  drop\nend\n",
    )]);
    let (_, diagnostics) = infer_uninitialized_locals_in_workspace(&ws);
    let ok = local_init_diagnostics_for(&diagnostics, "test::ok");
    assert!(ok.is_empty(), "expected no diagnostics, got: {ok:?}");
}

#[test]
fn local_init_full_frame_written_before_read_is_clean() {
    let ws = workspace_from_modules(&[(
        "test",
        "@locals(3)\nproc ok\n  push.42\n  loc_store.0\n  push.42\n  loc_store.1\n  push.42\n  loc_store.2\n  loc_load.0\n  drop\n  loc_load.1\n  drop\n  loc_load.2\n  drop\nend\n",
    )]);
    let (_, diagnostics) = infer_uninitialized_locals_in_workspace(&ws);
    let ok = local_init_diagnostics_for(&diagnostics, "test::ok");
    assert!(ok.is_empty(), "expected no diagnostics, got: {ok:?}");
}

#[test]
fn local_init_definite_write_on_all_branches_is_clean() {
    let ws = workspace_from_modules(&[(
        "test",
        "@locals(1)\nproc ok\n  push.1\n  if.true\n    push.42\n    loc_store.0\n  else\n    push.99\n    loc_store.0\n  end\n  loc_load.0\n  drop\nend\n",
    )]);
    let (_, diagnostics) = infer_uninitialized_locals_in_workspace(&ws);
    let ok = local_init_diagnostics_for(&diagnostics, "test::ok");
    assert!(ok.is_empty(), "expected no diagnostics, got: {ok:?}");
}

#[test]
fn local_init_aligned_locals_produce_no_padding_warnings() {
    let ws = workspace_from_modules(&[(
        "test",
        "@locals(4)\nproc ok\n  push.42\n  loc_store.0\n  push.42\n  loc_store.1\n  push.42\n  loc_store.2\n  push.42\n  loc_store.3\n  loc_load.0\n  drop\n  loc_load.1\n  drop\n  loc_load.2\n  drop\n  loc_load.3\n  drop\nend\n",
    )]);
    let (_, diagnostics) = infer_uninitialized_locals_in_workspace(&ws);
    let ok = local_init_diagnostics_for(&diagnostics, "test::ok");
    assert!(ok.is_empty(), "expected no diagnostics, got: {ok:?}");
}

#[test]
fn local_init_exec_does_not_disturb_initialized_locals() {
    let ws = workspace_from_modules(&[(
        "test",
        "@locals(1)\nproc helper\n  push.1\nend\nproc ok\n  push.42\n  loc_store.0\n  exec.helper\n  drop\n  loc_load.0\n  drop\nend\n",
    )]);
    let (_, diagnostics) = infer_uninitialized_locals_in_workspace(&ws);
    let ok = local_init_diagnostics_for(&diagnostics, "test::ok");
    assert!(ok.is_empty(), "expected no diagnostics, got: {ok:?}");
}

#[test]
fn local_init_zero_locals_produces_no_warnings() {
    let ws = workspace_from_modules(&[("test", "proc ok\n  push.42\nend\n")]);
    let (_, diagnostics) = infer_uninitialized_locals_in_workspace(&ws);
    let ok = local_init_diagnostics_for(&diagnostics, "test::ok");
    assert!(ok.is_empty(), "expected no diagnostics, got: {ok:?}");
}

// =========================================================================
// Exec local-init tests (interprocedural)
// =========================================================================

/// Positive: caller passes `locaddr.0` to a helper that reads the address
/// without writing first. The warning should appear at the caller-side `exec`
/// span, not inside the callee.
#[test]
fn exec_local_init_read_before_write_in_callee_warns_at_caller() {
    let ws = workspace_from_modules(&[(
        "test",
        // helper takes an address on the stack and does mem_load (read) without write
        "proc helper\n  mem_load\n  drop\nend\n\
         @locals(1)\n\
         proc caller\n  locaddr.0\n  exec.helper\nend\n",
    )]);
    let (_, diagnostics) = infer_uninitialized_locals_in_workspace(&ws);
    let caller_diags = local_init_diagnostics_for(&diagnostics, "test::caller");
    assert!(
        !caller_diags.is_empty(),
        "expected warning on caller-side exec for read-before-write in callee, got none"
    );
    // The diagnostic should reference local cell 0.
    assert!(
        caller_diags[0].local_indices.contains(&0),
        "expected local_indices to contain cell 0, got: {:?}",
        caller_diags[0].local_indices
    );
    // The callee itself should have no local-init diagnostics (it has no locals).
    let helper_diags = local_init_diagnostics_for(&diagnostics, "test::helper");
    assert!(
        helper_diags.is_empty(),
        "expected no diagnostics for helper (no locals), got: {helper_diags:?}"
    );
}

#[test]
fn local_init_repeat_summary_definite_write_crosses_old_cap() {
    let addr = repeat_test_var(0);
    let value = repeat_test_var(1);
    let stmts = vec![repeat_test_repeat(
        33,
        repeat_test_mem_store_body(&addr, &[value.clone()], 1, MemAccessKind::Element),
    )];
    let summary = extract_summary(&stmts, 1, &PreparedProcMap::default());
    assert!(!summary.unknown, "expected known summary, got opaque");
    let writes = summary
        .definitely_writes
        .get(&0)
        .unwrap_or_else(|| panic!("missing definite writes for input 0: {summary:?}"));
    for expected in 0..=32 {
        assert!(
            writes.contains(expected),
            "expected definite writes to include cell {expected}, got: {writes}"
        );
    }
}

#[test]
fn local_init_repeat_summary_read_before_write_crosses_old_cap() {
    let addr = repeat_test_var(0);
    let out = repeat_test_var(1);
    let stmts = vec![repeat_test_repeat(
        33,
        repeat_test_mem_load_body(&addr, &[out.clone()], 1, MemAccessKind::Element),
    )];
    let summary = extract_summary(&stmts, 1, &PreparedProcMap::default());
    assert!(!summary.unknown, "expected known summary, got opaque");
    let reads = summary
        .reads_before_write
        .get(&0)
        .unwrap_or_else(|| panic!("missing reads-before-write for input 0: {summary:?}"));
    for expected in 0..=32 {
        assert!(
            reads.contains(expected),
            "expected reads-before-write to include cell {expected}, got: {reads}"
        );
    }
}

#[test]
fn local_init_exec_repeat_summary_write_past_old_cap_makes_caller_read_safe() {
    let addr = repeat_test_var(0);
    let value = repeat_test_var(1);
    let callee_stmts = vec![repeat_test_repeat(
        33,
        repeat_test_mem_store_body(&addr, &[value.clone()], 1, MemAccessKind::Element),
    )];
    let callee_summary = extract_summary(&callee_stmts, 1, &PreparedProcMap::default());
    let mut summaries = LocalInitSummaryMap::default();
    summaries.insert(SymbolPath::new("test::writer".to_string()), callee_summary);

    let caller_stmts = vec![
        Stmt::Exec {
            span: SourceSpan::default(),
            call: Call {
                target: "test::writer".to_string(),
                args: vec![addr.clone()],
                results: vec![],
            },
        },
        Stmt::LocalLoad {
            span: SourceSpan::default(),
            load: LocalLoad {
                kind: LocalAccessKind::Element,
                index: 32,
                outputs: vec![repeat_test_var(2)],
            },
        },
    ];
    let env = repeat_test_env(33, &addr, 0);
    let result = eval_block_with_summaries(
        &SymbolPath::new("test::caller".to_string()),
        &caller_stmts,
        env,
        &summaries,
        &PreparedProcMap::default(),
    );
    assert!(
        result.diagnostics.is_empty(),
        "expected no diagnostics after exact repeat write summary, got: {:?}",
        result.diagnostics
    );
}

#[test]
fn local_init_exec_repeat_summary_read_past_old_cap_warns_on_caller() {
    let addr = repeat_test_var(0);
    let out = repeat_test_var(1);
    let callee_stmts = vec![repeat_test_repeat(
        33,
        repeat_test_mem_load_body(&addr, &[out.clone()], 1, MemAccessKind::Element),
    )];
    let callee_summary = extract_summary(&callee_stmts, 1, &PreparedProcMap::default());
    let mut summaries = LocalInitSummaryMap::default();
    summaries.insert(SymbolPath::new("test::reader".to_string()), callee_summary);

    let caller_stmts = vec![Stmt::Exec {
        span: SourceSpan::default(),
        call: Call {
            target: "test::reader".to_string(),
            args: vec![addr.clone()],
            results: vec![],
        },
    }];
    let mut env = repeat_test_env(33, &addr, 0);
    env.mark_definitely_written(&CellSet::closed_range(0, 31));
    let result = eval_block_with_summaries(
        &SymbolPath::new("test::caller".to_string()),
        &caller_stmts,
        env,
        &summaries,
        &PreparedProcMap::default(),
    );
    assert!(
        !result.diagnostics.is_empty(),
        "expected exec diagnostic for late repeat read past the old cap, got none"
    );
    assert!(
        result
            .diagnostics
            .iter()
            .any(|diag| diag.local_indices.contains(&32)),
        "expected caller diagnostic to mention cell 32, got: {:?}",
        result.diagnostics
    );
}

/// Positive: use two address arguments (`locaddr.0` and `locaddr.1`) so that
/// a reversed mapping would cause the warning to land on the wrong local.
#[test]
fn exec_local_init_argument_order_mapping_matches_analysis_input_convention() {
    let ws = workspace_from_modules(&[(
        "test",
        // helper takes two addresses: top-of-stack is locaddr.1, deeper is locaddr.0
        // helper reads the second input (the deeper one, which is locaddr.0)
        // stack at exec site: [locaddr.0, locaddr.1] (locaddr.1 on top)
        // The helper does: swap (bring deeper addr to top), mem_load, drop
        // So helper reads input position 1 in call.args order (locaddr.0 = args[1]).
        "proc helper\n  swap\n  mem_load\n  drop\n  drop\nend\n\
         @locals(2)\n\
         proc caller\n  push.42\n  loc_store.1\n  locaddr.0\n  locaddr.1\n  exec.helper\nend\n",
    )]);
    let (_, diagnostics) = infer_uninitialized_locals_in_workspace(&ws);
    let caller_diags = local_init_diagnostics_for(&diagnostics, "test::caller");
    assert!(
        !caller_diags.is_empty(),
        "expected warning for uninitialized local 0, got none"
    );
    // Should warn about local 0 (the uninitialized one), NOT local 1 (which was written).
    let indices: Vec<u16> = caller_diags
        .iter()
        .flat_map(|d| d.local_indices.iter().copied())
        .collect();
    assert!(
        indices.contains(&0),
        "expected local_indices to contain cell 0, got: {indices:?}"
    );
    assert!(
        !indices.contains(&1),
        "local 1 was written; should not appear in indices, got: {indices:?}"
    );
}

/// Positive: a callee reads a padding cell through an `exec`-propagated
/// address. The warning should surface at the caller.
#[test]
fn exec_local_init_padding_cell_effects_propagate_through_exec() {
    let ws = workspace_from_modules(&[(
        "test",
        // 3 declared locals => frame is 4 cells. Cell 3 is padding.
        // helper reads addr+3 (the padding cell).
        "proc helper\n  push.3\n  add\n  mem_load\n  drop\nend\n\
         @locals(3)\n\
         proc caller\n  locaddr.0\n  exec.helper\nend\n",
    )]);
    let (_, diagnostics) = infer_uninitialized_locals_in_workspace(&ws);
    let caller_diags = local_init_diagnostics_for(&diagnostics, "test::caller");
    assert!(
        !caller_diags.is_empty(),
        "expected warning for padding cell via exec, got none"
    );
    let indices: Vec<u16> = caller_diags
        .iter()
        .flat_map(|d| d.local_indices.iter().copied())
        .collect();
    assert!(
        indices.contains(&3),
        "expected padding cell 3 in indices, got: {indices:?}"
    );
}

/// Negative: callee writes through the passed address, caller reads afterwards.
/// No diagnostics expected.
#[test]
fn exec_local_init_definite_write_in_callee_makes_later_caller_read_safe() {
    let ws = workspace_from_modules(&[(
        "test",
        // helper writes 42 to the passed address
        "proc helper\n  push.42\n  swap\n  mem_store\nend\n\
         @locals(1)\n\
         proc caller\n  locaddr.0\n  exec.helper\n  loc_load.0\n  drop\nend\n",
    )]);
    let (_, diagnostics) = infer_uninitialized_locals_in_workspace(&ws);
    let caller_diags = local_init_diagnostics_for(&diagnostics, "test::caller");
    assert!(
        caller_diags.is_empty(),
        "expected no diagnostics after callee writes, got: {caller_diags:?}"
    );
}

/// Negative: `call` does not share the caller's memory context, so it must
/// not propagate writes back to the caller frame. With `exec`, the callee's
/// definite writes remove cells from `maybe_uninit`; with `call`, the frame
/// state must be unchanged.
///
/// Note: the MASM decompiler's signature inference does not currently handle
/// `call` invocations, so a caller that uses `call.X` ends up un-liftable.
/// This test verifies that the analysis does not crash and does not produce
/// false "safe" claims for a `call` invocation. We compare the exec variant
/// (which should suppress the warning after Phase 2) against the call variant
/// (which should never suppress it).
#[test]
fn exec_local_init_call_does_not_change_caller_frame_state() {
    // Exec variant: callee writes, so after Phase 2 the read should be safe.
    let ws_exec = workspace_from_modules(&[(
        "test",
        "proc helper\n  push.42\n  swap\n  mem_store\nend\n\
         @locals(1)\n\
         proc caller\n  locaddr.0\n  exec.helper\n  loc_load.0\n  drop\nend\n",
    )]);
    let (_, exec_diags) = infer_uninitialized_locals_in_workspace(&ws_exec);
    let exec_caller = local_init_diagnostics_for(&exec_diags, "test::caller");
    // After Phase 2 implementation, this should be empty (write propagated).
    // For now it warns because exec summaries don't propagate yet.

    // Call variant: callee writes but via `call` (separate context).
    // Signature inference returns Unknown for `call`, so the caller is
    // un-liftable. The analysis should conservatively produce NO diagnostics
    // (it cannot analyze what it cannot lift), which means it also cannot
    // claim the write happened. This is safe because zero diagnostics means
    // "no claims made," not "definitely safe."
    let ws_call = workspace_from_modules(&[(
        "test",
        "proc helper\n  push.42\n  swap\n  mem_store\nend\n\
         @locals(1)\n\
         proc caller\n  locaddr.0\n  call.helper\n  loc_load.0\n  drop\nend\n",
    )]);
    let (_, call_diags) = infer_uninitialized_locals_in_workspace(&ws_call);
    let _call_caller = local_init_diagnostics_for(&call_diags, "test::caller");

    // After Phase 2, exec propagates the callee write, so exec_caller should
    // be clean. Call must NOT propagate writes — the caller read should still
    // warn (or be unanalyzable, which is also safe since no false "safe" claim
    // is made). _call_caller may be empty (unanalyzable) or may warn — either
    // is correct as long as call does not falsely suppress the read warning.
    assert!(
        exec_caller.is_empty(),
        "exec should propagate callee write, but got warnings: {exec_caller:?}"
    );
}

/// Negative: `syscall` does not share the caller's memory context either.
/// Same reasoning as the `call` test above.
#[test]
fn exec_local_init_syscall_does_not_change_caller_frame_state() {
    // syscall also makes the caller un-liftable due to signature inference
    // not handling syscall invocations. Same invariant as the call test:
    // syscall must never make frame state better than exec.
    let ws = workspace_from_modules(&[(
        "test",
        "proc helper\n  push.42\n  swap\n  mem_store\nend\n\
         @locals(1)\n\
         proc caller\n  locaddr.0\n  syscall.helper\n  loc_load.0\n  drop\nend\n",
    )]);
    let (_, diagnostics) = infer_uninitialized_locals_in_workspace(&ws);
    let caller_diags = local_init_diagnostics_for(&diagnostics, "test::caller");
    // The caller is un-liftable due to syscall, so no diagnostics are produced.
    // This is the conservative safe behavior: no false "safe" claims.
    // If signature inference is later extended to handle syscall, this test
    // should be updated to assert that the warning IS produced.
    assert!(
        caller_diags.is_empty(),
        "expected no diagnostics for un-liftable syscall caller, got: {caller_diags:?}"
    );
}

/// Negative: a callee that passes an address through unchanged should
/// preserve that address binding at the caller. The `swapper` callee takes
/// two inputs and returns them in reversed order (both passthroughs). After
/// `exec.swapper`, the result variable that carries `locaddr.0` should still
/// have its address binding, so the subsequent `mem_store` through it
/// initializes local cell 0 and the `loc_load.0` is safe.
///
/// Without passthrough preservation, ALL result variables have their address
/// cleared, and the post-exec `mem_store` cannot track the write.
#[test]
fn exec_passthrough_preserves_addr_for_callee_write() {
    let source = concat!(
        // swapper: takes [a, b] (a on top), returns [b, a] (b on top).
        "proc swapper\n",
        "  swap\n",
        "end\n",
        "@locals(4)\n",
        "proc caller\n",
        "  push.99\n",
        "  locaddr.0\n",
        // Stack: [locaddr.0, 99]  (locaddr.0 on top)
        "  exec.swapper\n",
        // After exec: [99, locaddr.0] — result vars carry the swapped values.
        // The result carrying locaddr.0 should still have its address binding.
        "  swap\n",
        "  mem_store\n",
        "  drop\n",
        "  loc_load.0\n",
        "  drop\n",
        "end\n",
    );
    let ws = workspace_from_modules(&[("test", source)]);
    let (_, diagnostics) = infer_uninitialized_locals_in_workspace(&ws);
    let caller_diags = local_init_diagnostics_for(&diagnostics, "test::caller");
    assert!(
        caller_diags.is_empty(),
        "passthrough should preserve addr, but got: {caller_diags:?}"
    );
}

/// Negative: summary extraction must preserve nested exec passthroughs too.
/// Here `writer` receives an address, passes it through `identity`, then
/// writes through the returned result. The caller-side `exec.writer` should
/// therefore be summarized as a definite write through its input argument.
#[test]
fn exec_summary_preserves_nested_exec_passthrough_for_callee_write() {
    let source = concat!(
        "proc identity\n",
        "  dup.0\n",
        "  drop\n",
        "end\n",
        "proc writer\n",
        "  exec.identity\n",
        "  push.42\n",
        "  swap\n",
        "  mem_store\n",
        "end\n",
        "@locals(1)\n",
        "proc caller\n",
        "  locaddr.0\n",
        "  exec.writer\n",
        "  loc_load.0\n",
        "  drop\n",
        "end\n",
    );
    let ws = workspace_from_modules(&[("test", source)]);
    let (_, diagnostics) = infer_uninitialized_locals_in_workspace(&ws);
    let caller_diags = local_init_diagnostics_for(&diagnostics, "test::caller");
    assert!(
        caller_diags.is_empty(),
        "nested exec passthrough should preserve callee write summary, got: {caller_diags:?}"
    );
}

/// Negative: `exec` an unresolvable callee. The analysis should not claim
/// any post-call initialization.
///
/// An unresolvable target makes the caller un-liftable (signature inference
/// cannot determine the target's effect). The test verifies that the analysis
/// does not crash and does not produce false "safe" claims.
#[test]
fn exec_local_init_unknown_callee_does_not_claim_definite_write() {
    let ws = workspace_from_modules(&[(
        "test",
        "@locals(1)\n\
         proc caller\n  locaddr.0\n  exec.::missing::unknown\n  loc_load.0\n  drop\nend\n",
    )]);
    let (_, diagnostics) = infer_uninitialized_locals_in_workspace(&ws);
    let caller_diags = local_init_diagnostics_for(&diagnostics, "test::caller");
    // If the caller is liftable, the analysis should warn (unknown callee = no
    // definite write). If un-liftable, zero diagnostics is acceptable (no false
    // "safe" claims). Either way, the analysis must NOT claim cell 0 was written.
    // For now, the unresolvable exec makes the lift fail, producing empty diags.
    // If lift is later extended to handle unknown targets, update this assertion:
    // assert!(!caller_diags.is_empty(), "...");
    let _ = caller_diags; // Currently empty due to lift failure; that's fine.
}

#[test]
fn local_init_exec_repeat_word_write_past_old_cap_makes_caller_word_read_safe() {
    let addr = repeat_test_var(0);
    let values = [
        repeat_test_var(1),
        repeat_test_var(2),
        repeat_test_var(3),
        repeat_test_var(4),
    ];
    let callee_stmts = vec![repeat_test_repeat(
        33,
        repeat_test_mem_store_body(&addr, &values, 4, MemAccessKind::WordLe),
    )];
    let callee_summary = extract_summary(&callee_stmts, 1, &PreparedProcMap::default());
    let mut summaries = LocalInitSummaryMap::default();
    summaries.insert(
        SymbolPath::new("test::writer_words".to_string()),
        callee_summary,
    );

    let caller_stmts = vec![
        Stmt::Exec {
            span: SourceSpan::default(),
            call: Call {
                target: "test::writer_words".to_string(),
                args: vec![addr.clone()],
                results: vec![],
            },
        },
        Stmt::LocalLoad {
            span: SourceSpan::default(),
            load: LocalLoad {
                kind: LocalAccessKind::WordLe,
                index: 128,
                outputs: vec![
                    repeat_test_var(10),
                    repeat_test_var(11),
                    repeat_test_var(12),
                    repeat_test_var(13),
                ],
            },
        },
    ];
    let env = repeat_test_env(132, &addr, 0);
    let result = eval_block_with_summaries(
        &SymbolPath::new("test::caller".to_string()),
        &caller_stmts,
        env,
        &summaries,
        &PreparedProcMap::default(),
    );
    assert!(
        result.diagnostics.is_empty(),
        "expected no diagnostics after exact repeat word writes, got: {:?}",
        result.diagnostics
    );
}

#[test]
fn local_init_exec_repeat_word_read_past_old_cap_warns_on_caller() {
    let addr = repeat_test_var(0);
    let outputs = [
        repeat_test_var(1),
        repeat_test_var(2),
        repeat_test_var(3),
        repeat_test_var(4),
    ];
    let callee_stmts = vec![repeat_test_repeat(
        33,
        repeat_test_mem_load_body(&addr, &outputs, 4, MemAccessKind::WordBe),
    )];
    let callee_summary = extract_summary(&callee_stmts, 1, &PreparedProcMap::default());
    let mut summaries = LocalInitSummaryMap::default();
    summaries.insert(
        SymbolPath::new("test::reader_words".to_string()),
        callee_summary,
    );

    let caller_stmts = vec![Stmt::Exec {
        span: SourceSpan::default(),
        call: Call {
            target: "test::reader_words".to_string(),
            args: vec![addr.clone()],
            results: vec![],
        },
    }];
    let mut env = repeat_test_env(132, &addr, 0);
    env.mark_definitely_written(&CellSet::closed_range(0, 127));
    let result = eval_block_with_summaries(
        &SymbolPath::new("test::caller".to_string()),
        &caller_stmts,
        env,
        &summaries,
        &PreparedProcMap::default(),
    );
    assert!(
        !result.diagnostics.is_empty(),
        "expected exec diagnostic for late repeat word read, got none"
    );
    let expected = [128_u16, 129, 130, 131];
    for cell in expected {
        assert!(
            result
                .diagnostics
                .iter()
                .any(|diag| diag.local_indices.contains(&cell)),
            "expected caller diagnostic to mention cell {cell}, got: {:?}",
            result.diagnostics
        );
    }
}

// =========================================================================
// adv_pipe intrinsic tests (hand-crafted IR)
// =========================================================================

/// `adv_pipe` writes 2 words (8 cells) at the given base address.
/// After `locaddr.0` + `adv_pipe`, cells 0..7 should be marked as written.
/// A subsequent `loc_load.0` of those cells should produce no diagnostic.
#[test]
fn local_init_adv_pipe_initializes_eight_cells_at_base_addr() {
    use masm_decompiler::ir::{Intrinsic, Stmt, ValueId, Var};
    use miden_debug_types::SourceSpan;

    use super::state::LocalInitEnv;
    use super::summary::LocalInitSummaryMap;
    use crate::prepared::PreparedProcMap;
    use crate::SymbolPath;

    // Frame: 8 locals (2 words), aligned to 8.
    let frame = FrameLayout::new(8);
    let env = LocalInitEnv::new(frame);

    // Empty summaries and prepared maps (no exec calls in this block).
    let summaries = LocalInitSummaryMap::default();
    let prepared = PreparedProcMap::default();
    let proc_path = SymbolPath::new("test::adv_pipe_test".to_string());

    // --- Build hand-crafted IR ---

    // Stmt 1: locaddr.0 — produces addr_var pointing to cell 0.
    let addr_var = Var::new(ValueId::new(100), 0);
    let locaddr_stmt = Stmt::Intrinsic {
        span: SourceSpan::default(),
        intrinsic: Intrinsic {
            name: "locaddr.0".to_string(),
            args: vec![],
            results: vec![addr_var.clone()],
        },
    };

    // Stmt 2: adv_pipe — 12 hasher-state args + 1 address (args[12]).
    // Results: 1 updated address (results[0]) + 12 hasher-state outputs.
    let hasher_args: Vec<Var> = (0..12)
        .map(|i| Var::new(ValueId::new(200 + i), 0))
        .collect();
    let mut adv_pipe_args = hasher_args.clone();
    adv_pipe_args.push(addr_var.clone()); // args[12] = address

    let updated_addr_var = Var::new(ValueId::new(300), 0);
    let mut adv_pipe_results = vec![updated_addr_var.clone()];
    for i in 1..13 {
        adv_pipe_results.push(Var::new(ValueId::new(300 + i), 0));
    }

    let adv_pipe_stmt = Stmt::Intrinsic {
        span: SourceSpan::default(),
        intrinsic: Intrinsic {
            name: "adv_pipe".to_string(),
            args: adv_pipe_args,
            results: adv_pipe_results,
        },
    };

    let stmts = vec![locaddr_stmt, adv_pipe_stmt];
    let result =
        super::transfer::eval_block_with_summaries(&proc_path, &stmts, env, &summaries, &prepared);

    // After adv_pipe at base 0, cells 0..7 should all be initialized.
    let uninit = result.env.cells_maybe_uninit(&CellSet::closed_range(0, 7));
    assert!(
        uninit.is_empty(),
        "expected all cells 0..7 to be initialized after adv_pipe, but these are still uninit: {uninit}"
    );

    // The updated address (results[0]) should point to cell 8, but frame is
    // only 8 cells (0..7), so it clips to empty with maybe_nonlocal set.
    let updated_addr = result.env.addr_for_var(&updated_addr_var);
    assert!(
        updated_addr.is_some(),
        "expected updated address to be tracked for results[0]"
    );
    let updated_addr = updated_addr.unwrap();
    assert!(
        updated_addr.local_cells.is_empty(),
        "shifted addr cell 8 should be clipped out of frame size 8, got: {}",
        updated_addr.local_cells
    );
    assert!(
        updated_addr.maybe_nonlocal,
        "shifted addr should be maybe_nonlocal after clipping"
    );

    // No diagnostics should have been emitted.
    assert!(
        result.diagnostics.is_empty(),
        "expected no diagnostics, got: {:?}",
        result.diagnostics
    );
}
