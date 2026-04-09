//! Transfer functions and control-flow walker for uninitialized-local-read analysis.
//!
//! This module walks the SSA IR statements of a procedure, updating the
//! [`LocalInitEnv`] at each point and emitting diagnostics when a read may
//! observe an uninitialized local frame cell.

use std::collections::BTreeMap;

use masm_decompiler::ir::{
    BinOp, Constant, Expr, LocalAccessKind, MemAccessKind, Stmt, ValueId, Var, VarBase,
};
use miden_debug_types::SourceSpan;

use crate::prepared::PreparedProcMap;
use crate::SymbolPath;

use super::domain::{AccessFootprint, AddrAbs, CellSet, FrameLayout};
use super::state::LocalInitEnv;
use super::summary::{
    LocalInitDiagnostic, LocalInitDiagnosticKind, LocalInitSummary, LocalInitSummaryMap,
};

/// Maximum number of fixed-point iterations for while loops.
const MAX_WHILE_PASSES: usize = 32;

/// Frame size used during summary inference. Large enough to avoid clipping
/// input-derived address offsets in practice. Only matters for summary
/// extraction — the actual caller frame size is used during diagnostics.
const SUMMARY_FRAME_SIZE: u16 = 256;

/// Result of evaluating a block of statements.
pub(crate) struct EvalResult {
    /// Environment after the block.
    pub(crate) env: LocalInitEnv,
    /// Diagnostics emitted during evaluation.
    pub(crate) diagnostics: Vec<LocalInitDiagnostic>,
}

/// Abstract carried binding for one repeat-loop phi across iterations.
#[derive(Debug, Clone, Default)]
struct RepeatCarriedValue {
    /// Abstract address tracked for the carried value, if any.
    addr: Option<AddrAbs>,
    /// Known constant tracked for the carried value, if any.
    constant: Option<u64>,
}

/// Snapshot used to restore transient repeat-template bindings.
#[derive(Debug, Clone)]
struct RepeatBindingRestore {
    /// Template variable rebound for the current iteration.
    var: Var,
    /// Previous tracked address for `var`, if any.
    prev_addr: Option<AddrAbs>,
    /// Previous tracked constant for `var`, if any.
    prev_constant: Option<u64>,
}

// ---------------------------------------------------------------------------
// Environment seeding
// ---------------------------------------------------------------------------

/// Create the initial environment for a procedure (intraprocedural analysis).
///
/// All frame cells start as maybe-uninitialized. No variable addresses are
/// tracked for the procedure inputs (they come from the caller's stack, not
/// from local memory).
pub(crate) fn seed_input_env(_input_count: usize, frame: FrameLayout) -> LocalInitEnv {
    LocalInitEnv::new(frame)
}

/// Seed input-cell tracking for summary inference.
///
/// For each input position, creates a synthetic `AddrAbs` with
/// `input_cells = { analysis_idx: singleton(0) }`, meaning "cell 0
/// relative to analysis input `analysis_idx`." The `local_cells` field
/// is left empty because during summary inference we don't know the
/// caller's frame.
///
/// Input variables are synthesized using the same scheme as the decompiler's
/// lifting pass: `Var::new(ValueId(depth), depth)` where `depth` goes from
/// 0 (deepest input) to `input_count - 1` (topmost input). The analysis
/// input position is `input_count - 1 - depth` (deepest = position 0).
pub(crate) fn seed_input_addr_cells(input_count: usize, env: &mut LocalInitEnv) {
    use masm_decompiler::ir::{ValueId, Var};
    for depth in 0..input_count {
        let analysis_idx = input_count - 1 - depth;
        let var = Var::new(ValueId::new(depth as u64), depth);
        let mut input_cells = BTreeMap::new();
        input_cells.insert(analysis_idx, CellSet::singleton(0));
        let addr = AddrAbs {
            local_cells: CellSet::empty(),
            maybe_nonlocal: true,
            input_cells,
        };
        env.set_addr_for_var(&var, addr);
    }
}

// ---------------------------------------------------------------------------
/// Walk a sequence of statements with exec summary lookups.
pub(crate) fn eval_block_with_summaries(
    proc_path: &SymbolPath,
    stmts: &[Stmt],
    mut env: LocalInitEnv,
    summaries: &LocalInitSummaryMap,
    prepared: &PreparedProcMap,
) -> EvalResult {
    let mut diagnostics = Vec::new();
    for stmt in stmts {
        let result = eval_stmt(proc_path, stmt, env, Some(summaries), prepared);
        env = result.env;
        diagnostics.extend(result.diagnostics);
    }
    EvalResult { env, diagnostics }
}

/// Clear all tracked bindings for a variable.
fn clear_var_bindings(var: &Var, env: &mut LocalInitEnv) {
    env.clear_addr_for_var(var);
    env.clear_constant_for_var(var);
}

/// Set an address binding and clear any stale constant binding.
fn set_var_addr(var: &Var, addr: AddrAbs, env: &mut LocalInitEnv) {
    env.set_addr_for_var(var, addr);
    env.clear_constant_for_var(var);
}

/// Capture the tracked bindings currently associated with `var`.
fn capture_var_bindings(env: &LocalInitEnv, var: &Var) -> RepeatCarriedValue {
    RepeatCarriedValue {
        addr: env.addr_for_var(var).cloned(),
        constant: env.constant_for_var(var),
    }
}

/// Capture the initial carried bindings for one repeat phi.
///
/// Summary extraction seeds procedure inputs as synthetic `ValueId(depth)`
/// variables. If a test or lifted repeat exposes `phi.init` as a `LoopInput`
/// before the body has run, fall back to the corresponding synthetic input.
fn capture_initial_repeat_carried_value(
    env: &LocalInitEnv,
    phi: &masm_decompiler::ir::LoopPhi,
) -> RepeatCarriedValue {
    let direct = capture_var_bindings(env, &phi.init);
    if direct.addr.is_some() || direct.constant.is_some() {
        return direct;
    }

    match phi.init.base {
        VarBase::LoopInput { .. } => {
            let fallback = Var::new(ValueId::new(phi.init.stack_depth as u64), phi.init.stack_depth);
            capture_var_bindings(env, &fallback)
        }
        VarBase::Value(_) => direct,
    }
}

/// Apply carried bindings to a variable, clearing missing components.
fn apply_repeat_carried_value(var: &Var, carried: &RepeatCarriedValue, env: &mut LocalInitEnv) {
    if let Some(addr) = carried.addr.clone() {
        env.set_addr_for_var(var, addr);
    } else {
        env.clear_addr_for_var(var);
    }

    if let Some(constant) = carried.constant {
        env.set_constant_for_var(var, constant);
    } else {
        env.clear_constant_for_var(var);
    }
}

/// Preserve tracked passthrough bindings for call results and clear all others.
fn apply_passthrough_result_bindings(
    call: &masm_decompiler::ir::Call,
    env: &mut LocalInitEnv,
    prepared: &PreparedProcMap,
) {
    let callee_path = SymbolPath::new(call.target.to_string());
    let callee_map = prepared.get(&callee_path).map(|p| &p.passthrough_map);
    for (idx, result) in call.results.iter().enumerate() {
        if let Some(Some(input_idx)) = callee_map.and_then(|m| m.get(idx)) {
            // Passthrough: inherit address from the corresponding argument.
            // input_idx uses 0=deepest; call.args uses 0=topmost.
            let arg_idx = call.args.len().checked_sub(1 + *input_idx);
            if let Some(arg) = arg_idx.and_then(|i| call.args.get(i)) {
                let carried = capture_var_bindings(env, arg);
                apply_repeat_carried_value(result, &carried, env);
                continue;
            }
        }
        clear_var_bindings(result, env);
    }
}

/// Extend `dest` with diagnostics from `src`, skipping duplicates.
fn extend_unique_diagnostics(
    dest: &mut Vec<LocalInitDiagnostic>,
    src: impl IntoIterator<Item = LocalInitDiagnostic>,
) {
    for diagnostic in src {
        let already_present = dest.iter().any(|existing| {
            existing.procedure == diagnostic.procedure
                && existing.span == diagnostic.span
                && existing.kind == diagnostic.kind
                && existing.message == diagnostic.message
                && existing.local_indices == diagnostic.local_indices
                && existing.related == diagnostic.related
        });
        if !already_present {
            dest.push(diagnostic);
        }
    }
}

// ---------------------------------------------------------------------------
// Statement evaluation
// ---------------------------------------------------------------------------

/// Evaluate a single statement.
fn eval_stmt(
    proc_path: &SymbolPath,
    stmt: &Stmt,
    mut env: LocalInitEnv,
    summaries: Option<&LocalInitSummaryMap>,
    prepared: &PreparedProcMap,
) -> EvalResult {
    match stmt {
        Stmt::Assign { dest, expr, .. } => {
            assign_expr_addr(dest, expr, &mut env);
            EvalResult {
                env,
                diagnostics: Vec::new(),
            }
        }

        Stmt::LocalLoad { span, load } => {
            let diags = apply_local_load(proc_path, *span, load, &env);
            // Clear address for output variables (loaded values are data, not addresses).
            for out in &load.outputs {
                clear_var_bindings(out, &mut env);
            }
            EvalResult {
                env,
                diagnostics: diags,
            }
        }

        Stmt::LocalStore { store, .. } => {
            apply_local_store(store, &mut env);
            EvalResult {
                env,
                diagnostics: Vec::new(),
            }
        }

        Stmt::LocalStoreW { store, .. } => {
            apply_local_store_word(store, &mut env);
            EvalResult {
                env,
                diagnostics: Vec::new(),
            }
        }

        Stmt::MemLoad { span, load } => {
            let diags = apply_mem_load(proc_path, *span, load, &env);
            // Clear address for output variables.
            for out in &load.outputs {
                clear_var_bindings(out, &mut env);
            }
            EvalResult {
                env,
                diagnostics: diags,
            }
        }

        Stmt::MemStore { store, .. } => {
            apply_mem_store(store, &mut env);
            EvalResult {
                env,
                diagnostics: Vec::new(),
            }
        }

        Stmt::Intrinsic { intrinsic, .. } => {
            // Check for locaddr.N intrinsics.
            if let Some(index) = intrinsic
                .name
                .strip_prefix("locaddr.")
                .and_then(|s| s.parse::<u16>().ok())
            {
                // locaddr.N produces an address pointing to local cell N.
                let addr = AddrAbs::exact_local(CellSet::singleton(index));
                for result in &intrinsic.results {
                    set_var_addr(result, addr.clone(), &mut env);
                }
            } else if intrinsic.name == "adv_pipe"
                && intrinsic.args.len() == 13  // 12 hasher-state + 1 address
                && intrinsic.results.len() == 13
            // 12 hasher-state + 1 updated address
            {
                // adv_pipe writes 2 words (8 cells) at the address in args[12].
                // results[0] is the updated address (base + 8).
                if let Some(base_addr) = env.addr_for_var(&intrinsic.args[12]).cloned() {
                    apply_adv_pipe_write(&base_addr, &mut env);
                    let shifted = adv_pipe_shifted_result(&base_addr, env.frame.aligned);
                    set_var_addr(&intrinsic.results[0], shifted, &mut env);
                }

                // Clear address for all other results (the hasher state outputs).
                for result in &intrinsic.results[1..] {
                    clear_var_bindings(result, &mut env);
                }
            } else {
                // Unknown intrinsics: clear address tracking for results.
                for result in &intrinsic.results {
                    clear_var_bindings(result, &mut env);
                }
            }
            EvalResult {
                env,
                diagnostics: Vec::new(),
            }
        }

        Stmt::Exec { span, call } => {
            let diags = apply_exec(proc_path, *span, call, &mut env, summaries);
            apply_passthrough_result_bindings(call, &mut env, prepared);
            EvalResult {
                env,
                diagnostics: diags,
            }
        }

        Stmt::Call { call, .. } | Stmt::SysCall { call, .. } => {
            // call and syscall create new memory contexts, so they do NOT
            // propagate local-frame effects back to the caller.
            for result in &call.results {
                clear_var_bindings(result, &mut env);
            }
            EvalResult {
                env,
                diagnostics: Vec::new(),
            }
        }

        Stmt::DynCall { results, .. } => {
            for result in results {
                clear_var_bindings(result, &mut env);
            }
            EvalResult {
                env,
                diagnostics: Vec::new(),
            }
        }

        Stmt::If {
            then_body,
            else_body,
            phis,
            ..
        } => eval_if(
            proc_path, then_body, else_body, phis, env, summaries, prepared,
        ),

        Stmt::While { body, phis, .. } => {
            eval_while_loop(proc_path, body, phis, env, summaries, prepared)
        }

        Stmt::Repeat {
            loop_count,
            body,
            phis,
            ..
        } => eval_repeat_loop(proc_path, *loop_count, body, phis, env, summaries, prepared),

        // Statements with no local-memory effects.
        Stmt::AdvLoad { load, .. } => {
            for out in &load.outputs {
                clear_var_bindings(out, &mut env);
            }
            EvalResult {
                env,
                diagnostics: Vec::new(),
            }
        }
        Stmt::AdvStore { .. } | Stmt::Return { .. } => EvalResult {
            env,
            diagnostics: Vec::new(),
        },
    }
}

// ---------------------------------------------------------------------------
// Exec summary application
// ---------------------------------------------------------------------------

/// Apply an `exec` call, using the callee's summary if available.
///
/// For `exec`, the callee shares the caller's memory context, so:
/// 1. Reads-before-write in the callee may observe uninitialized cells.
/// 2. Definite writes in the callee initialize cells for the caller.
///
/// If no summary is available (unknown callee), the call is treated
/// conservatively: no writes are claimed, no reads-before-write are reported.
fn apply_exec(
    proc_path: &SymbolPath,
    span: SourceSpan,
    call: &masm_decompiler::ir::Call,
    env: &mut LocalInitEnv,
    summaries: Option<&LocalInitSummaryMap>,
) -> Vec<LocalInitDiagnostic> {
    let mut diagnostics = Vec::new();

    let summary = summaries
        .and_then(|s| s.get(&SymbolPath::new(call.target.to_string())))
        .filter(|s| !s.unknown);

    let Some(summary) = summary else {
        // Unknown callee: conservative (no effects).
        return diagnostics;
    };

    let input_count = call.args.len();

    // Check reads-before-write: for each input position in the summary,
    // map the callee's input-relative cells to the caller's frame and check
    // if any are still uninitialized.
    for (input_idx, callee_cells) in &summary.reads_before_write {
        if callee_cells.is_empty() {
            continue;
        }
        // Map callee input_idx to call.args index.
        let Some(arg_index) = invert_input_index_for_call_args(input_count, *input_idx) else {
            continue;
        };
        let Some(arg_var) = call.args.get(arg_index) else {
            continue;
        };
        let Some(caller_addr) = env.addr_for_var(arg_var).cloned() else {
            continue;
        };

        // Map callee cells to caller cells.
        let caller_cells = map_summary_cells_to_caller(callee_cells, &caller_addr);
        if caller_cells.is_empty() {
            continue;
        }

        let uninit = env.cells_maybe_uninit(&caller_cells);
        if !uninit.is_empty() {
            let indices: Vec<u16> = uninit.iter_cells().collect();
            diagnostics.push(LocalInitDiagnostic {
                procedure: proc_path.clone(),
                span,
                kind: LocalInitDiagnosticKind::ExecReadBeforeWrite,
                message: format!(
                    "exec.{} may read uninitialized local cell(s) {:?} via argument",
                    call.target, indices
                ),
                local_indices: indices,
                related: Vec::new(),
            });
        }
    }

    // Apply definite writes: for each input position in the summary,
    // map the callee's definite-write cells to the caller's frame and
    // mark them as initialized.
    for (input_idx, callee_cells) in &summary.definitely_writes {
        if callee_cells.is_empty() {
            continue;
        }
        let Some(arg_index) = invert_input_index_for_call_args(input_count, *input_idx) else {
            continue;
        };
        let Some(arg_var) = call.args.get(arg_index) else {
            continue;
        };
        let Some(caller_addr) = env.addr_for_var(arg_var).cloned() else {
            continue;
        };

        let caller_cells = map_summary_cells_to_caller(callee_cells, &caller_addr);
        if !caller_cells.is_empty() {
            env.mark_definitely_written(&caller_cells);
        }
    }

    diagnostics
}

/// Convert callee input-relative cells to caller-local cells.
///
/// A callee summary says "I read/write cell offset X through input position I."
/// The caller passed `caller_addr` as argument I. If `caller_addr` points to
/// local cell `B`, then the callee's cell offset `X` corresponds to caller
/// cell `B + X`.
fn map_summary_cells_to_caller(callee_cells: &CellSet, caller_addr: &AddrAbs) -> CellSet {
    if caller_addr.local_cells.is_empty() {
        return CellSet::empty();
    }

    // For each base cell in the caller's address, shift the callee cells
    // and union the results.
    let mut result = CellSet::empty();
    for base in caller_addr.local_cells.iter_cells() {
        let shifted = callee_cells.shift_by(base);
        result = result.union(&shifted);
    }
    result
}

/// Map between analysis input positions and `call.args` indices.
///
/// The decompiler assigns analysis input positions as
/// `analysis_idx = input_count - 1 - depth`, where `depth = 0` is the
/// deepest stack element. The `call.args` array is ordered with the
/// topmost stack element at index 0, so `call.args[i]` corresponds to
/// callee depth `input_count - 1 - i`, and hence to analysis position `i`.
///
/// Therefore, the mapping is the identity: analysis input `i` corresponds
/// to `call.args[i]`.
///
/// Returns `None` if the input_idx is out of range.
fn invert_input_index_for_call_args(input_count: usize, input_idx: usize) -> Option<usize> {
    if input_idx >= input_count {
        return None;
    }
    Some(input_idx)
}

// ---------------------------------------------------------------------------
// Control flow evaluation
// ---------------------------------------------------------------------------

/// Evaluate an if/else branch.
fn eval_if(
    proc_path: &SymbolPath,
    then_body: &[Stmt],
    else_body: &[Stmt],
    phis: &[masm_decompiler::ir::IfPhi],
    env: LocalInitEnv,
    summaries: Option<&LocalInitSummaryMap>,
    prepared: &PreparedProcMap,
) -> EvalResult {
    let then_result = eval_block_inner(proc_path, then_body, env.clone(), summaries, prepared);
    let else_result = eval_block_inner(proc_path, else_body, env, summaries, prepared);

    let mut joined = then_result.env.join(&else_result.env);

    // Process phi nodes: join addresses from both branches.
    for phi in phis {
        assign_phi_bindings(
            &phi.dest,
            &phi.then_var,
            &then_result.env,
            &phi.else_var,
            &else_result.env,
            &mut joined,
        );
    }

    let mut diagnostics = then_result.diagnostics;
    diagnostics.extend(else_result.diagnostics);
    EvalResult {
        env: joined,
        diagnostics,
    }
}

/// Evaluate a while loop using fixed-point iteration.
///
/// A while loop may execute zero times, so the post-loop state must include
/// the pre-loop state (via join).
fn eval_while_loop(
    proc_path: &SymbolPath,
    body: &[Stmt],
    phis: &[masm_decompiler::ir::LoopPhi],
    env: LocalInitEnv,
    summaries: Option<&LocalInitSummaryMap>,
    prepared: &PreparedProcMap,
) -> EvalResult {
    let mut current = env.clone();
    let mut all_diagnostics = Vec::new();

    for _ in 0..MAX_WHILE_PASSES {
        let body_result = eval_block_inner(proc_path, body, current.clone(), summaries, prepared);
        let next = current.join(&body_result.env);

        if next == current {
            // Converged. Collect diagnostics from one final pass.
            all_diagnostics = body_result.diagnostics;
            current = next;
            break;
        }
        all_diagnostics = body_result.diagnostics;
        current = next;
    }

    // While may execute zero times: join with pre-loop state.
    let post_loop = env.join(&current);

    // Process loop phi nodes.
    let mut final_env = post_loop;
    for phi in phis {
        assign_phi_bindings(
            &phi.dest,
            &phi.init,
            &env,
            &phi.step,
            &current,
            &mut final_env,
        );
    }

    EvalResult {
        env: final_env,
        diagnostics: all_diagnostics,
    }
}

/// Evaluate a repeat loop.
///
/// A repeat loop always executes at least once, so the post-loop state does
/// not need to include the pre-loop state.
fn eval_repeat_loop(
    proc_path: &SymbolPath,
    count: usize,
    body: &[Stmt],
    phis: &[masm_decompiler::ir::LoopPhi],
    env: LocalInitEnv,
    summaries: Option<&LocalInitSummaryMap>,
    prepared: &PreparedProcMap,
) -> EvalResult {
    let mut current = env.clone();
    let mut carried = capture_initial_repeat_carried_values(phis, &env);
    let mut all_diagnostics = Vec::new();

    for _ in 0..count {
        let mut iter_env = current.clone();
        let restores = seed_repeat_iteration_bindings(phis, &carried, &mut iter_env);

        let body_result = eval_block_inner(proc_path, body, iter_env, summaries, prepared);
        let mut next = body_result.env;
        let next_carried = update_repeat_carried_values(phis, &next, &carried);
        restore_repeat_iteration_bindings(&mut next, restores);
        extend_unique_diagnostics(&mut all_diagnostics, body_result.diagnostics);

        if next == current {
            // Converged early.
            current = next;
            break;
        }
        current = next;
        carried = next_carried;
    }

    // A repeat loop executes at least once, so the exit binding is the final
    // carried value, not a join with the pre-loop template binding.
    for (phi, carried_value) in phis.iter().zip(carried.iter()) {
        apply_repeat_carried_value(&phi.dest, carried_value, &mut current);
    }

    EvalResult {
        env: current,
        diagnostics: all_diagnostics,
    }
}

/// Internal block evaluator that threads the summaries parameter through.
fn eval_block_inner(
    proc_path: &SymbolPath,
    stmts: &[Stmt],
    mut env: LocalInitEnv,
    summaries: Option<&LocalInitSummaryMap>,
    prepared: &PreparedProcMap,
) -> EvalResult {
    let mut diagnostics = Vec::new();
    for stmt in stmts {
        let result = eval_stmt(proc_path, stmt, env, summaries, prepared);
        env = result.env;
        diagnostics.extend(result.diagnostics);
    }
    EvalResult { env, diagnostics }
}

// ---------------------------------------------------------------------------
// Address tracking
// ---------------------------------------------------------------------------

/// Track address propagation through an assignment expression.
///
/// - `Expr::Var(v)`: copy the address from `v`.
/// - `Expr::Binary(Add, lhs, rhs)`: if one side is an address and the other
///   a known constant, shift the address by the constant and clip to the frame.
/// - Anything else: clear the address for `dest`.
fn assign_expr_addr(dest: &Var, expr: &Expr, env: &mut LocalInitEnv) {
    // Track constant values through assignments for addr+const folding.
    match expr {
        Expr::Constant(Constant::Felt(v)) => {
            env.set_constant_for_var(dest, *v);
        }
        Expr::Var(v) => {
            if let Some(c) = env.constant_for_var(v) {
                env.set_constant_for_var(dest, c);
            } else {
                env.clear_constant_for_var(dest);
            }
        }
        _ => env.clear_constant_for_var(dest),
    }

    match expr {
        Expr::Var(v) => {
            if let Some(addr) = env.addr_for_var(v).cloned() {
                env.set_addr_for_var(dest, addr);
            } else {
                env.clear_addr_for_var(dest);
            }
        }

        Expr::Binary(BinOp::Add, lhs, rhs) => {
            // Try: addr + constant or constant + addr.
            let addr_and_offset =
                try_addr_plus_const(lhs, rhs, env).or_else(|| try_addr_plus_const(rhs, lhs, env));

            if let Some((base_addr, offset)) = addr_and_offset {
                let shifted = base_addr.local_cells.shift_by(offset);
                let clipped = shifted.clip_to_frame(env.frame.aligned);
                let leaves_frame = shifted != clipped || base_addr.maybe_nonlocal;
                // Propagate input_cells with the same shift.
                let input_cells = base_addr
                    .input_cells
                    .iter()
                    .map(|(idx, cells)| (*idx, cells.shift_by(offset)))
                    .collect();
                set_var_addr(
                    dest,
                    AddrAbs {
                        local_cells: clipped,
                        maybe_nonlocal: leaves_frame,
                        input_cells,
                    },
                    env,
                );
            } else {
                // One side is addr but offset is not a constant:
                // conservatively mark as possibly pointing anywhere in frame.
                let has_addr = expr_has_addr(lhs, env) || expr_has_addr(rhs, env);
                if has_addr {
                    // Merge input_cells from both operands so summary inference
                    // can still track reads/writes through this address.
                    let mut input_cells = expr_input_cells(lhs, env);
                    for (idx, cells) in expr_input_cells(rhs, env) {
                        input_cells
                            .entry(idx)
                            .and_modify(|existing| *existing = existing.union(&cells))
                            .or_insert(cells);
                    }
                    set_var_addr(
                        dest,
                        AddrAbs {
                            local_cells: env.frame.all_cells(),
                            maybe_nonlocal: true,
                            input_cells,
                        },
                        env,
                    );
                } else {
                    env.clear_addr_for_var(dest);
                }
            }
        }

        // Subtraction and other binary ops on addresses are not tracked.
        Expr::Binary(_, lhs, rhs) => {
            let has_addr = expr_has_addr(lhs, env) || expr_has_addr(rhs, env);
            if has_addr {
                // Conservative: could point anywhere in the frame.
                // Preserve input_cells for summary inference.
                let mut input_cells = expr_input_cells(lhs, env);
                for (idx, cells) in expr_input_cells(rhs, env) {
                    input_cells
                        .entry(idx)
                        .and_modify(|existing| *existing = existing.union(&cells))
                        .or_insert(cells);
                }
                set_var_addr(
                    dest,
                    AddrAbs {
                        local_cells: env.frame.all_cells(),
                        maybe_nonlocal: true,
                        input_cells,
                    },
                    env,
                );
            } else {
                env.clear_addr_for_var(dest);
            }
        }

        _ => {
            env.clear_addr_for_var(dest);
        }
    }
}

/// Try to extract an (addr, constant-offset) pair from `(addr_expr, offset_expr)`.
///
/// The offset may come from either a literal `Expr::Constant` or from an
/// `Expr::Var` whose constant value is known via the environment's constant
/// tracker.
fn try_addr_plus_const(
    addr_expr: &Expr,
    offset_expr: &Expr,
    env: &LocalInitEnv,
) -> Option<(AddrAbs, u16)> {
    let addr = match addr_expr {
        Expr::Var(v) => env.addr_for_var(v)?.clone(),
        _ => return None,
    };
    let offset = match offset_expr {
        Expr::Constant(Constant::Felt(v)) => {
            let v = *v;
            u16::try_from(v).ok()?
        }
        Expr::Var(v) => {
            let c = env.constant_for_var(v)?;
            u16::try_from(c).ok()?
        }
        _ => return None,
    };
    Some((addr, offset))
}

/// Check whether an expression references a variable with a tracked address.
fn expr_has_addr(expr: &Expr, env: &LocalInitEnv) -> bool {
    match expr {
        Expr::Var(v) => env.addr_for_var(v).is_some(),
        _ => false,
    }
}

/// Extract `input_cells` from an expression's address, if any.
///
/// Used in conservative fallback branches where `try_addr_plus_const` fails
/// but we still need to preserve input-cell tracking for summary inference.
fn expr_input_cells(expr: &Expr, env: &LocalInitEnv) -> BTreeMap<usize, CellSet> {
    match expr {
        Expr::Var(v) => env
            .addr_for_var(v)
            .map(|a| a.input_cells.clone())
            .unwrap_or_default(),
        _ => BTreeMap::new(),
    }
}

/// Join tracked bindings for a phi node from two branch environments.
fn assign_phi_bindings(
    dest: &Var,
    lhs_var: &Var,
    lhs_env: &LocalInitEnv,
    rhs_var: &Var,
    rhs_env: &LocalInitEnv,
    target_env: &mut LocalInitEnv,
) {
    match (lhs_env.addr_for_var(lhs_var), rhs_env.addr_for_var(rhs_var)) {
        (Some(lhs_addr), Some(rhs_addr)) => {
            target_env.set_addr_for_var(dest, lhs_addr.join(rhs_addr));
        }
        _ => {
            target_env.clear_addr_for_var(dest);
        }
    }

    match (lhs_env.constant_for_var(lhs_var), rhs_env.constant_for_var(rhs_var)) {
        (Some(lhs), Some(rhs)) if lhs == rhs => target_env.set_constant_for_var(dest, lhs),
        _ => target_env.clear_constant_for_var(dest),
    }
}

/// Capture the initial carried bindings for all repeat-loop phis.
fn capture_initial_repeat_carried_values(
    phis: &[masm_decompiler::ir::LoopPhi],
    env: &LocalInitEnv,
) -> Vec<RepeatCarriedValue> {
    phis.iter()
        .map(|phi| capture_initial_repeat_carried_value(env, phi))
        .collect()
}

/// Seed repeat-loop destinations and transient template aliases for one iteration.
fn seed_repeat_iteration_bindings(
    phis: &[masm_decompiler::ir::LoopPhi],
    carried: &[RepeatCarriedValue],
    target_env: &mut LocalInitEnv,
) -> Vec<RepeatBindingRestore> {
    let mut restores = Vec::with_capacity(phis.len());
    for (phi, carried_value) in phis.iter().zip(carried.iter()) {
        restores.push(RepeatBindingRestore {
            var: phi.init.clone(),
            prev_addr: target_env.addr_for_var(&phi.init).cloned(),
            prev_constant: target_env.constant_for_var(&phi.init),
        });
        apply_repeat_carried_value(&phi.dest, carried_value, target_env);
        apply_repeat_carried_value(&phi.init, carried_value, target_env);
    }
    restores
}

/// Restore transient repeat-template bindings after one iteration body run.
fn restore_repeat_iteration_bindings(
    env: &mut LocalInitEnv,
    restores: Vec<RepeatBindingRestore>,
) {
    for restore in restores {
        if let Some(addr) = restore.prev_addr {
            env.set_addr_for_var(&restore.var, addr);
        } else {
            env.clear_addr_for_var(&restore.var);
        }

        if let Some(constant) = restore.prev_constant {
            env.set_constant_for_var(&restore.var, constant);
        } else {
            env.clear_constant_for_var(&restore.var);
        }
    }
}

/// Compute the carried bindings that should feed the next repeat iteration.
fn update_repeat_carried_values(
    phis: &[masm_decompiler::ir::LoopPhi],
    post_body_env: &LocalInitEnv,
    _previous_carried: &[RepeatCarriedValue],
) -> Vec<RepeatCarriedValue> {
    phis.iter()
        .map(|phi| capture_var_bindings(post_body_env, &phi.step))
        .collect()
}

// ---------------------------------------------------------------------------
// Local load/store transfer functions
// ---------------------------------------------------------------------------

/// Check a local load for possibly-uninitialized reads and emit diagnostics.
fn apply_local_load(
    proc_path: &SymbolPath,
    span: SourceSpan,
    load: &masm_decompiler::ir::LocalLoad,
    env: &LocalInitEnv,
) -> Vec<LocalInitDiagnostic> {
    let mut diagnostics = Vec::new();

    match load.kind {
        LocalAccessKind::Element => {
            let cell = CellSet::singleton(load.index);
            let uninit = env.cells_maybe_uninit(&cell);
            if !uninit.is_empty() {
                diagnostics.push(LocalInitDiagnostic {
                    procedure: proc_path.clone(),
                    span,
                    kind: LocalInitDiagnosticKind::LocalRead,
                    message: format!("local {} may be read before initialization", load.index),
                    local_indices: uninit.iter_cells().collect(),
                    related: Vec::new(),
                });
            }
        }
        LocalAccessKind::WordBe | LocalAccessKind::WordLe => {
            let cells = CellSet::closed_range(load.index, load.index.saturating_add(3));
            let uninit = env.cells_maybe_uninit(&cells);
            if !uninit.is_empty() {
                diagnostics.push(LocalInitDiagnostic {
                    procedure: proc_path.clone(),
                    span,
                    kind: LocalInitDiagnosticKind::LocalWordRead,
                    message: format!(
                        "local word starting at {} may be read before initialization",
                        load.index
                    ),
                    local_indices: uninit.iter_cells().collect(),
                    related: Vec::new(),
                });
            }
        }
    }

    diagnostics
}

/// Apply a scalar local store: mark the stored cell as definitely written.
fn apply_local_store(store: &masm_decompiler::ir::LocalStore, env: &mut LocalInitEnv) {
    let cell = CellSet::singleton(store.index);
    env.mark_definitely_written(&cell);
}

/// Apply a word local store: mark all 4 cells as definitely written.
fn apply_local_store_word(store: &masm_decompiler::ir::LocalStoreW, env: &mut LocalInitEnv) {
    match store.kind {
        LocalAccessKind::WordBe | LocalAccessKind::WordLe => {
            let cells = CellSet::closed_range(store.index, store.index.saturating_add(3));
            env.mark_definitely_written(&cells);
        }
        LocalAccessKind::Element => {
            let cell = CellSet::singleton(store.index);
            env.mark_definitely_written(&cell);
        }
    }
}

// ---------------------------------------------------------------------------
// Memory load/store transfer functions
// ---------------------------------------------------------------------------

/// Resolve the abstract address from the first address operand of a memory
/// operation, if one is tracked.
fn resolve_addr_abs(address: &[Var], env: &LocalInitEnv) -> Option<AddrAbs> {
    let addr = address.first().and_then(|v| env.addr_for_var(v)).cloned()?;
    if addr.local_cells.is_empty() && addr.input_cells.is_empty() {
        return None;
    }
    Some(addr)
}

/// Check a memory load for possibly-uninitialized reads via local addresses.
fn apply_mem_load(
    proc_path: &SymbolPath,
    span: SourceSpan,
    load: &masm_decompiler::ir::MemLoad,
    env: &LocalInitEnv,
) -> Vec<LocalInitDiagnostic> {
    let mut diagnostics = Vec::new();

    let Some(addr) = resolve_addr_abs(&load.address, env) else {
        return diagnostics;
    };

    // Only check local_cells for diagnostics (input_cells are for summary inference).
    if !addr.local_cells.is_empty() {
        let fp = match load.kind {
            MemAccessKind::Element => AccessFootprint::for_scalar(&addr),
            MemAccessKind::WordBe | MemAccessKind::WordLe => AccessFootprint::for_word(&addr),
        };

        let uninit = env.cells_maybe_uninit(&fp.may_cells);
        if !uninit.is_empty() {
            let indices: Vec<u16> = uninit.iter_cells().collect();
            diagnostics.push(LocalInitDiagnostic {
                procedure: proc_path.clone(),
                span,
                kind: LocalInitDiagnosticKind::MemReadViaLocalAddr,
                message: format!(
                    "memory read via local address may observe uninitialized frame cell(s) {:?}",
                    indices
                ),
                local_indices: indices,
                related: Vec::new(),
            });
        }
    }

    diagnostics
}

/// Apply a memory store via a local address: mark cells as definitely written.
fn apply_mem_store(store: &masm_decompiler::ir::MemStore, env: &mut LocalInitEnv) {
    let Some(addr) = resolve_addr_abs(&store.address, env) else {
        return;
    };

    if !addr.local_cells.is_empty() {
        let fp = match store.kind {
            MemAccessKind::Element => AccessFootprint::for_scalar(&addr),
            MemAccessKind::WordBe | MemAccessKind::WordLe => AccessFootprint::for_word(&addr),
        };

        env.mark_definitely_written(&fp.must_cells);
    }
}

/// Mark the 8 local cells at `base_addr` as definitely written (for `adv_pipe`).
///
/// Only marks cells when the address is an exact singleton local (not
/// maybe-nonlocal). This is conservative: imprecise addresses produce
/// no definite write.
fn apply_adv_pipe_write(base_addr: &AddrAbs, env: &mut LocalInitEnv) {
    if base_addr.local_cells.is_empty() {
        return;
    }
    let definite = !base_addr.maybe_nonlocal && base_addr.local_cells.len() == 1;
    if definite {
        for base in base_addr.local_cells.iter_cells() {
            let window = CellSet::closed_range(base, base.saturating_add(7));
            let clipped = window.clip_to_frame(env.frame.aligned);
            env.mark_definitely_written(&clipped);
        }
    }
}

/// Compute the shifted result address for `adv_pipe` (`base + 8`).
///
/// Clips the shifted cells to the frame. Sets `maybe_nonlocal` if clipping
/// removed any cells or the base was already maybe-nonlocal.
fn adv_pipe_shifted_result(base_addr: &AddrAbs, frame_aligned: u16) -> AddrAbs {
    let shifted_cells = base_addr.local_cells.shift_by(8);
    let clipped = shifted_cells.clip_to_frame(frame_aligned);
    AddrAbs {
        local_cells: clipped.clone(),
        maybe_nonlocal: base_addr.maybe_nonlocal || shifted_cells != clipped,
        input_cells: base_addr
            .input_cells
            .iter()
            .map(|(k, v)| (*k, v.shift_by(8)))
            .collect(),
    }
}

// ---------------------------------------------------------------------------
// Summary extraction helpers
// ---------------------------------------------------------------------------

/// Extract a [`LocalInitSummary`] by analyzing a procedure's lifted stmts.
///
/// This runs the summary-inference variant of the analysis, which tracks
/// reads and writes through input-derived addresses using `input_cells`.
pub(crate) fn extract_summary(
    stmts: &[Stmt],
    input_count: usize,
    prepared: &PreparedProcMap,
) -> LocalInitSummary {
    // We analyze the procedure with a dummy frame that represents the
    // maximum possible frame we might encounter. During summary inference,
    // all frame effects happen through input-derived addresses.
    // We use a large frame to avoid clipping issues.
    let frame = FrameLayout::new(SUMMARY_FRAME_SIZE);
    let mut env = LocalInitEnv::new(frame);

    // Seed each input variable with input_cells tracking.
    seed_input_addr_cells(input_count, &mut env);

    // Walk statements and collect reads/writes through input-derived addresses.
    let mut reads_before_write: BTreeMap<usize, CellSet> = BTreeMap::new();
    let mut definitely_writes: BTreeMap<usize, CellSet> = BTreeMap::new();

    extract_summary_from_block(
        stmts,
        &mut env,
        &mut reads_before_write,
        &mut definitely_writes,
        prepared,
    );

    LocalInitSummary::known(reads_before_write, definitely_writes)
}

/// Recursively extract summary information from a block of statements.
fn extract_summary_from_block(
    stmts: &[Stmt],
    env: &mut LocalInitEnv,
    reads_before_write: &mut BTreeMap<usize, CellSet>,
    definitely_writes: &mut BTreeMap<usize, CellSet>,
    prepared: &PreparedProcMap,
) {
    for stmt in stmts {
        extract_summary_from_stmt(stmt, env, reads_before_write, definitely_writes, prepared);
    }
}

/// Process a single statement for summary extraction.
fn extract_summary_from_stmt(
    stmt: &Stmt,
    env: &mut LocalInitEnv,
    reads_before_write: &mut BTreeMap<usize, CellSet>,
    definitely_writes: &mut BTreeMap<usize, CellSet>,
    prepared: &PreparedProcMap,
) {
    match stmt {
        Stmt::Assign { dest, expr, .. } => {
            assign_expr_addr(dest, expr, env);
        }

        Stmt::MemLoad { load, .. } => {
            // Check if this load reads through an input-derived address.
            if let Some(addr) = resolve_addr_abs(&load.address, env) {
                record_input_read(
                    &addr,
                    &load.kind,
                    env,
                    reads_before_write,
                    definitely_writes,
                );
            }
            // Clear address for output variables.
            for out in &load.outputs {
                clear_var_bindings(out, env);
            }
        }

        Stmt::MemStore { store, .. } => {
            // Check if this store writes through an input-derived address.
            if let Some(addr) = resolve_addr_abs(&store.address, env) {
                record_input_write(&addr, &store.kind, definitely_writes);
            }
            // Also apply the write to the local frame tracking.
            apply_mem_store(store, env);
        }

        Stmt::LocalLoad { load, .. } => {
            // Local loads don't involve input-derived addresses.
            for out in &load.outputs {
                clear_var_bindings(out, env);
            }
        }

        Stmt::LocalStore { store, .. } => {
            apply_local_store(store, env);
        }

        Stmt::LocalStoreW { store, .. } => {
            apply_local_store_word(store, env);
        }

        Stmt::Intrinsic { intrinsic, .. } => {
            if let Some(index) = intrinsic
                .name
                .strip_prefix("locaddr.")
                .and_then(|s| s.parse::<u16>().ok())
            {
                let addr = AddrAbs::exact_local(CellSet::singleton(index));
                for result in &intrinsic.results {
                    set_var_addr(result, addr.clone(), env);
                }
            } else if intrinsic.name == "adv_pipe"
                && intrinsic.args.len() == 13  // 12 hasher-state + 1 address
                && intrinsic.results.len() == 13
            // 12 hasher-state + 1 updated address
            {
                // adv_pipe writes 2 words (8 cells) at the address in args[12].
                // results[0] is the updated address (base + 8).
                if let Some(base_addr) = env.addr_for_var(&intrinsic.args[12]).cloned() {
                    // Record input-cell writes for summary inference.
                    // Model as two WordBe writes: base and base + 4.
                    record_input_write(&base_addr, &MemAccessKind::WordBe, definitely_writes);
                    let addr_plus_4 = AddrAbs {
                        local_cells: base_addr.local_cells.shift_by(4),
                        maybe_nonlocal: base_addr.maybe_nonlocal,
                        input_cells: base_addr
                            .input_cells
                            .iter()
                            .map(|(k, v)| (*k, v.shift_by(4)))
                            .collect(),
                    };
                    record_input_write(&addr_plus_4, &MemAccessKind::WordBe, definitely_writes);

                    // Mark local cells as written in the env too.
                    apply_adv_pipe_write(&base_addr, env);
                    let shifted = adv_pipe_shifted_result(&base_addr, env.frame.aligned);
                    set_var_addr(&intrinsic.results[0], shifted, env);
                }

                // Clear address for all other results (the hasher state outputs).
                for result in &intrinsic.results[1..] {
                    clear_var_bindings(result, env);
                }
            } else {
                for result in &intrinsic.results {
                    clear_var_bindings(result, env);
                }
            }
        }

        Stmt::Exec { call, .. } => {
            // Nested execs do not apply local-init summaries here, but their
            // passthrough results still need address provenance so later writes
            // through those results contribute to this procedure's summary.
            apply_passthrough_result_bindings(call, env, prepared);
        }

        Stmt::Call { call, .. } | Stmt::SysCall { call, .. } => {
            // call/syscall use distinct memory contexts, so returned values
            // are treated as opaque for local-frame summary extraction.
            for result in &call.results {
                clear_var_bindings(result, env);
            }
        }

        Stmt::DynCall { results, .. } => {
            for result in results {
                clear_var_bindings(result, env);
            }
        }

        Stmt::If {
            then_body,
            else_body,
            phis,
            ..
        } => {
            // For summary inference, we need to analyze both branches.
            let mut then_env = env.clone();
            let mut then_rbw = reads_before_write.clone();
            let mut then_dw = definitely_writes.clone();
            extract_summary_from_block(
                then_body,
                &mut then_env,
                &mut then_rbw,
                &mut then_dw,
                prepared,
            );

            let mut else_env = env.clone();
            let mut else_rbw = reads_before_write.clone();
            let mut else_dw = definitely_writes.clone();
            extract_summary_from_block(
                else_body,
                &mut else_env,
                &mut else_rbw,
                &mut else_dw,
                prepared,
            );

            // Join environments.
            *env = then_env.join(&else_env);

            // Merge reads-before-write: union from both branches.
            for (idx, cells) in then_rbw {
                reads_before_write
                    .entry(idx)
                    .and_modify(|existing| *existing = existing.union(&cells))
                    .or_insert(cells);
            }
            for (idx, cells) in else_rbw {
                reads_before_write
                    .entry(idx)
                    .and_modify(|existing| *existing = existing.union(&cells))
                    .or_insert(cells);
            }

            // Merge definite writes: intersection (must be written on ALL paths).
            let all_keys: std::collections::BTreeSet<usize> =
                then_dw.keys().chain(else_dw.keys()).copied().collect();
            for idx in all_keys {
                let then_cells = then_dw.get(&idx).cloned().unwrap_or_else(CellSet::empty);
                let else_cells = else_dw.get(&idx).cloned().unwrap_or_else(CellSet::empty);
                let intersected = then_cells.intersection(&else_cells);
                if !intersected.is_empty() {
                    definitely_writes.insert(idx, intersected);
                }
            }

            // Process phi nodes.
            for phi in phis {
                assign_phi_bindings(
                    &phi.dest,
                    &phi.then_var,
                    &then_env,
                    &phi.else_var,
                    &else_env,
                    env,
                );
            }
        }

        Stmt::While { body, phis, .. } => {
            // Conservative: while may execute zero times.
            let pre_env = env.clone();
            let mut loop_env = env.clone();
            for _ in 0..MAX_WHILE_PASSES {
                let mut iter_env = loop_env.clone();
                extract_summary_from_block(
                    body,
                    &mut iter_env,
                    reads_before_write,
                    definitely_writes,
                    prepared,
                );
                let joined = loop_env.join(&iter_env);
                if joined == loop_env {
                    break;
                }
                loop_env = joined;
            }
            // Join with pre-loop (while may not execute).
            *env = pre_env.join(&loop_env);

            for phi in phis {
                assign_phi_bindings(&phi.dest, &phi.init, &pre_env, &phi.step, &loop_env, env);
            }
        }

        Stmt::Repeat {
            loop_count,
            body,
            phis,
            ..
        } => {
            let mut current = env.clone();
            let mut carried = capture_initial_repeat_carried_values(phis, env);
            for _ in 0..*loop_count {
                let mut iter_env = current.clone();
                let restores = seed_repeat_iteration_bindings(phis, &carried, &mut iter_env);
                extract_summary_from_block(
                    body,
                    &mut iter_env,
                    reads_before_write,
                    definitely_writes,
                    prepared,
                );
                let next_carried = update_repeat_carried_values(phis, &iter_env, &carried);
                restore_repeat_iteration_bindings(&mut iter_env, restores);
                current = iter_env;
                carried = next_carried;
            }

            for (phi, carried_value) in phis.iter().zip(carried.iter()) {
                apply_repeat_carried_value(&phi.dest, carried_value, &mut current);
            }
            *env = current;
        }

        Stmt::AdvLoad { load, .. } => {
            for out in &load.outputs {
                clear_var_bindings(out, env);
            }
        }
        Stmt::AdvStore { .. } | Stmt::Return { .. } => {}
    }
}

/// Record a read through an input-derived address for summary extraction.
///
/// Checks if the address has `input_cells` and whether the corresponding cells
/// have been written yet. If not, records a read-before-write.
fn record_input_read(
    addr: &AddrAbs,
    kind: &MemAccessKind,
    _env: &LocalInitEnv,
    reads_before_write: &mut BTreeMap<usize, CellSet>,
    definitely_writes: &BTreeMap<usize, CellSet>,
) {
    for (input_idx, input_cell_set) in &addr.input_cells {
        // Compute which input-relative cells this load accesses.
        let accessed = match kind {
            MemAccessKind::Element => input_cell_set.clone(),
            MemAccessKind::WordBe | MemAccessKind::WordLe => {
                // Word access: each base cell accesses base..base+3.
                let mut union = CellSet::empty();
                for base in input_cell_set.iter_cells() {
                    union = union.union(&CellSet::closed_range(base, base.saturating_add(3)));
                }
                union
            }
        };

        // Remove cells that were already definitely written.
        let already_written = definitely_writes
            .get(input_idx)
            .cloned()
            .unwrap_or_else(CellSet::empty);
        let unwritten = accessed.difference(&already_written);

        if !unwritten.is_empty() {
            reads_before_write
                .entry(*input_idx)
                .and_modify(|existing| *existing = existing.union(&unwritten))
                .or_insert(unwritten);
        }
    }
}

/// Record a write through an input-derived address for summary extraction.
///
/// For input_cells tracking, the `maybe_nonlocal` flag on the `AddrAbs` is
/// irrelevant: input_cells tracks effects relative to the input position,
/// not relative to the local frame. The definiteness of a write depends only
/// on whether the input_cell_set targets exactly one cell per input position.
fn record_input_write(
    addr: &AddrAbs,
    kind: &MemAccessKind,
    definitely_writes: &mut BTreeMap<usize, CellSet>,
) {
    for (input_idx, input_cell_set) in &addr.input_cells {
        // Only count definite writes when the input_cell_set targets
        // exactly one cell (i.e., we know precisely which cell is written).
        let written = match kind {
            MemAccessKind::Element => {
                if input_cell_set.len() == 1 {
                    input_cell_set.clone()
                } else {
                    CellSet::empty()
                }
            }
            MemAccessKind::WordBe | MemAccessKind::WordLe => {
                if input_cell_set.len() == 1 {
                    let base = input_cell_set.iter_cells().next().unwrap();
                    CellSet::closed_range(base, base.saturating_add(3))
                } else {
                    CellSet::empty()
                }
            }
        };

        if !written.is_empty() {
            definitely_writes
                .entry(*input_idx)
                .and_modify(|existing| *existing = existing.union(&written))
                .or_insert(written);
        }
    }
}
