//! Interprocedural provenance summaries for unconstrained advice.

use masm_decompiler::{
    ir::{LocalAccessKind, LoopPhi, Stmt},
    SymbolPath,
};

use super::{
    shared::{
        apply_intrinsic_effect, apply_local_load_scalar, apply_local_load_word, apply_local_store,
        apply_local_store_word, assign_expr_metadata, assign_phi_metadata, expr_output_fact,
        join_loop_head_env, refine_if_envs, seed_input_env, Env, MAX_LOOP_PASSES,
    },
    summary::{AdviceSummary, AdviceSummaryMap},
};
use crate::abstract_interp::{iterate_to_fixpoint, FixpointConfig, JoinSemiLattice};

/// Analyze one lifted procedure and summarize which outputs may carry unconstrained advice.
pub(crate) fn analyze_proc_provenance(
    input_count: usize,
    output_count: usize,
    stmts: &[Stmt],
    callee_summaries: &AdviceSummaryMap,
) -> AdviceSummary {
    let env = seed_input_env(input_count);
    let result = eval_block(stmts, env, callee_summaries);
    if result.opaque {
        AdviceSummary::unknown_with_arity(output_count)
    } else {
        build_summary(stmts, output_count, &result.env)
    }
}

/// Result of evaluating a statement block.
#[derive(Debug, Clone)]
struct EvalResult {
    env: Env,
    opaque: bool,
}

/// Loop-head state driven by the generic fixpoint engine.
///
/// The engine still operates on `join_assign`, but loop phi updates are part of the loop-head
/// transition itself rather than a second generic environment join.
#[derive(Clone)]
struct ProvenanceLoopState<'a> {
    env: Env,
    entry_env: &'a Env,
    phis: &'a [LoopPhi],
}

impl<'a> ProvenanceLoopState<'a> {
    /// Build the initial loop-head state from the loop entry environment.
    fn at_loop_head(entry_env: &'a Env, phis: &'a [LoopPhi]) -> Self {
        Self {
            env: entry_env.clone(),
            entry_env,
            phis,
        }
    }

    /// Build the candidate state produced by one loop-body evaluation.
    fn from_body_env(body_env: Env, entry_env: &'a Env, phis: &'a [LoopPhi]) -> Self {
        Self {
            env: body_env,
            entry_env,
            phis,
        }
    }

    /// Consume the loop state and return its loop-head environment.
    fn into_env(self) -> Env {
        self.env
    }
}

impl JoinSemiLattice for ProvenanceLoopState<'_> {
    fn join_assign(&mut self, other: &Self) -> bool {
        let next_env = join_loop_head_env(&self.env, self.entry_env, &other.env, self.phis);
        let changed = self.env != next_env;
        self.env = next_env;
        changed
    }
}

/// Build the final output summary from the return statement.
fn build_summary(stmts: &[Stmt], output_count: usize, env: &Env) -> AdviceSummary {
    let Some(values) = stmts.iter().find_map(|stmt| match stmt {
        Stmt::Return { values, .. } => Some(values.as_slice()),
        _ => None,
    }) else {
        return AdviceSummary::new(vec![super::domain::AdviceFact::bottom(); output_count]);
    };

    let mut outputs = Vec::with_capacity(output_count);
    for index in (0..output_count).rev() {
        let fact = values
            .get(index)
            .map(|var| env.fact_for_var(var))
            .unwrap_or_else(super::domain::AdviceFact::bottom);
        outputs.push(fact);
    }
    AdviceSummary::new(outputs)
}

/// Evaluate a statement block from top to bottom.
fn eval_block(stmts: &[Stmt], mut env: Env, callee_summaries: &AdviceSummaryMap) -> EvalResult {
    let mut opaque = false;

    for stmt in stmts {
        let result = eval_stmt(stmt, env, callee_summaries);
        env = result.env;
        opaque |= result.opaque;
    }

    EvalResult { env, opaque }
}

/// Evaluate a single statement.
fn eval_stmt(stmt: &Stmt, mut env: Env, callee_summaries: &AdviceSummaryMap) -> EvalResult {
    let mut opaque = false;

    match stmt {
        Stmt::Assign { dest, expr, .. } => {
            let fact = expr_output_fact(expr, &env);
            env.set_var_fact(dest, fact);
            assign_expr_metadata(dest, expr, &mut env);
        }
        Stmt::AdvLoad { span, load } => {
            for output in &load.outputs {
                env.set_var_fact(output, super::domain::AdviceFact::from_source(*span));
                env.clear_var_metadata(output);
            }
        }
        Stmt::AdvStore { .. } | Stmt::MemStore { .. } | Stmt::Return { .. } => {}
        Stmt::MemLoad { load, .. } => {
            for output in &load.outputs {
                env.set_var_fact(output, super::domain::AdviceFact::bottom());
                env.clear_var_metadata(output);
            }
        }
        Stmt::LocalStore { store, .. } => {
            apply_local_store(&store.values, u32::from(store.index), &mut env);
        }
        Stmt::LocalStoreW { store, .. } => {
            apply_local_store_word(store.kind, &store.values, u32::from(store.index), &mut env);
        }
        Stmt::LocalLoad { load, .. } => match load.kind {
            LocalAccessKind::Element => {
                apply_local_load_scalar(&load.outputs, u32::from(load.index), &mut env);
            }
            LocalAccessKind::WordBe | LocalAccessKind::WordLe => {
                apply_local_load_word(load.kind, &load.outputs, u32::from(load.index), &mut env);
            }
        },
        Stmt::Call { call, .. } | Stmt::Exec { call, .. } | Stmt::SysCall { call, .. } => {
            assign_call_results(
                &mut env,
                &call.target,
                &call.args,
                &call.results,
                callee_summaries,
            );
        }
        Stmt::DynCall { results, .. } => {
            for result in results {
                env.set_var_fact(result, super::domain::AdviceFact::bottom());
                env.clear_var_metadata(result);
            }
            opaque = true;
        }
        Stmt::Intrinsic { span, intrinsic } => {
            apply_intrinsic_effect(*span, intrinsic, &mut env);
        }
        Stmt::If {
            cond,
            then_body,
            else_body,
            phis,
            ..
        } => {
            let (then_env, else_env) = refine_if_envs(cond, &env);
            let then_result = eval_block(then_body, then_env, callee_summaries);
            let else_result = eval_block(else_body, else_env, callee_summaries);
            opaque |= then_result.opaque || else_result.opaque;

            env = then_result.env.join(&else_result.env);
            for phi in phis {
                let merged = then_result
                    .env
                    .fact_for_var(&phi.then_var)
                    .join(&else_result.env.fact_for_var(&phi.else_var));
                env.set_var_fact(&phi.dest, merged);
                assign_phi_metadata(
                    &phi.dest,
                    &phi.then_var,
                    &then_result.env,
                    &phi.else_var,
                    &else_result.env,
                    &mut env,
                );
            }
        }
        Stmt::While { body, phis, .. } | Stmt::Repeat { body, phis, .. } => {
            let loop_result = eval_loop_block(body, phis, env, callee_summaries);
            env = loop_result.env;
            opaque |= loop_result.opaque;
        }
    }

    EvalResult { env, opaque }
}

/// Evaluate a structured loop body conservatively.
fn eval_loop_block(
    body: &[Stmt],
    phis: &[LoopPhi],
    entry_env: Env,
    callee_summaries: &AdviceSummaryMap,
) -> EvalResult {
    let mut opaque = false;
    let result = iterate_to_fixpoint(
        ProvenanceLoopState::at_loop_head(&entry_env, phis),
        FixpointConfig::new(MAX_LOOP_PASSES),
        |loop_env| {
            let body_result = eval_block(body, loop_env.env.clone(), callee_summaries);
            opaque |= body_result.opaque;
            ProvenanceLoopState::from_body_env(body_result.env, &entry_env, phis)
        },
    );

    EvalResult {
        env: result.into_state().into_env(),
        opaque,
    }
}

/// Assign call-result facts by substituting caller arguments into callee summaries.
pub(crate) fn assign_call_results(
    env: &mut Env,
    target: &str,
    args: &[masm_decompiler::ir::Var],
    results: &[masm_decompiler::ir::Var],
    callee_summaries: &AdviceSummaryMap,
) {
    let Some(summary) = callee_summaries.get(&SymbolPath::new(target.to_string())) else {
        for result in results {
            env.set_var_fact(result, super::domain::AdviceFact::bottom());
            env.clear_var_metadata(result);
        }
        return;
    };
    if summary.is_unknown() {
        for result in results {
            env.set_var_fact(result, super::domain::AdviceFact::bottom());
            env.clear_var_metadata(result);
        }
        return;
    }

    let arg_facts = args
        .iter()
        .map(|arg| env.fact_for_var(arg))
        .collect::<Vec<_>>();
    for (result, summary_fact) in results.iter().zip(summary.outputs.iter()) {
        env.set_var_fact(result, substitute_output_fact(summary_fact, &arg_facts));
        env.clear_var_metadata(result);
    }
    for result in results.iter().skip(summary.outputs.len()) {
        env.set_var_fact(result, super::domain::AdviceFact::bottom());
        env.clear_var_metadata(result);
    }
}

/// Substitute caller argument facts into a callee output summary fact.
fn substitute_output_fact(
    summary_fact: &super::domain::AdviceFact,
    arg_facts: &[super::domain::AdviceFact],
) -> super::domain::AdviceFact {
    let mut substituted = super::domain::AdviceFact::bottom();
    substituted.source_spans = summary_fact.source_spans.clone();
    for input_index in &summary_fact.from_inputs {
        if let Some(arg_fact) = arg_facts.get(*input_index) {
            substituted = substituted.join(arg_fact);
        }
    }
    substituted
}

#[cfg(test)]
mod tests {
    use super::ProvenanceLoopState;
    use crate::{
        abstract_interp::JoinSemiLattice,
        unconstrained_advice::shared::{join_loop_head_env, Env},
    };
    use masm_decompiler::{
        ir::{LoopPhi, Var},
        types::VarKey,
    };

    /// Return a small synthetic SSA variable for provenance-loop tests.
    fn test_var(index: u8) -> Var {
        Var::new(u64::from(index).into(), usize::from(index))
    }

    #[test]
    fn loop_state_join_preserves_phi_metadata_across_iterations() {
        let init = test_var(0);
        let step = test_var(1);
        let dest = test_var(2);
        let phi = LoopPhi {
            dest: dest.clone(),
            init: init.clone(),
            step: step.clone(),
        };

        let mut entry_env = Env::default();
        let shared_identity = VarKey::from_var(&init);
        entry_env.set_var_identity(&init, shared_identity.clone());

        let mut body_env = Env::default();
        body_env.set_var_identity(&step, shared_identity.clone());

        let loop_head = join_loop_head_env(
            &entry_env,
            &entry_env,
            &body_env,
            std::slice::from_ref(&phi),
        );
        assert_eq!(loop_head.identity_for_var(&dest), shared_identity);

        let mut state =
            ProvenanceLoopState::at_loop_head(&entry_env, std::slice::from_ref(&phi));
        let candidate = ProvenanceLoopState::from_body_env(
            body_env.clone(),
            &entry_env,
            std::slice::from_ref(&phi),
        );

        assert!(state.join_assign(&candidate));
        assert_eq!(state.env.identity_for_var(&dest), shared_identity);
    }
}
