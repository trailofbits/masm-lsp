//! Interprocedural provenance summaries for unconstrained advice.

use masm_decompiler::{
    ir::{LocalAccessKind, Stmt},
    SymbolPath,
};

use super::{
    shared::{
        apply_intrinsic_effect, apply_local_load_scalar, apply_local_load_word, apply_local_store,
        apply_local_store_word, assign_expr_metadata, assign_phi_metadata, expr_output_fact,
        refine_if_envs, seed_input_env, Env, MAX_LOOP_PASSES,
    },
    summary::{AdviceSummary, AdviceSummaryMap},
};

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
    phis: &[masm_decompiler::ir::LoopPhi],
    entry_env: Env,
    callee_summaries: &AdviceSummaryMap,
) -> EvalResult {
    let mut loop_env = entry_env.clone();
    let mut opaque = false;

    for _ in 0..MAX_LOOP_PASSES {
        let body_result = eval_block(body, loop_env.clone(), callee_summaries);
        opaque |= body_result.opaque;

        let mut next_env = loop_env.join(&body_result.env);
        for phi in phis {
            let merged = entry_env
                .fact_for_var(&phi.init)
                .join(&body_result.env.fact_for_var(&phi.step));
            next_env.set_var_fact(&phi.dest, merged);
            assign_phi_metadata(
                &phi.dest,
                &phi.init,
                &entry_env,
                &phi.step,
                &body_result.env,
                &mut next_env,
            );
        }

        if next_env == loop_env {
            loop_env = next_env;
            break;
        }
        loop_env = next_env;
    }

    EvalResult {
        env: loop_env,
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
