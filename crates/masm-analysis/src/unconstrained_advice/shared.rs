//! Shared state and transfer helpers for unconstrained-advice analyses.

use std::collections::{HashMap, HashSet};

use masm_decompiler::{
    ir::{BinOp, Expr, Intrinsic, LocalAccessKind, LoopPhi, Stmt, UnOp, Var},
    types::VarKey,
};

use crate::abstract_interp::JoinSemiLattice;

use super::domain::AdviceFact;

/// Maximum number of loop-approximation passes.
pub(crate) const MAX_LOOP_PASSES: usize = 32;

/// Exact witness that a boolean value was computed as `x == 0`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct EqZeroWitness {
    /// Alias identity of the value compared against zero.
    pub(crate) value_identity: VarKey,
}

/// Shared flow environment at a program point.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub(crate) struct Env {
    vars: HashMap<VarKey, AdviceFact>,
    locals: HashMap<u32, AdviceFact>,
    aliases: HashMap<VarKey, VarKey>,
    local_aliases: HashMap<u32, VarKey>,
    zero_tests: HashMap<VarKey, EqZeroWitness>,
    local_zero_tests: HashMap<u32, EqZeroWitness>,
    nonzero_identities: HashSet<VarKey>,
}

impl Env {
    /// Read the current fact for a variable.
    pub(crate) fn fact_for_var(&self, var: &Var) -> AdviceFact {
        self.vars
            .get(&VarKey::from_var(var))
            .cloned()
            .unwrap_or_else(AdviceFact::bottom)
    }

    /// Set the current fact for a variable.
    pub(crate) fn set_var_fact(&mut self, var: &Var, fact: AdviceFact) {
        self.vars.insert(VarKey::from_var(var), fact);
    }

    /// Return the alias identity for a variable.
    pub(crate) fn identity_for_var(&self, var: &Var) -> VarKey {
        let key = VarKey::from_var(var);
        self.aliases.get(&key).cloned().unwrap_or(key)
    }

    /// Return the alias identity for a local slot, if known.
    pub(crate) fn identity_for_local(&self, slot: u32) -> Option<VarKey> {
        self.local_aliases.get(&slot).cloned()
    }

    /// Set the alias identity for a variable.
    pub(crate) fn set_var_identity(&mut self, var: &Var, identity: VarKey) {
        let key = VarKey::from_var(var);
        if identity == key {
            self.aliases.remove(&key);
        } else {
            self.aliases.insert(key, identity);
        }
    }

    /// Clear any alias identity for a variable.
    pub(crate) fn clear_var_identity(&mut self, var: &Var) {
        self.aliases.remove(&VarKey::from_var(var));
    }

    /// Set the alias identity for a local slot.
    pub(crate) fn set_local_identity(&mut self, slot: u32, identity: Option<VarKey>) {
        match identity {
            Some(identity) => {
                self.local_aliases.insert(slot, identity);
            }
            None => {
                self.local_aliases.remove(&slot);
            }
        }
    }

    /// Return the zero-test witness for a variable, if any.
    pub(crate) fn zero_test_for_var(&self, var: &Var) -> Option<EqZeroWitness> {
        self.zero_tests.get(&VarKey::from_var(var)).cloned()
    }

    /// Return the zero-test witness for a local slot, if any.
    pub(crate) fn zero_test_for_local(&self, slot: u32) -> Option<EqZeroWitness> {
        self.local_zero_tests.get(&slot).cloned()
    }

    /// Set the zero-test witness for a variable.
    pub(crate) fn set_var_zero_test(&mut self, var: &Var, witness: Option<EqZeroWitness>) {
        match witness {
            Some(witness) => {
                self.zero_tests.insert(VarKey::from_var(var), witness);
            }
            None => {
                self.zero_tests.remove(&VarKey::from_var(var));
            }
        }
    }

    /// Set the zero-test witness for a local slot.
    pub(crate) fn set_local_zero_test(&mut self, slot: u32, witness: Option<EqZeroWitness>) {
        match witness {
            Some(witness) => {
                self.local_zero_tests.insert(slot, witness);
            }
            None => {
                self.local_zero_tests.remove(&slot);
            }
        }
    }

    /// Return true if the variable is proven non-zero on the current path.
    pub(crate) fn is_var_nonzero(&self, var: &Var) -> bool {
        self.nonzero_identities
            .contains(&self.identity_for_var(var))
    }

    /// Mark the given alias identity as non-zero on the current path.
    pub(crate) fn mark_identity_nonzero(&mut self, identity: VarKey) {
        self.nonzero_identities.insert(identity);
    }

    /// Clear all best-effort metadata for a variable definition.
    pub(crate) fn clear_var_metadata(&mut self, var: &Var) {
        self.clear_var_identity(var);
        self.set_var_zero_test(var, None);
    }

    /// Sanitize a variable from this point onward.
    pub(crate) fn sanitize_var(&mut self, var: &Var) {
        self.set_var_fact(var, AdviceFact::bottom());
    }

    /// Read the current fact for a local slot.
    pub(crate) fn fact_for_local(&self, slot: u32) -> AdviceFact {
        self.locals
            .get(&slot)
            .cloned()
            .unwrap_or_else(AdviceFact::bottom)
    }

    /// Set the current fact for a local slot.
    pub(crate) fn set_local_fact(&mut self, slot: u32, fact: AdviceFact) {
        self.locals.insert(slot, fact);
    }

    /// Join two environments conservatively.
    pub(crate) fn join(&self, other: &Self) -> Self {
        let mut joined = self.clone();
        for (key, fact) in &other.vars {
            let current = joined
                .vars
                .get(key)
                .cloned()
                .unwrap_or_else(AdviceFact::bottom);
            joined.vars.insert(key.clone(), current.join(fact));
        }
        for (slot, fact) in &other.locals {
            let current = joined
                .locals
                .get(slot)
                .cloned()
                .unwrap_or_else(AdviceFact::bottom);
            joined.locals.insert(*slot, current.join(fact));
        }
        joined.aliases = agreeing_entries(&self.aliases, &other.aliases);
        joined.local_aliases = agreeing_entries(&self.local_aliases, &other.local_aliases);
        joined.zero_tests = agreeing_entries(&self.zero_tests, &other.zero_tests);
        joined.local_zero_tests = agreeing_entries(&self.local_zero_tests, &other.local_zero_tests);
        joined.nonzero_identities = self
            .nonzero_identities
            .intersection(&other.nonzero_identities)
            .cloned()
            .collect();
        joined
    }
}

impl JoinSemiLattice for Env {
    fn join_assign(&mut self, other: &Self) -> bool {
        let joined = self.join(other);
        let changed = *self != joined;
        *self = joined;
        changed
    }
}

/// Retain only entries that are present in both maps with the same value.
fn agreeing_entries<K, V>(lhs: &HashMap<K, V>, rhs: &HashMap<K, V>) -> HashMap<K, V>
where
    K: Clone + Eq + std::hash::Hash,
    V: Clone + Eq,
{
    lhs.iter()
        .filter_map(|(key, value)| {
            rhs.get(key)
                .filter(|other| *other == value)
                .map(|_| (key.clone(), value.clone()))
        })
        .collect()
}

/// Seed input variables using the same numbering scheme as the lifting pass.
pub(crate) fn seed_input_env(input_count: usize) -> Env {
    let mut env = Env::default();
    for depth in 0..input_count {
        let input_position = input_count - 1 - depth;
        let var = Var::new((depth as u64).into(), depth);
        env.set_var_fact(&var, AdviceFact::from_input(input_position));
    }
    env
}

/// Preserve alias and zero-test metadata across a fresh assignment.
pub(crate) fn assign_expr_metadata(dest: &Var, expr: &Expr, env: &mut Env) {
    if let Some(identity) = expr_identity(expr, env) {
        env.set_var_identity(dest, identity);
    } else {
        env.clear_var_identity(dest);
    }
    env.set_var_zero_test(dest, eq_zero_witness_for_expr(expr, env));
}

/// Preserve metadata across a phi only when both sides agree exactly.
pub(crate) fn assign_phi_metadata(
    dest: &Var,
    lhs_var: &Var,
    lhs_env: &Env,
    rhs_var: &Var,
    rhs_env: &Env,
    env: &mut Env,
) {
    let lhs_identity = lhs_env.identity_for_var(lhs_var);
    let rhs_identity = rhs_env.identity_for_var(rhs_var);
    if lhs_identity == rhs_identity {
        env.set_var_identity(dest, lhs_identity);
    } else {
        env.clear_var_identity(dest);
    }

    let lhs_witness = lhs_env.zero_test_for_var(lhs_var);
    let rhs_witness = rhs_env.zero_test_for_var(rhs_var);
    if lhs_witness.is_some() && lhs_witness == rhs_witness {
        env.set_var_zero_test(dest, lhs_witness);
    } else {
        env.set_var_zero_test(dest, None);
    }
}

/// Join one loop-body evaluation back into the current abstract loop state.
pub(crate) fn join_loop_head_env(
    loop_env: &Env,
    entry_env: &Env,
    body_env: &Env,
    phis: &[LoopPhi],
) -> Env {
    let mut next_env = loop_env.join(body_env);
    for phi in phis {
        let merged = entry_env
            .fact_for_var(&phi.init)
            .join(&body_env.fact_for_var(&phi.step));
        next_env.set_var_fact(&phi.dest, merged);
        assign_phi_metadata(
            &phi.dest,
            &phi.init,
            entry_env,
            &phi.step,
            body_env,
            &mut next_env,
        );
    }

    next_env
}

/// Refine branch environments using an exact `eq.0` witness when available.
pub(crate) fn refine_if_envs(cond: &Expr, env: &Env) -> (Env, Env) {
    let then_env = env.clone();
    let mut else_env = env.clone();
    if let Some(witness) = eq_zero_witness_for_expr(cond, env) {
        else_env.mark_identity_nonzero(witness.value_identity);
    }
    (then_env, else_env)
}

/// Refine the environment after `assertz` proves an `eq.0` witness is zero.
pub(crate) fn refine_nonzero_from_intrinsic(intrinsic: &Intrinsic, env: &mut Env) {
    if intrinsic_base_name(&intrinsic.name) != "assertz" {
        return;
    }
    let Some(arg) = intrinsic.args.first() else {
        return;
    };
    let Some(witness) = env.zero_test_for_var(arg) else {
        return;
    };
    env.mark_identity_nonzero(witness.value_identity);
}

/// Return the exact alias identity of an expression, when it is a simple copy.
pub(crate) fn expr_identity(expr: &Expr, env: &Env) -> Option<VarKey> {
    match expr {
        Expr::Var(var) => Some(env.identity_for_var(var)),
        _ => None,
    }
}

/// Return the exact `eq.0` witness carried by an expression, if any.
pub(crate) fn eq_zero_witness_for_expr(expr: &Expr, env: &Env) -> Option<EqZeroWitness> {
    match expr {
        Expr::Var(var) => env.zero_test_for_var(var),
        Expr::Binary(BinOp::Eq, lhs, rhs) => {
            zero_comparison_var(lhs, rhs).map(|var| EqZeroWitness {
                value_identity: env.identity_for_var(var),
            })
        }
        _ => None,
    }
}

/// Return true when the expression is proven non-zero by the best-effort refinement.
pub(crate) fn expr_is_proven_nonzero(expr: &Expr, env: &Env) -> bool {
    match expr {
        Expr::Constant(constant) => !constant.is_zero(),
        Expr::Var(var) => env.is_var_nonzero(var),
        _ => false,
    }
}

/// Compute the provenance fact for an expression result.
pub(crate) fn expr_output_fact(expr: &Expr, env: &Env) -> AdviceFact {
    match expr {
        Expr::Var(var) => env.fact_for_var(var),
        Expr::Ternary {
            then_expr,
            else_expr,
            ..
        } => expr_output_fact(then_expr, env).join(&expr_output_fact(else_expr, env)),
        Expr::Unary(op, inner) => match op {
            UnOp::Neg | UnOp::Inv | UnOp::Pow2 => expr_output_fact(inner, env),
            UnOp::Not
            | UnOp::U32Cast
            | UnOp::U32Test
            | UnOp::U32Not
            | UnOp::U32Clz
            | UnOp::U32Ctz
            | UnOp::U32Clo
            | UnOp::U32Cto => AdviceFact::bottom(),
        },
        Expr::Binary(op, lhs, rhs) => match op {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                expr_output_fact(lhs, env).join(&expr_output_fact(rhs, env))
            }
            BinOp::And
            | BinOp::Or
            | BinOp::Xor
            | BinOp::Eq
            | BinOp::Neq
            | BinOp::Lt
            | BinOp::Lte
            | BinOp::Gt
            | BinOp::Gte
            | BinOp::U32And
            | BinOp::U32Or
            | BinOp::U32Xor
            | BinOp::U32Shl
            | BinOp::U32Shr
            | BinOp::U32Rotr
            | BinOp::U32Lt
            | BinOp::U32Lte
            | BinOp::U32Gt
            | BinOp::U32Gte
            | BinOp::U32WrappingAdd
            | BinOp::U32WrappingSub
            | BinOp::U32WrappingMul
            | BinOp::U32Exp => AdviceFact::bottom(),
        },
        Expr::EqW { .. } | Expr::True | Expr::False | Expr::Constant(_) => AdviceFact::bottom(),
    }
}

/// Apply the common provenance transfer semantics of one intrinsic statement.
pub(crate) fn apply_intrinsic_effect(
    span: miden_debug_types::SourceSpan,
    intrinsic: &Intrinsic,
    env: &mut Env,
) {
    if intrinsic.name.starts_with("adv_push.") {
        for result in &intrinsic.results {
            env.set_var_fact(result, AdviceFact::from_source(span));
            env.clear_var_metadata(result);
        }
        return;
    }

    match intrinsic_base_name(&intrinsic.name) {
        "u32assert" | "u32assert2" | "u32assertw" => {
            for arg in &intrinsic.args {
                env.sanitize_var(arg);
            }
        }
        "u32split" => {
            for result in &intrinsic.results {
                env.set_var_fact(result, AdviceFact::bottom());
                env.clear_var_metadata(result);
            }
        }
        "is_odd" => {
            for result in &intrinsic.results {
                env.set_var_fact(result, AdviceFact::bottom());
                env.clear_var_metadata(result);
            }
        }
        "adv_pipe" => {
            apply_adv_pipe_effect(span, intrinsic, env);
        }
        "mem_stream" | "sdepth" => {
            for result in &intrinsic.results {
                env.set_var_fact(result, AdviceFact::bottom());
                env.clear_var_metadata(result);
            }
        }
        name if name.starts_with("locaddr") => {
            for result in &intrinsic.results {
                env.set_var_fact(result, AdviceFact::bottom());
                env.clear_var_metadata(result);
            }
        }
        _ if intrinsic_requires_u32_precondition(&intrinsic.name) => {
            for result in &intrinsic.results {
                env.set_var_fact(result, AdviceFact::bottom());
                env.clear_var_metadata(result);
            }
        }
        _ => {
            let joined =
                AdviceFact::join_all(intrinsic.args.iter().map(|arg| env.fact_for_var(arg)));
            for result in &intrinsic.results {
                env.set_var_fact(result, joined.clone());
                env.clear_var_metadata(result);
            }
        }
    }
}

/// Store a scalar local and preserve exact alias metadata when possible.
pub(crate) fn apply_local_store(values: &[Var], index: u32, env: &mut Env) {
    let fact = AdviceFact::join_all(values.iter().map(|var| env.fact_for_var(var)));
    env.set_local_fact(index, fact);
    let identity = single_var(values).map(|var| env.identity_for_var(var));
    let witness = single_var(values).and_then(|var| env.zero_test_for_var(var));
    env.set_local_identity(index, identity);
    env.set_local_zero_test(index, witness);
}

/// Store a local word slot-by-slot.
pub(crate) fn apply_local_store_word(
    kind: LocalAccessKind,
    values: &[Var],
    index: u32,
    env: &mut Env,
) {
    for (offset, value) in values.iter().enumerate() {
        let slot = match kind {
            // Canonical slot order follows ascending local-memory addresses.
            LocalAccessKind::WordBe => index + (values.len().saturating_sub(1) - offset) as u32,
            LocalAccessKind::WordLe => index + offset as u32,
            LocalAccessKind::Element => index,
        };
        env.set_local_fact(slot, env.fact_for_var(value));
        env.set_local_identity(slot, None);
        env.set_local_zero_test(slot, None);
    }
}

/// Load a scalar local and restore exact alias metadata when available.
pub(crate) fn apply_local_load_scalar(outputs: &[Var], index: u32, env: &mut Env) {
    let fact = env.fact_for_local(index);
    let identity = env.identity_for_local(index);
    let witness = env.zero_test_for_local(index);
    for output in outputs {
        env.set_var_fact(output, fact.clone());
        if let Some(identity) = identity.clone() {
            env.set_var_identity(output, identity);
        } else {
            env.clear_var_identity(output);
        }
        env.set_var_zero_test(output, witness.clone());
    }
}

/// Load a local word slot-by-slot, preserving stack order.
pub(crate) fn apply_local_load_word(
    kind: LocalAccessKind,
    outputs: &[Var],
    index: u32,
    env: &mut Env,
) {
    for (offset, output) in outputs.iter().enumerate() {
        let slot = match kind {
            LocalAccessKind::WordBe => index + offset as u32,
            LocalAccessKind::WordLe => index + (outputs.len().saturating_sub(1) - offset) as u32,
            LocalAccessKind::Element => index,
        };
        env.set_var_fact(output, env.fact_for_local(slot));
        env.clear_var_metadata(output);
    }
}

/// Return the variable compared against zero in an `eq.0`-shaped expression.
pub(crate) fn zero_comparison_var<'a>(lhs: &'a Expr, rhs: &'a Expr) -> Option<&'a Var> {
    match (lhs, rhs) {
        (Expr::Var(var), Expr::Constant(constant)) if constant.is_zero() => Some(var),
        (Expr::Constant(constant), Expr::Var(var)) if constant.is_zero() => Some(var),
        _ => None,
    }
}

/// Return the statement span for a structured statement.
pub(crate) fn stmt_span(stmt: &Stmt) -> miden_debug_types::SourceSpan {
    match stmt {
        Stmt::Assign { span, .. }
        | Stmt::MemLoad { span, .. }
        | Stmt::MemStore { span, .. }
        | Stmt::AdvLoad { span, .. }
        | Stmt::AdvStore { span, .. }
        | Stmt::LocalLoad { span, .. }
        | Stmt::LocalStore { span, .. }
        | Stmt::LocalStoreW { span, .. }
        | Stmt::Call { span, .. }
        | Stmt::Exec { span, .. }
        | Stmt::SysCall { span, .. }
        | Stmt::DynCall { span, .. }
        | Stmt::Intrinsic { span, .. }
        | Stmt::Repeat { span, .. }
        | Stmt::If { span, .. }
        | Stmt::While { span, .. }
        | Stmt::Return { span, .. } => *span,
    }
}

/// Return the base intrinsic name before immediates or `.err=*` suffixes.
pub(crate) fn intrinsic_base_name(name: &str) -> &str {
    name.split_once('.').map_or(name, |(base, _)| base)
}

/// Return true if an intrinsic requires caller-side `U32` preconditions.
pub(crate) fn intrinsic_requires_u32_precondition(name: &str) -> bool {
    if !name.starts_with("u32") {
        return false;
    }

    !matches!(
        intrinsic_base_name(name),
        "u32assert" | "u32assert2" | "u32assertw" | "u32cast" | "u32split" | "u32test" | "u32testw"
    )
}

/// Return the sole variable in the slice, if there is exactly one.
pub(crate) fn single_var(vars: &[Var]) -> Option<&Var> {
    match vars {
        [var] => Some(var),
        _ => None,
    }
}

/// Apply the custom transfer semantics of `adv_pipe`.
fn apply_adv_pipe_effect(
    span: miden_debug_types::SourceSpan,
    intrinsic: &Intrinsic,
    env: &mut Env,
) {
    if intrinsic.args.len() == 13 && intrinsic.results.len() == 13 {
        env.set_var_fact(&intrinsic.results[0], AdviceFact::bottom());
        env.clear_var_metadata(&intrinsic.results[0]);

        for (offset, result) in intrinsic.results[1..5].iter().enumerate() {
            let preserved_input = &intrinsic.args[11 - offset];
            env.set_var_fact(result, env.fact_for_var(preserved_input));
            env.clear_var_metadata(result);
        }

        for result in &intrinsic.results[5..] {
            env.set_var_fact(result, AdviceFact::from_source(span));
            env.clear_var_metadata(result);
        }
        return;
    }

    for result in &intrinsic.results {
        env.set_var_fact(result, AdviceFact::from_source(span));
        env.clear_var_metadata(result);
    }
}

#[cfg(test)]
mod tests {
    use super::Env;
    use crate::{
        abstract_interp::JoinSemiLattice,
        unconstrained_advice::domain::AdviceFact,
    };
    use masm_decompiler::ir::Var;

    /// Return a small synthetic SSA variable for flow-environment tests.
    fn test_var(index: u8) -> Var {
        Var::new(u64::from(index).into(), usize::from(index))
    }

    #[test]
    fn env_join_assign_reports_changes_and_merges_facts() {
        let mut lhs = Env::default();
        let lhs_var = test_var(0);
        lhs.set_var_fact(&lhs_var, AdviceFact::from_input(0));

        let mut rhs = Env::default();
        let rhs_var = test_var(1);
        rhs.set_var_fact(&lhs_var, AdviceFact::from_input(1));
        rhs.set_var_fact(&rhs_var, AdviceFact::from_input(2));

        assert!(lhs.join_assign(&rhs));
        assert_eq!(
            lhs.fact_for_var(&lhs_var),
            AdviceFact::from_input(0).join(&AdviceFact::from_input(1))
        );
        assert_eq!(lhs.fact_for_var(&rhs_var), AdviceFact::from_input(2));
        assert!(!lhs.join_assign(&rhs));
    }
}
