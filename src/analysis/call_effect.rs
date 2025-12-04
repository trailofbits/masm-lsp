//! Shared procedure call effect resolution.
//!
//! This module centralizes how we resolve a procedure's stack effect from the
//! contract store, so callers can handle known/unknown effects consistently.

use miden_assembly_syntax::ast::InvocationTarget;

use crate::symbol_resolution::SymbolResolver;

use super::contracts::{ContractStore, StackEffect};

/// Result of resolving a procedure call's stack effect.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CallEffect {
    /// Known stack effect with both inputs and outputs.
    Known { inputs: usize, outputs: usize },
    /// Known inputs but unknown outputs (conservative).
    KnownInputs { inputs: usize },
    /// Unknown effect (no contract or dynamic target).
    Unknown,
}

/// Resolve a procedure call's stack effect using the resolver and contract store.
///
/// Returns `CallEffect::Unknown` if the target cannot be resolved or no contract is found.
pub fn resolve_call_effect(
    resolver: Option<&SymbolResolver>,
    contracts: Option<&ContractStore>,
    target: &InvocationTarget,
) -> CallEffect {
    // Dynamic/MastRoot calls are unknown by definition.
    if matches!(target, InvocationTarget::MastRoot(_)) {
        return CallEffect::Unknown;
    }

    let store = match contracts {
        Some(c) => c,
        None => return CallEffect::Unknown,
    };

    // Try resolver first when available.
    if let Some(resolver) = resolver {
        if let Some(resolved) = resolver.resolve_target(target) {
            if let Some(contract) = store.get(&resolved) {
                return contract_to_effect(contract);
            }
        }
    }

    // Fallback: best-effort lookup by name/suffix when resolver is not available.
    let contract = match target {
        InvocationTarget::Symbol(ident) => store
            .get_by_name(ident.as_str())
            .or_else(|| store.get_by_suffix(ident.as_str())),
        InvocationTarget::Path(path) => store.get_by_suffix(path.inner().as_str()),
        InvocationTarget::MastRoot(_) => None,
    };

    contract.map_or(CallEffect::Unknown, contract_to_effect)
}

fn contract_to_effect(contract: &crate::analysis::contracts::ProcContract) -> CallEffect {
    match &contract.stack_effect {
        StackEffect::Known { inputs, outputs } => CallEffect::Known {
            inputs: *inputs,
            outputs: *outputs,
        },
        StackEffect::KnownInputs { inputs } => CallEffect::KnownInputs { inputs: *inputs },
        StackEffect::Unknown => CallEffect::Unknown,
    }
}
