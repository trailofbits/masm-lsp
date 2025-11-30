//! Checker for untrusted values passed to procedure calls.
//!
//! This checker uses workspace-inferred contracts to determine if
//! a called procedure validates its inputs or requires u32 values.
//! It warns when unvalidated advice is passed to procedures that
//! don't validate their inputs.

use miden_assembly_syntax::ast::{Instruction, InvocationTarget};

use crate::analysis::checker::{has_unvalidated_advice, AnalysisFinding, CheckContext, Checker};
use crate::analysis::contracts::ProcContract;
use crate::analysis::types::AnalysisState;

/// Checker that detects untrusted values passed to procedure calls.
pub struct ProcedureCallChecker;

impl ProcedureCallChecker {
    /// Extract the target name from an invocation target.
    fn get_target_name(target: &InvocationTarget) -> Option<String> {
        match target {
            InvocationTarget::Symbol(ident) => Some(ident.as_str().to_string()),
            InvocationTarget::Path(path) => Some(path.inner().as_str().to_string()),
            InvocationTarget::MastRoot(_) => None,
        }
    }

    /// Look up the full contract for a procedure.
    fn get_contract<'a>(target: &str, ctx: &'a CheckContext) -> Option<&'a ProcContract> {
        let store = ctx.contracts?;

        // Try by suffix first, then by name
        store
            .get_by_suffix(target)
            .or_else(|| store.get_by_name(target))
    }

    /// Get the number of inputs to check for a procedure.
    /// Returns None if the stack effect is unknown.
    fn get_input_count(contract: Option<&ProcContract>) -> Option<usize> {
        contract.and_then(|c| c.stack_effect.inputs())
    }
}

impl Checker for ProcedureCallChecker {
    fn check(
        &self,
        inst: &Instruction,
        state: &AnalysisState,
        ctx: &CheckContext,
    ) -> Vec<AnalysisFinding> {
        // Only check procedure call instructions
        let target = match inst {
            Instruction::Exec(t) | Instruction::Call(t) | Instruction::SysCall(t) => t,
            _ => return vec![],
        };

        let target_name = match Self::get_target_name(target) {
            Some(name) => name,
            None => return vec![], // MAST root calls - can't analyze
        };

        // Look up contract from workspace
        let contract = Self::get_contract(&target_name, ctx);

        // Get the number of inputs to check based on the contract's stack effect
        // If unknown, we cannot reliably check so we skip the diagnostic
        let input_count = match Self::get_input_count(contract) {
            Some(count) => count,
            None => return vec![], // Unknown stack effect - skip diagnostic
        };

        match contract {
            Some(c) if c.validates_inputs() => {
                // Procedure validates inputs - no warning needed
                vec![]
            }
            Some(c) if c.requires_u32_inputs() => {
                // Known procedure that requires u32 inputs but doesn't validate
                if let Some(taint) = has_unvalidated_advice(state, input_count) {
                    vec![AnalysisFinding::warning(format!(
                        "Unvalidated advice passed to `{}` which requires \
                         u32 inputs but doesn't validate them.",
                        target_name
                    ))
                    .with_tracked_value(taint)]
                } else {
                    vec![]
                }
            }
            Some(_) | None => {
                // Known procedure that doesn't require u32, or unknown procedure
                // with known stack effect - no warning needed
                vec![]
            }
        }
    }

    fn name(&self) -> &'static str {
        "procedure-call"
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::contracts::{ContractStore, ProcContract, StackEffect, ValidationBehavior};
    use crate::analysis::types::AnalysisState;
    use crate::symbol_path::SymbolPath;
    use miden_assembly_syntax::ast::Ident;
    use miden_debug_types::SourceSpan;
    use tower_lsp::lsp_types::Url;

    fn make_exec(name: &str) -> Instruction {
        Instruction::Exec(InvocationTarget::Symbol(Ident::new(name).unwrap()))
    }

    fn make_context_with_contracts<'a>(
        uri: &'a Url,
        span: SourceSpan,
        contracts: &'a ContractStore,
    ) -> CheckContext<'a> {
        CheckContext {
            contracts: Some(contracts),
            uri,
            span,
        }
    }

    #[test]
    fn test_no_warning_for_validating_procedure() {
        let mut state = AnalysisState::new("test".to_string());
        let span = SourceSpan::default();

        // Push unvalidated advice
        let t = state.make_advice(span);
        state.stack.push(t);

        // Create contract store with validating procedure with known stack effect
        let mut contracts = ContractStore::new();
        contracts.update_document(vec![ProcContract {
            path: SymbolPath::new("::test::validating_proc"),
            validates: ValidationBehavior::ValidatesU32,
            uses_u32_ops: true,
            reads_advice: false,
            uses_merkle_ops: false,
            stack_effect: StackEffect::Known { inputs: 2, outputs: 1 },
            definition_range: None,
        }]);

        let checker = ProcedureCallChecker;
        let uri = Url::parse("file:///test.masm").unwrap();
        let ctx = make_context_with_contracts(&uri, span, &contracts);

        let findings = checker.check(&make_exec("validating_proc"), &state, &ctx);
        assert!(findings.is_empty());
    }

    #[test]
    fn test_warns_for_non_validating_u32_procedure() {
        let mut state = AnalysisState::new("test".to_string());
        let span = SourceSpan::default();

        // Push unvalidated advice
        let t = state.make_advice(span);
        state.stack.push(t);

        // Create contract store with non-validating u32 procedure with known stack effect
        let mut contracts = ContractStore::new();
        contracts.update_document(vec![ProcContract {
            path: SymbolPath::new("::test::u32_proc"),
            validates: ValidationBehavior::None,
            uses_u32_ops: true,
            reads_advice: false,
            uses_merkle_ops: false,
            stack_effect: StackEffect::Known { inputs: 2, outputs: 1 },
            definition_range: None,
        }]);

        let checker = ProcedureCallChecker;
        let uri = Url::parse("file:///test.masm").unwrap();
        let ctx = make_context_with_contracts(&uri, span, &contracts);

        let findings = checker.check(&make_exec("u32_proc"), &state, &ctx);
        assert_eq!(findings.len(), 1);
        assert!(findings[0].message.contains("u32_proc"));
    }

    #[test]
    fn test_no_warning_for_unknown_procedure() {
        let mut state = AnalysisState::new("test".to_string());
        let span = SourceSpan::default();

        // Push unvalidated advice
        let t = state.make_advice(span);
        state.stack.push(t);

        // Empty contract store - procedure unknown
        let contracts = ContractStore::new();

        let checker = ProcedureCallChecker;
        let uri = Url::parse("file:///test.masm").unwrap();
        let ctx = make_context_with_contracts(&uri, span, &contracts);

        // Unknown procedures have unknown stack effects, so no warning is emitted
        let findings = checker.check(&make_exec("unknown_proc"), &state, &ctx);
        assert!(findings.is_empty());
    }

    #[test]
    fn test_no_warning_for_validated_input() {
        let mut state = AnalysisState::new("test".to_string());
        let span = SourceSpan::default();

        // Push validated advice
        let mut t = state.make_advice(span);
        t.apply_validation();
        state.stack.push(t);

        // Create contract store with non-validating u32 procedure with known stack effect
        let mut contracts = ContractStore::new();
        contracts.update_document(vec![ProcContract {
            path: SymbolPath::new("::test::u32_proc"),
            validates: ValidationBehavior::None,
            uses_u32_ops: true,
            reads_advice: false,
            uses_merkle_ops: false,
            stack_effect: StackEffect::Known { inputs: 2, outputs: 1 },
            definition_range: None,
        }]);

        let checker = ProcedureCallChecker;
        let uri = Url::parse("file:///test.masm").unwrap();
        let ctx = make_context_with_contracts(&uri, span, &contracts);

        let findings = checker.check(&make_exec("u32_proc"), &state, &ctx);
        assert!(findings.is_empty());
    }

    #[test]
    fn test_uses_known_input_count_from_stack_effect() {
        let mut state = AnalysisState::new("test".to_string());
        let span = SourceSpan::default();

        // Push a literal at position 0 (top of stack after all pushes)
        let lit = state.make_literal(42, None);
        state.stack.push(lit);

        // Push unvalidated advice at position 1
        let advice = state.make_advice(span);
        state.stack.push(advice);

        // Swap so literal is on top, advice is at position 1
        state.stack.swap(0, 1);

        // Create a procedure that only consumes 1 input (position 0)
        let mut contracts = ContractStore::new();
        contracts.update_document(vec![ProcContract {
            path: SymbolPath::new("::test::single_input_proc"),
            validates: ValidationBehavior::None,
            uses_u32_ops: true,
            reads_advice: false,
            uses_merkle_ops: false,
            stack_effect: StackEffect::Known { inputs: 1, outputs: 1 },
            definition_range: None,
        }]);

        let checker = ProcedureCallChecker;
        let uri = Url::parse("file:///test.masm").unwrap();
        let ctx = make_context_with_contracts(&uri, span, &contracts);

        // Should NOT warn because the procedure only consumes position 0 (the literal)
        // Position 1 (unvalidated advice) is not consumed by this procedure
        let findings = checker.check(&make_exec("single_input_proc"), &state, &ctx);
        assert!(
            findings.is_empty(),
            "Should not warn when unvalidated advice is beyond the procedure's input count"
        );
    }

    #[test]
    fn test_warns_when_unvalidated_advice_within_input_count() {
        let mut state = AnalysisState::new("test".to_string());
        let span = SourceSpan::default();

        // Push unvalidated advice at position 0
        let advice = state.make_advice(span);
        state.stack.push(advice);

        // Create a procedure that consumes 2 inputs
        let mut contracts = ContractStore::new();
        contracts.update_document(vec![ProcContract {
            path: SymbolPath::new("::test::two_input_proc"),
            validates: ValidationBehavior::None,
            uses_u32_ops: true,
            reads_advice: false,
            uses_merkle_ops: false,
            stack_effect: StackEffect::Known { inputs: 2, outputs: 1 },
            definition_range: None,
        }]);

        let checker = ProcedureCallChecker;
        let uri = Url::parse("file:///test.masm").unwrap();
        let ctx = make_context_with_contracts(&uri, span, &contracts);

        // Should warn because unvalidated advice is at position 0, which is consumed
        let findings = checker.check(&make_exec("two_input_proc"), &state, &ctx);
        assert_eq!(
            findings.len(),
            1,
            "Should warn when unvalidated advice is within the procedure's input count"
        );
    }
}
