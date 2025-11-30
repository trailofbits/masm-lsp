//! Checker for untrusted values passed to procedure calls.
//!
//! This checker uses workspace-inferred contracts to determine if
//! a called procedure validates its inputs or requires u32 values.
//! It warns when unvalidated advice is passed to procedures that
//! don't validate their inputs.

use miden_assembly_syntax::ast::{Instruction, InvocationTarget};

use crate::analysis::checker::{has_unvalidated_advice, AnalysisFinding, CheckContext, Checker};
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

    /// Look up contract information for a procedure.
    fn get_contract_info(
        target: &str,
        ctx: &CheckContext,
    ) -> Option<(bool, bool)> {
        // Returns (validates_inputs, requires_u32)
        let store = ctx.contracts?;

        // Try by suffix first, then by name
        let contract = store
            .get_by_suffix(target)
            .or_else(|| store.get_by_name(target))?;

        Some((contract.validates_inputs(), contract.requires_u32_inputs()))
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
        let contract_info = Self::get_contract_info(&target_name, ctx);

        match contract_info {
            Some((true, _)) => {
                // Procedure validates inputs - no warning needed
                vec![]
            }
            Some((false, true)) => {
                // Known procedure that requires u32 inputs but doesn't validate
                if let Some(taint) = has_unvalidated_advice(state, 4) {
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
            Some((false, false)) => {
                // Known procedure that doesn't require u32 - no warning
                vec![]
            }
            None => {
                // Unknown procedure - conservative warning if passing untrusted values
                if let Some(taint) = has_unvalidated_advice(state, 4) {
                    vec![AnalysisFinding::warning(format!(
                        "Unvalidated advice may be passed to `{}`. \
                         Consider adding validation if the procedure expects u32 inputs.",
                        target_name
                    ))
                    .with_tracked_value(taint)]
                } else {
                    vec![]
                }
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

        // Create contract store with validating procedure
        let mut contracts = ContractStore::new();
        contracts.update_document(vec![ProcContract {
            path: SymbolPath::new("::test::validating_proc"),
            validates: ValidationBehavior::ValidatesU32,
            uses_u32_ops: true,
            reads_advice: false,
            uses_merkle_ops: false,
            stack_effect: StackEffect::Unknown,
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

        // Create contract store with non-validating u32 procedure
        let mut contracts = ContractStore::new();
        contracts.update_document(vec![ProcContract {
            path: SymbolPath::new("::test::u32_proc"),
            validates: ValidationBehavior::None,
            uses_u32_ops: true,
            reads_advice: false,
            uses_merkle_ops: false,
            stack_effect: StackEffect::Unknown,
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
    fn test_warns_for_unknown_procedure_with_untrusted_input() {
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

        let findings = checker.check(&make_exec("unknown_proc"), &state, &ctx);
        assert_eq!(findings.len(), 1);
        assert!(findings[0].message.contains("unknown_proc"));
        assert!(findings[0].message.contains("may be passed"));
    }

    #[test]
    fn test_no_warning_for_validated_input() {
        let mut state = AnalysisState::new("test".to_string());
        let span = SourceSpan::default();

        // Push validated advice
        let mut t = state.make_advice(span);
        t.apply_validation();
        state.stack.push(t);

        // Create contract store with non-validating u32 procedure
        let mut contracts = ContractStore::new();
        contracts.update_document(vec![ProcContract {
            path: SymbolPath::new("::test::u32_proc"),
            validates: ValidationBehavior::None,
            uses_u32_ops: true,
            reads_advice: false,
            uses_merkle_ops: false,
            stack_effect: StackEffect::Unknown,
            definition_range: None,
        }]);

        let checker = ProcedureCallChecker;
        let uri = Url::parse("file:///test.masm").unwrap();
        let ctx = make_context_with_contracts(&uri, span, &contracts);

        let findings = checker.check(&make_exec("u32_proc"), &state, &ctx);
        assert!(findings.is_empty());
    }
}
