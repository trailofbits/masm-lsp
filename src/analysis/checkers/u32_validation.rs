//! Checker for unvalidated advice values in u32 operations.
//!
//! u32 operations in Miden have undefined behavior when given inputs
//! that are not valid 32-bit values. This checker detects when values
//! from the advice stack are used in u32 operations without first
//! being validated with `u32assert`, `u32assert2`, or `u32assertw`.

use miden_assembly_syntax::ast::Instruction;

use crate::analysis::checker::{has_unvalidated_advice, is_u32_op, AnalysisFinding, CheckContext, Checker};
use crate::analysis::types::AnalysisState;

/// Checker that detects unvalidated advice values in u32 operations.
pub struct U32ValidationChecker;

impl Checker for U32ValidationChecker {
    fn check(
        &self,
        inst: &Instruction,
        state: &AnalysisState,
        _ctx: &CheckContext,
    ) -> Vec<AnalysisFinding> {
        if !is_u32_op(inst) {
            return vec![];
        }

        // Check top 2 stack values for unvalidated advice
        // (most u32 ops are binary operations)
        if let Some(taint) = has_unvalidated_advice(state, 2) {
            return vec![AnalysisFinding::warning(
                "Unvalidated advice value used in u32 operation. \
                 Add `u32assert` or `u32assert2` before this instruction.",
            )
            .with_tracked_value(taint)];
        }

        vec![]
    }

    fn name(&self) -> &'static str {
        "u32-validation"
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::checkers::test_utils::make_test_context;
    use crate::analysis::types::AnalysisState;
    use miden_debug_types::SourceSpan;

    #[test]
    fn test_detects_unvalidated_advice_in_u32_op() {
        let mut state = AnalysisState::new("test".to_string());
        let span = SourceSpan::default();

        // Push unvalidated advice values
        let t1 = state.make_advice(span);
        state.stack.push(t1);
        let t2 = state.make_advice(span);
        state.stack.push(t2);

        let checker = U32ValidationChecker;
        let (uri, span) = make_test_context();
        let ctx = CheckContext {
            contracts: None,
            uri: &uri,
            span,
        };

        let findings = checker.check(&Instruction::U32WrappingAdd, &state, &ctx);
        assert_eq!(findings.len(), 1);
        assert!(findings[0].message.contains("u32assert"));
    }

    #[test]
    fn test_no_warning_for_validated_values() {
        let mut state = AnalysisState::new("test".to_string());
        let span = SourceSpan::default();

        // Push validated values
        let mut t1 = state.make_advice(span);
        t1.apply_validation();
        state.stack.push(t1);

        let mut t2 = state.make_advice(span);
        t2.apply_validation();
        state.stack.push(t2);

        let checker = U32ValidationChecker;
        let (uri, span) = make_test_context();
        let ctx = CheckContext {
            contracts: None,
            uri: &uri,
            span,
        };

        let findings = checker.check(&Instruction::U32WrappingAdd, &state, &ctx);
        assert!(findings.is_empty());
    }

    #[test]
    fn test_no_warning_for_literals() {
        let mut state = AnalysisState::new("test".to_string());

        // Push literal values
        let l1 = state.make_literal(1, None);
        state.stack.push(l1);
        let l2 = state.make_literal(2, None);
        state.stack.push(l2);

        let checker = U32ValidationChecker;
        let (uri, span) = make_test_context();
        let ctx = CheckContext {
            contracts: None,
            uri: &uri,
            span,
        };

        let findings = checker.check(&Instruction::U32WrappingAdd, &state, &ctx);
        assert!(findings.is_empty());
    }

    #[test]
    fn test_ignores_non_u32_ops() {
        let mut state = AnalysisState::new("test".to_string());
        let span = SourceSpan::default();

        // Push unvalidated advice
        let t = state.make_advice(span);
        state.stack.push(t);

        let checker = U32ValidationChecker;
        let (uri, span) = make_test_context();
        let ctx = CheckContext {
            contracts: None,
            uri: &uri,
            span,
        };

        // Non-u32 operations should not trigger warnings
        let findings = checker.check(&Instruction::Add, &state, &ctx);
        assert!(findings.is_empty());
    }
}
