//! Checker for unvalidated advice values in u32 operations.
//!
//! u32 operations in Miden have undefined behavior when given inputs
//! that are not valid 32-bit values. This checker detects when values
//! from the advice stack are used in u32 operations without first
//! being validated with `u32assert`, `u32assert2`, or `u32assertw`.

use miden_assembly_syntax::ast::Instruction;

use crate::analysis::checker::{
    has_unvalidated_advice, is_u32_op, u32_op_input_count, AnalysisFinding, CheckContext, Checker,
};
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

        // Get the actual number of inputs consumed by this specific u32 operation
        let input_count = u32_op_input_count(inst);
        if input_count == 0 {
            return vec![];
        }

        // Check only the stack positions actually consumed by this operation
        if let Some(taint) = has_unvalidated_advice(state, input_count) {
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

    #[test]
    fn test_input_count_binary_ops() {
        use crate::analysis::checker::u32_op_input_count;

        // Binary ops should return 2
        assert_eq!(u32_op_input_count(&Instruction::U32WrappingAdd), 2);
        assert_eq!(u32_op_input_count(&Instruction::U32OverflowingSub), 2);
        assert_eq!(u32_op_input_count(&Instruction::U32Div), 2);
        assert_eq!(u32_op_input_count(&Instruction::U32And), 2);
        assert_eq!(u32_op_input_count(&Instruction::U32Lt), 2);
    }

    #[test]
    fn test_input_count_unary_ops() {
        use crate::analysis::checker::u32_op_input_count;

        // Unary ops should return 1
        assert_eq!(u32_op_input_count(&Instruction::U32Not), 1);
        assert_eq!(u32_op_input_count(&Instruction::U32Popcnt), 1);
        assert_eq!(u32_op_input_count(&Instruction::U32Clz), 1);
    }

    #[test]
    fn test_input_count_ternary_ops() {
        use crate::analysis::checker::u32_op_input_count;

        // Ternary ops should return 3
        assert_eq!(u32_op_input_count(&Instruction::U32OverflowingAdd3), 3);
        assert_eq!(u32_op_input_count(&Instruction::U32OverflowingMadd), 3);
        assert_eq!(u32_op_input_count(&Instruction::U32WrappingAdd3), 3);
        assert_eq!(u32_op_input_count(&Instruction::U32WrappingMadd), 3);
    }

    #[test]
    fn test_unary_op_ignores_second_position() {
        let mut state = AnalysisState::new("test".to_string());
        let span = SourceSpan::default();

        // Push a literal (trusted) at position 0
        let lit = state.make_literal(42, None);
        state.stack.push(lit);

        // Push unvalidated advice at position 1 (not consumed by unary ops)
        let advice = state.make_advice(span);
        state.stack.push(advice);

        // Swap so literal is on top, advice is at position 1
        state.stack.swap(0, 1);

        let checker = U32ValidationChecker;
        let (uri, span) = make_test_context();
        let ctx = CheckContext {
            contracts: None,
            uri: &uri,
            span,
        };

        // Unary u32 ops only consume position 0 (the literal)
        // Position 1 (advice) should NOT trigger a warning
        let findings = checker.check(&Instruction::U32Not, &state, &ctx);
        assert!(
            findings.is_empty(),
            "Unary ops should only check position 0, not position 1"
        );
    }

    #[test]
    fn test_ternary_ops_check_three_inputs() {
        let mut state = AnalysisState::new("test".to_string());
        let span = SourceSpan::default();

        // Push two validated values at positions 0 and 1
        let mut v1 = state.make_advice(span);
        v1.apply_validation();
        state.stack.push(v1);
        let mut v2 = state.make_advice(span);
        v2.apply_validation();
        state.stack.push(v2);

        // Push unvalidated advice at position 2
        let advice = state.make_advice(span);
        state.stack.push(advice);

        // Reorder so unvalidated is at position 2
        state.stack.swap(0, 2);

        let checker = U32ValidationChecker;
        let (uri, span) = make_test_context();
        let ctx = CheckContext {
            contracts: None,
            uri: &uri,
            span,
        };

        // Ternary ops consume positions 0, 1, 2 - should find the unvalidated at position 2
        let findings = checker.check(&Instruction::U32OverflowingAdd3, &state, &ctx);
        assert_eq!(
            findings.len(),
            1,
            "Ternary ops should check position 2 and find unvalidated advice"
        );
    }
}
