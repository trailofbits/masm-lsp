//! Checker for unvalidated Merkle tree depth values.
//!
//! Merkle tree operations in Miden require a depth parameter that must
//! be <= 64. When the depth comes from the advice stack, it should be
//! validated before use to prevent potential issues.

use miden_assembly_syntax::ast::Instruction;

use crate::analysis::checker::{CheckContext, Checker, Finding};
use crate::analysis::types::AnalysisState;

/// Checker that detects unvalidated depth values in Merkle operations.
pub struct MerkleDepthChecker;

impl Checker for MerkleDepthChecker {
    fn check(
        &self,
        inst: &Instruction,
        state: &AnalysisState,
        _ctx: &CheckContext,
    ) -> Vec<Finding> {
        // Only check Merkle operations that take depth as input
        let needs_depth_check = matches!(
            inst,
            Instruction::MTreeGet | Instruction::MTreeSet | Instruction::MTreeVerify
        );

        if !needs_depth_check {
            return vec![];
        }

        // Depth is typically at a specific stack position depending on the operation
        // For mtree_get and mtree_set, depth is at the top of the stack
        if let Some(t) = state.stack.peek(0) {
            if t.source.is_untrusted() && !t.is_validated() {
                return vec![Finding::warning(
                    "MTree depth from advice not validated. Ensure depth <= 64 before use.",
                )
                .with_taint(t.clone())];
            }
        }

        vec![]
    }

    fn name(&self) -> &'static str {
        "merkle-depth"
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::types::AnalysisState;
    use miden_debug_types::SourceSpan;
    use tower_lsp::lsp_types::Url;

    fn make_context() -> (Url, SourceSpan) {
        (
            Url::parse("file:///test.masm").unwrap(),
            SourceSpan::default(),
        )
    }

    #[test]
    fn test_detects_unvalidated_depth_in_mtree_get() {
        let mut state = AnalysisState::new("test".to_string());
        let span = SourceSpan::default();

        // Push unvalidated advice as depth
        let t = state.make_advice(span);
        state.stack.push(t);

        let checker = MerkleDepthChecker;
        let (uri, span) = make_context();
        let ctx = CheckContext {
            contracts: None,
            uri: &uri,
            span,
        };

        let findings = checker.check(&Instruction::MTreeGet, &state, &ctx);
        assert_eq!(findings.len(), 1);
        assert!(findings[0].message.contains("depth"));
    }

    #[test]
    fn test_no_warning_for_validated_depth() {
        let mut state = AnalysisState::new("test".to_string());
        let span = SourceSpan::default();

        // Push validated depth
        let mut depth = state.make_advice(span);
        depth.apply_validation();
        state.stack.push(depth);

        let checker = MerkleDepthChecker;
        let (uri, span) = make_context();
        let ctx = CheckContext {
            contracts: None,
            uri: &uri,
            span,
        };

        let findings = checker.check(&Instruction::MTreeGet, &state, &ctx);
        assert!(findings.is_empty());
    }

    #[test]
    fn test_no_warning_for_literal_depth() {
        let mut state = AnalysisState::new("test".to_string());

        // Push literal depth
        let lit = state.make_literal(32, None);
        state.stack.push(lit);

        let checker = MerkleDepthChecker;
        let (uri, span) = make_context();
        let ctx = CheckContext {
            contracts: None,
            uri: &uri,
            span,
        };

        let findings = checker.check(&Instruction::MTreeGet, &state, &ctx);
        assert!(findings.is_empty());
    }

    #[test]
    fn test_ignores_non_merkle_ops() {
        let mut state = AnalysisState::new("test".to_string());
        let span = SourceSpan::default();

        // Push unvalidated advice
        let t = state.make_advice(span);
        state.stack.push(t);

        let checker = MerkleDepthChecker;
        let (uri, span) = make_context();
        let ctx = CheckContext {
            contracts: None,
            uri: &uri,
            span,
        };

        // Non-Merkle operations should not trigger warnings
        let findings = checker.check(&Instruction::Add, &state, &ctx);
        assert!(findings.is_empty());
    }
}
