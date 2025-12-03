//! Analysis state for procedure analysis.
//!
//! This module provides the `AnalysisState` type that tracks state during
//! analysis of a procedure, including the symbolic stack and taint generation.

use miden_debug_types::SourceSpan;

use super::bounds::Bounds;
use super::symbolic_stack::SymbolicStack;
use super::taint::{TrackedValue, ValidationState, ValueOrigin};

// ═══════════════════════════════════════════════════════════════════════════
// Analysis State
// ═══════════════════════════════════════════════════════════════════════════

/// State tracked during analysis of a procedure.
#[derive(Clone, Debug)]
pub struct AnalysisState {
    /// Name of the current procedure
    pub proc_name: String,
    /// Symbolic stack state
    pub stack: SymbolicStack,
    /// Number of advice values consumed
    pub advice_consumed: usize,
    /// Next tracked value ID to assign
    next_value_id: u64,
}

impl AnalysisState {
    pub fn new(proc_name: String) -> Self {
        Self {
            proc_name,
            stack: SymbolicStack::new(),
            advice_consumed: 0,
            next_value_id: 0,
        }
    }

    fn next_id(&mut self) -> u64 {
        let id = self.next_value_id;
        self.next_value_id += 1;
        id
    }

    /// Create a tracked value for a literal constant.
    pub fn make_literal(&mut self, value: u64, span: Option<SourceSpan>) -> TrackedValue {
        TrackedValue {
            id: self.next_id(),
            bounds: Bounds::Const(value),
            origin: ValueOrigin::Literal,
            validated: ValidationState::Validated,
            source_span: span,
        }
    }

    /// Create a tracked value for a value from the advice stack.
    pub fn make_advice(&mut self, span: SourceSpan) -> TrackedValue {
        self.advice_consumed += 1;
        TrackedValue {
            id: self.next_id(),
            bounds: Bounds::Field,
            origin: ValueOrigin::AdviceStack,
            validated: ValidationState::None,
            source_span: Some(span),
        }
    }

    /// Create a tracked value for a value from the Merkle store.
    pub fn make_merkle(&mut self, span: SourceSpan) -> TrackedValue {
        TrackedValue {
            id: self.next_id(),
            bounds: Bounds::Field,
            origin: ValueOrigin::MerkleStore,
            validated: ValidationState::None,
            source_span: Some(span),
        }
    }

    /// Create a tracked value for a derived value (e.g., arithmetic result).
    pub fn make_derived(&mut self, bounds: Bounds) -> TrackedValue {
        TrackedValue {
            id: self.next_id(),
            bounds,
            origin: ValueOrigin::Derived,
            validated: ValidationState::Validated,
            source_span: None,
        }
    }

    /// Create a tracked value for a value loaded from memory.
    pub fn make_memory(&mut self, span: SourceSpan) -> TrackedValue {
        TrackedValue {
            id: self.next_id(),
            bounds: Bounds::Field,
            origin: ValueOrigin::Memory,
            validated: ValidationState::None,
            source_span: Some(span),
        }
    }

    /// Create a tracked value for a procedure return value.
    ///
    /// Return values from procedures with known contracts are considered
    /// validated (we trust the procedure to return valid values).
    pub fn make_proc_return(&mut self, target: String) -> TrackedValue {
        TrackedValue {
            id: self.next_id(),
            bounds: Bounds::Field,
            origin: ValueOrigin::ProcReturn { target },
            validated: ValidationState::Validated,
            source_span: None,
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Tests
// ═══════════════════════════════════════════════════════════════════════════

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_analysis_state_new() {
        let state = AnalysisState::new("test_proc".to_string());
        assert_eq!(state.proc_name, "test_proc");
        assert_eq!(state.stack.depth(), 0);
        assert_eq!(state.advice_consumed, 0);
    }

    #[test]
    fn test_make_literal() {
        let mut state = AnalysisState::new("test".to_string());
        let taint = state.make_literal(42, None);

        assert_eq!(taint.id, 0);
        assert_eq!(taint.bounds, Bounds::Const(42));
        assert!(taint.is_validated());
    }

    #[test]
    fn test_make_advice() {
        let mut state = AnalysisState::new("test".to_string());
        let taint = state.make_advice(SourceSpan::default());

        assert_eq!(taint.id, 0);
        assert_eq!(taint.bounds, Bounds::Field);
        assert!(!taint.is_validated());
        assert!(matches!(taint.origin, ValueOrigin::AdviceStack));
        assert_eq!(state.advice_consumed, 1);
    }

    #[test]
    fn test_make_derived() {
        let mut state = AnalysisState::new("test".to_string());
        let taint = state.make_derived(Bounds::u32());

        assert!(taint.bounds.is_u32());
        assert!(taint.is_validated());
    }

    #[test]
    fn test_taint_id_increments() {
        let mut state = AnalysisState::new("test".to_string());

        let t1 = state.make_literal(1, None);
        let t2 = state.make_literal(2, None);
        let t3 = state.make_derived(Bounds::Field);

        assert_eq!(t1.id, 0);
        assert_eq!(t2.id, 1);
        assert_eq!(t3.id, 2);
    }
}
