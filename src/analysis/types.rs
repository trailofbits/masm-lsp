//! Core types for static analysis.
//!
//! This module contains the fundamental data structures used for tracking
//! value provenance, bounds, and validation state during analysis.

use miden_debug_types::SourceSpan;

// ═══════════════════════════════════════════════════════════════════════════
// Constants
// ═══════════════════════════════════════════════════════════════════════════

pub const U32_MAX: u64 = 0xFFFF_FFFF;

// ═══════════════════════════════════════════════════════════════════════════
// Value bounds tracking
// ═══════════════════════════════════════════════════════════════════════════

/// Tracks what we know about a value's possible range.
#[derive(Clone, Debug, PartialEq)]
pub enum Bounds {
    /// No constraints - full field element
    Field,
    /// Known exact value
    Const(u64),
    /// Known range [lo, hi] inclusive
    Range { lo: u64, hi: u64 },
    /// Boolean: exactly 0 or 1
    Bool,
}

impl Default for Bounds {
    fn default() -> Self {
        Bounds::Field
    }
}

impl Bounds {
    /// Create bounds representing a u32 value [0, 2^32-1].
    pub fn u32() -> Self {
        Bounds::Range { lo: 0, hi: U32_MAX }
    }

    /// Returns true if the value is guaranteed to be within u32 range.
    pub fn is_u32(&self) -> bool {
        match self {
            Bounds::Const(v) => *v <= U32_MAX,
            Bounds::Range { lo: _, hi } => *hi <= U32_MAX,
            Bounds::Bool => true,
            Bounds::Field => false,
        }
    }

    /// Returns true if the value is guaranteed to be boolean (0 or 1).
    pub fn is_bool(&self) -> bool {
        matches!(self, Bounds::Bool)
            || matches!(self, Bounds::Const(v) if *v <= 1)
            || matches!(self, Bounds::Range { lo: 0, hi } if *hi <= 1)
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Taint tracking - provenance of values
// ═══════════════════════════════════════════════════════════════════════════

/// Tracks where a value came from (its provenance).
#[derive(Clone, Debug)]
pub enum Source {
    /// Literal constant in the code
    Literal,
    /// Read from advice stack (UNTRUSTED)
    AdviceStack,
    /// Read from advice map (UNTRUSTED)
    AdviceMap,
    /// Read from Merkle store (UNTRUSTED)
    MerkleStore,
    /// Loaded from memory
    Memory,
    /// Derived from other values (e.g., arithmetic result)
    Derived,
    /// Input parameter to procedure
    ProcInput { position: usize },
    /// Return value from procedure call
    ProcReturn { target: String },
}

impl Default for Source {
    fn default() -> Self {
        Source::Derived
    }
}

impl Source {
    /// Returns true if this source is untrusted external input.
    pub fn is_untrusted(&self) -> bool {
        matches!(
            self,
            Source::AdviceStack | Source::AdviceMap | Source::MerkleStore
        )
    }
}

/// Validation state of a taint.
#[derive(Clone, Debug, Default)]
pub enum ValidationState {
    /// Not validated
    #[default]
    None,
    /// Validated (e.g., by u32assert)
    Validated,
}

/// A tracked value with bounds and provenance.
#[derive(Clone, Debug)]
pub struct Taint {
    /// Unique identifier for this taint
    pub id: u64,
    /// Known bounds on this value
    pub bounds: Bounds,
    /// Where this value came from
    pub source: Source,
    /// Whether this value has been validated
    pub validated: ValidationState,
    /// Location where this value was created/loaded
    pub source_span: Option<SourceSpan>,
}

impl Taint {
    /// Returns true if this taint has been validated.
    pub fn is_validated(&self) -> bool {
        matches!(self.validated, ValidationState::Validated)
    }

    /// Mark this taint as validated and constrain bounds to u32.
    pub fn apply_validation(&mut self) {
        self.validated = ValidationState::Validated;
        // Also constrain bounds to u32 after validation
        if matches!(self.bounds, Bounds::Field) {
            self.bounds = Bounds::u32();
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Symbolic stack for tracking values
// ═══════════════════════════════════════════════════════════════════════════

/// A symbolic stack that tracks taint information.
///
/// The stack grows upward: the last element is the top of the stack (position 0).
#[derive(Clone, Debug, Default)]
pub struct SymbolicStack {
    elements: Vec<Taint>,
}

impl SymbolicStack {
    pub fn new() -> Self {
        Self::default()
    }

    /// Push a value onto the stack.
    pub fn push(&mut self, taint: Taint) {
        self.elements.push(taint);
    }

    /// Pop a value from the stack.
    pub fn pop(&mut self) -> Option<Taint> {
        self.elements.pop()
    }

    /// Peek at a value on the stack (0 = top).
    pub fn peek(&self, n: usize) -> Option<&Taint> {
        if n < self.elements.len() {
            Some(&self.elements[self.elements.len() - 1 - n])
        } else {
            None
        }
    }

    /// Peek at a value on the stack mutably (0 = top).
    pub fn peek_mut(&mut self, n: usize) -> Option<&mut Taint> {
        let len = self.elements.len();
        if n < len {
            Some(&mut self.elements[len - 1 - n])
        } else {
            None
        }
    }

    /// Get the current stack depth.
    pub fn depth(&self) -> usize {
        self.elements.len()
    }

    /// Duplicate the value at position n onto the top.
    pub fn dup(&mut self, n: usize) -> bool {
        if n < self.elements.len() {
            let idx = self.elements.len() - 1 - n;
            let elem = self.elements[idx].clone();
            self.elements.push(elem);
            true
        } else {
            false
        }
    }

    /// Swap values at positions a and b (0 = top).
    pub fn swap(&mut self, a: usize, b: usize) -> bool {
        let len = self.elements.len();
        if a < len && b < len {
            let idx_a = len - 1 - a;
            let idx_b = len - 1 - b;
            self.elements.swap(idx_a, idx_b);
            true
        } else {
            false
        }
    }

    /// Move the element at position n to the top.
    pub fn movup(&mut self, n: usize) -> bool {
        let len = self.elements.len();
        if n < len && n > 0 {
            let idx = len - 1 - n;
            let elem = self.elements.remove(idx);
            self.elements.push(elem);
            true
        } else {
            false
        }
    }

    /// Move the top element to position n.
    pub fn movdn(&mut self, n: usize) -> bool {
        let len = self.elements.len();
        if n < len && n > 0 {
            let elem = self.elements.pop().unwrap();
            let idx = len - n;
            self.elements.insert(idx, elem);
            true
        } else {
            false
        }
    }

    /// Clear all tracked values from the stack.
    ///
    /// This is used when we encounter a procedure call with unknown stack effects.
    /// We clear the tracking to avoid false positives, since we can't know what
    /// values remain on the stack after the call.
    pub fn clear(&mut self) {
        self.elements.clear();
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Analysis state per procedure
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
    /// Next taint ID to assign
    next_taint_id: u64,
}

impl AnalysisState {
    pub fn new(proc_name: String) -> Self {
        Self {
            proc_name,
            stack: SymbolicStack::new(),
            advice_consumed: 0,
            next_taint_id: 0,
        }
    }

    fn next_id(&mut self) -> u64 {
        let id = self.next_taint_id;
        self.next_taint_id += 1;
        id
    }

    /// Create a taint for a literal constant.
    pub fn make_literal(&mut self, value: u64, span: Option<SourceSpan>) -> Taint {
        Taint {
            id: self.next_id(),
            bounds: Bounds::Const(value),
            source: Source::Literal,
            validated: ValidationState::Validated,
            source_span: span,
        }
    }

    /// Create a taint for a value from the advice stack.
    pub fn make_advice(&mut self, span: SourceSpan) -> Taint {
        self.advice_consumed += 1;
        Taint {
            id: self.next_id(),
            bounds: Bounds::Field,
            source: Source::AdviceStack,
            validated: ValidationState::None,
            source_span: Some(span),
        }
    }

    /// Create a taint for a value from the Merkle store.
    pub fn make_merkle(&mut self, span: SourceSpan) -> Taint {
        Taint {
            id: self.next_id(),
            bounds: Bounds::Field,
            source: Source::MerkleStore,
            validated: ValidationState::None,
            source_span: Some(span),
        }
    }

    /// Create a taint for a derived value (e.g., arithmetic result).
    pub fn make_derived(&mut self, bounds: Bounds) -> Taint {
        Taint {
            id: self.next_id(),
            bounds,
            source: Source::Derived,
            validated: ValidationState::Validated,
            source_span: None,
        }
    }

    /// Create a taint for a value loaded from memory.
    pub fn make_memory(&mut self, span: SourceSpan) -> Taint {
        Taint {
            id: self.next_id(),
            bounds: Bounds::Field,
            source: Source::Memory,
            validated: ValidationState::None,
            source_span: Some(span),
        }
    }

    /// Create a taint for a procedure return value.
    ///
    /// Return values from procedures with known contracts are considered
    /// validated (we trust the procedure to return valid values).
    pub fn make_proc_return(&mut self, target: String) -> Taint {
        Taint {
            id: self.next_id(),
            bounds: Bounds::Field,
            source: Source::ProcReturn { target },
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
    fn test_bounds_u32() {
        assert!(Bounds::Const(0).is_u32());
        assert!(Bounds::Const(U32_MAX).is_u32());
        assert!(!Bounds::Const(U32_MAX + 1).is_u32());
        assert!(Bounds::u32().is_u32());
        assert!(!Bounds::Field.is_u32());
        assert!(Bounds::Bool.is_u32());
    }

    #[test]
    fn test_bounds_bool() {
        assert!(Bounds::Bool.is_bool());
        assert!(Bounds::Const(0).is_bool());
        assert!(Bounds::Const(1).is_bool());
        assert!(!Bounds::Const(2).is_bool());
        assert!(Bounds::Range { lo: 0, hi: 1 }.is_bool());
        assert!(!Bounds::Range { lo: 0, hi: 2 }.is_bool());
    }

    #[test]
    fn test_source_untrusted() {
        assert!(Source::AdviceStack.is_untrusted());
        assert!(Source::AdviceMap.is_untrusted());
        assert!(Source::MerkleStore.is_untrusted());
        assert!(!Source::Literal.is_untrusted());
        assert!(!Source::Memory.is_untrusted());
        assert!(!Source::Derived.is_untrusted());
    }

    #[test]
    fn test_symbolic_stack_basic() {
        let mut stack = SymbolicStack::new();
        assert_eq!(stack.depth(), 0);

        let mut state = AnalysisState::new("test".to_string());
        stack.push(state.make_literal(1, None));
        stack.push(state.make_literal(2, None));

        assert_eq!(stack.depth(), 2);
        assert!(matches!(stack.peek(0).unwrap().bounds, Bounds::Const(2)));
        assert!(matches!(stack.peek(1).unwrap().bounds, Bounds::Const(1)));
    }

    #[test]
    fn test_symbolic_stack_dup() {
        let mut stack = SymbolicStack::new();
        let mut state = AnalysisState::new("test".to_string());

        stack.push(state.make_literal(1, None));
        stack.push(state.make_literal(2, None));

        assert!(stack.dup(1)); // Duplicate element at position 1 (value 1)
        assert_eq!(stack.depth(), 3);
        assert!(matches!(stack.peek(0).unwrap().bounds, Bounds::Const(1)));
    }

    #[test]
    fn test_symbolic_stack_swap() {
        let mut stack = SymbolicStack::new();
        let mut state = AnalysisState::new("test".to_string());

        stack.push(state.make_literal(1, None));
        stack.push(state.make_literal(2, None));

        assert!(stack.swap(0, 1));
        assert!(matches!(stack.peek(0).unwrap().bounds, Bounds::Const(1)));
        assert!(matches!(stack.peek(1).unwrap().bounds, Bounds::Const(2)));
    }

    #[test]
    fn test_taint_validation() {
        let mut state = AnalysisState::new("test".to_string());
        let span = SourceSpan::default();
        let mut taint = state.make_advice(span);

        assert!(!taint.is_validated());
        assert!(matches!(taint.bounds, Bounds::Field));

        taint.apply_validation();

        assert!(taint.is_validated());
        assert!(taint.bounds.is_u32());
    }
}
