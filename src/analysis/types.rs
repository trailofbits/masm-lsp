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

    /// Returns the known constant value, if any.
    pub fn as_const(&self) -> Option<u64> {
        match self {
            Bounds::Const(v) => Some(*v),
            _ => None,
        }
    }

    // ═══════════════════════════════════════════════════════════════════════
    // Arithmetic operations for constant/range propagation
    // ═══════════════════════════════════════════════════════════════════════

    /// Helper to get (lo, hi) bounds for any Bounds variant
    fn as_range(&self) -> Option<(u64, u64)> {
        match self {
            Bounds::Const(v) => Some((*v, *v)),
            Bounds::Range { lo, hi } => Some((*lo, *hi)),
            Bounds::Bool => Some((0, 1)),
            Bounds::Field => None,
        }
    }

    /// Add two bounds, propagating constants and ranges where possible.
    pub fn add(&self, other: &Bounds) -> Bounds {
        match (self, other) {
            // Const + Const = Const
            (Bounds::Const(a), Bounds::Const(b)) => Bounds::Const(a.wrapping_add(*b)),

            // Const + Range or Range + Const = shifted Range
            (Bounds::Const(c), Bounds::Range { lo, hi })
            | (Bounds::Range { lo, hi }, Bounds::Const(c)) => {
                let new_lo = lo.saturating_add(*c);
                let new_hi = hi.saturating_add(*c);
                if new_lo == new_hi {
                    Bounds::Const(new_lo)
                } else {
                    Bounds::Range {
                        lo: new_lo,
                        hi: new_hi,
                    }
                }
            }

            // Range + Range = widened Range
            (Bounds::Range { lo: l1, hi: h1 }, Bounds::Range { lo: l2, hi: h2 }) => {
                let new_lo = l1.saturating_add(*l2);
                let new_hi = h1.saturating_add(*h2);
                Bounds::Range {
                    lo: new_lo,
                    hi: new_hi,
                }
            }

            // Bool operations
            (Bounds::Bool, Bounds::Bool) => Bounds::Range { lo: 0, hi: 2 },
            (Bounds::Bool, Bounds::Const(c)) | (Bounds::Const(c), Bounds::Bool) => {
                Bounds::Range {
                    lo: *c,
                    hi: c.saturating_add(1),
                }
            }

            _ => Bounds::Field,
        }
    }

    /// Subtract: self - other, propagating constants and ranges where possible.
    pub fn sub(&self, other: &Bounds) -> Bounds {
        match (self, other) {
            // Const - Const = Const
            (Bounds::Const(a), Bounds::Const(b)) => Bounds::Const(a.wrapping_sub(*b)),

            // Range - Const = shifted Range
            (Bounds::Range { lo, hi }, Bounds::Const(c)) => {
                let new_lo = lo.saturating_sub(*c);
                let new_hi = hi.saturating_sub(*c);
                if new_lo == new_hi {
                    Bounds::Const(new_lo)
                } else {
                    Bounds::Range {
                        lo: new_lo,
                        hi: new_hi,
                    }
                }
            }

            // Const - Range: result can be negative in field arithmetic, be conservative
            // But for u64, we can compute bounds: [c - hi, c - lo] (may wrap)
            (Bounds::Const(c), Bounds::Range { lo, hi }) => {
                if *c >= *hi {
                    // No underflow possible
                    Bounds::Range {
                        lo: c.saturating_sub(*hi),
                        hi: c.saturating_sub(*lo),
                    }
                } else {
                    // May underflow, be conservative
                    Bounds::Field
                }
            }

            // Range - Range: complex, be conservative for now
            (Bounds::Range { lo: l1, hi: h1 }, Bounds::Range { lo: l2, hi: h2 }) => {
                if *l1 >= *h2 {
                    // No underflow: [l1-h2, h1-l2]
                    Bounds::Range {
                        lo: l1.saturating_sub(*h2),
                        hi: h1.saturating_sub(*l2),
                    }
                } else {
                    Bounds::Field
                }
            }

            _ => Bounds::Field,
        }
    }

    /// Multiply two bounds, propagating constants and ranges where possible.
    pub fn mul(&self, other: &Bounds) -> Bounds {
        match (self, other) {
            // Const * Const = Const
            (Bounds::Const(a), Bounds::Const(b)) => Bounds::Const(a.wrapping_mul(*b)),

            // Const(0) * anything = 0, anything * Const(0) = 0
            (Bounds::Const(0), _) | (_, Bounds::Const(0)) => Bounds::Const(0),

            // Const(1) * x = x, x * Const(1) = x
            (Bounds::Const(1), other) | (other, Bounds::Const(1)) => other.clone(),

            // Const * Range or Range * Const = scaled Range
            (Bounds::Const(c), Bounds::Range { lo, hi })
            | (Bounds::Range { lo, hi }, Bounds::Const(c)) => {
                let new_lo = lo.saturating_mul(*c);
                let new_hi = hi.saturating_mul(*c);
                if new_lo == new_hi {
                    Bounds::Const(new_lo)
                } else {
                    Bounds::Range {
                        lo: new_lo,
                        hi: new_hi,
                    }
                }
            }

            // Bool * anything: result is in [0, max_other]
            (Bounds::Bool, other) | (other, Bounds::Bool) => {
                if let Some((_, hi)) = other.as_range() {
                    Bounds::Range { lo: 0, hi }
                } else {
                    Bounds::Field
                }
            }

            // Range * Range: complex, use [lo1*lo2, hi1*hi2] as approximation
            (Bounds::Range { lo: l1, hi: h1 }, Bounds::Range { lo: l2, hi: h2 }) => {
                let new_lo = l1.saturating_mul(*l2);
                let new_hi = h1.saturating_mul(*h2);
                Bounds::Range {
                    lo: new_lo,
                    hi: new_hi,
                }
            }

            _ => Bounds::Field,
        }
    }

    /// Division: self / other (integer division)
    pub fn div(&self, other: &Bounds) -> Bounds {
        match (self, other) {
            (Bounds::Const(a), Bounds::Const(b)) if *b != 0 => Bounds::Const(a / b),
            (Bounds::Range { lo, hi }, Bounds::Const(c)) if *c != 0 => {
                let new_lo = lo / c;
                let new_hi = hi / c;
                if new_lo == new_hi {
                    Bounds::Const(new_lo)
                } else {
                    Bounds::Range {
                        lo: new_lo,
                        hi: new_hi,
                    }
                }
            }
            _ => Bounds::Field,
        }
    }

    /// Modulo: self % other
    pub fn modulo(&self, other: &Bounds) -> Bounds {
        match (self, other) {
            (Bounds::Const(a), Bounds::Const(b)) if *b != 0 => Bounds::Const(a % b),
            // x % c is always in [0, c-1]
            (_, Bounds::Const(c)) if *c != 0 => Bounds::Range {
                lo: 0,
                hi: c.saturating_sub(1),
            },
            _ => Bounds::Field,
        }
    }

    /// Equality comparison: returns Bool or Const(0/1) if determinable.
    pub fn eq(&self, other: &Bounds) -> Bounds {
        match (self, other) {
            (Bounds::Const(a), Bounds::Const(b)) => Bounds::Const(if a == b { 1 } else { 0 }),
            // Disjoint ranges: definitely not equal
            (Bounds::Range { lo: l1, hi: h1 }, Bounds::Range { lo: l2, hi: h2 })
                if *h1 < *l2 || *h2 < *l1 =>
            {
                Bounds::Const(0)
            }
            (Bounds::Range { lo, hi }, Bounds::Const(c))
            | (Bounds::Const(c), Bounds::Range { lo, hi })
                if *c < *lo || *c > *hi =>
            {
                Bounds::Const(0)
            }
            _ => Bounds::Bool,
        }
    }

    /// Inequality comparison: returns Bool or Const(0/1) if determinable.
    pub fn neq(&self, other: &Bounds) -> Bounds {
        match (self, other) {
            (Bounds::Const(a), Bounds::Const(b)) => Bounds::Const(if a != b { 1 } else { 0 }),
            // Disjoint ranges: definitely not equal → result is 1
            (Bounds::Range { lo: l1, hi: h1 }, Bounds::Range { lo: l2, hi: h2 })
                if *h1 < *l2 || *h2 < *l1 =>
            {
                Bounds::Const(1)
            }
            (Bounds::Range { lo, hi }, Bounds::Const(c))
            | (Bounds::Const(c), Bounds::Range { lo, hi })
                if *c < *lo || *c > *hi =>
            {
                Bounds::Const(1)
            }
            _ => Bounds::Bool,
        }
    }

    /// Less than comparison.
    pub fn lt(&self, other: &Bounds) -> Bounds {
        match (self, other) {
            (Bounds::Const(a), Bounds::Const(b)) => Bounds::Const(if a < b { 1 } else { 0 }),
            // If self.hi < other.lo, definitely less than
            (Bounds::Range { hi: h1, .. }, Bounds::Range { lo: l2, .. }) if *h1 < *l2 => {
                Bounds::Const(1)
            }
            (Bounds::Range { hi, .. }, Bounds::Const(c)) if *hi < *c => Bounds::Const(1),
            (Bounds::Const(c), Bounds::Range { lo, .. }) if *c < *lo => Bounds::Const(1),
            // If self.lo >= other.hi, definitely not less than
            (Bounds::Range { lo: l1, .. }, Bounds::Range { hi: h2, .. }) if *l1 >= *h2 => {
                Bounds::Const(0)
            }
            (Bounds::Range { lo, .. }, Bounds::Const(c)) if *lo >= *c => Bounds::Const(0),
            (Bounds::Const(c), Bounds::Range { hi, .. }) if *c >= *hi => Bounds::Const(0),
            _ => Bounds::Bool,
        }
    }

    /// Greater than comparison.
    pub fn gt(&self, other: &Bounds) -> Bounds {
        // gt(a, b) = lt(b, a)
        other.lt(self)
    }

    /// Less than or equal comparison.
    pub fn lte(&self, other: &Bounds) -> Bounds {
        match (self, other) {
            (Bounds::Const(a), Bounds::Const(b)) => Bounds::Const(if a <= b { 1 } else { 0 }),
            // If self.hi <= other.lo, definitely <=
            (Bounds::Range { hi: h1, .. }, Bounds::Range { lo: l2, .. }) if *h1 <= *l2 => {
                Bounds::Const(1)
            }
            // If self.lo > other.hi, definitely not <=
            (Bounds::Range { lo: l1, .. }, Bounds::Range { hi: h2, .. }) if *l1 > *h2 => {
                Bounds::Const(0)
            }
            _ => Bounds::Bool,
        }
    }

    /// Greater than or equal comparison.
    pub fn gte(&self, other: &Bounds) -> Bounds {
        other.lte(self)
    }

    /// Increment by 1
    pub fn incr(&self) -> Bounds {
        self.add(&Bounds::Const(1))
    }

    /// Decrement by 1
    pub fn decr(&self) -> Bounds {
        self.sub(&Bounds::Const(1))
    }

    /// Bitwise AND - result is bounded by smaller operand
    pub fn bitand(&self, other: &Bounds) -> Bounds {
        match (self.as_range(), other.as_range()) {
            (Some((_, h1)), Some((_, h2))) => {
                // Result is at most min(h1, h2)
                Bounds::Range {
                    lo: 0,
                    hi: h1.min(h2),
                }
            }
            _ => Bounds::Field,
        }
    }

    /// Bitwise OR - result is bounded by larger operand (approximately)
    pub fn bitor(&self, other: &Bounds) -> Bounds {
        match (self.as_range(), other.as_range()) {
            (Some((l1, h1)), Some((l2, h2))) => {
                // Result is at least max(l1, l2), at most can be larger
                // Conservative: assume result could be up to max of both
                Bounds::Range {
                    lo: l1.max(l2),
                    hi: h1.max(h2),
                }
            }
            _ => Bounds::Field,
        }
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

    // ═══════════════════════════════════════════════════════════════════════
    // P1: Bounds arithmetic propagation tests
    // ═══════════════════════════════════════════════════════════════════════

    #[test]
    fn test_bounds_add_const_const() {
        let a = Bounds::Const(10);
        let b = Bounds::Const(5);
        assert_eq!(a.add(&b), Bounds::Const(15));
    }

    #[test]
    fn test_bounds_add_const_range() {
        let c = Bounds::Const(10);
        let r = Bounds::Range { lo: 0, hi: 100 };
        // 10 + [0, 100] = [10, 110]
        assert_eq!(c.add(&r), Bounds::Range { lo: 10, hi: 110 });
        assert_eq!(r.add(&c), Bounds::Range { lo: 10, hi: 110 });
    }

    #[test]
    fn test_bounds_add_range_range() {
        let r1 = Bounds::Range { lo: 0, hi: 10 };
        let r2 = Bounds::Range { lo: 5, hi: 15 };
        // [0, 10] + [5, 15] = [5, 25]
        assert_eq!(r1.add(&r2), Bounds::Range { lo: 5, hi: 25 });
    }

    #[test]
    fn test_bounds_sub_const_const() {
        let a = Bounds::Const(10);
        let b = Bounds::Const(3);
        assert_eq!(a.sub(&b), Bounds::Const(7));
    }

    #[test]
    fn test_bounds_sub_range_const() {
        let r = Bounds::Range { lo: 10, hi: 100 };
        let c = Bounds::Const(5);
        // [10, 100] - 5 = [5, 95]
        assert_eq!(r.sub(&c), Bounds::Range { lo: 5, hi: 95 });
    }

    #[test]
    fn test_bounds_sub_const_range_no_underflow() {
        let c = Bounds::Const(100);
        let r = Bounds::Range { lo: 10, hi: 50 };
        // 100 - [10, 50] = [50, 90] (when c >= hi)
        assert_eq!(c.sub(&r), Bounds::Range { lo: 50, hi: 90 });
    }

    #[test]
    fn test_bounds_mul_const_const() {
        let a = Bounds::Const(7);
        let b = Bounds::Const(6);
        assert_eq!(a.mul(&b), Bounds::Const(42));
    }

    #[test]
    fn test_bounds_mul_const_range() {
        let c = Bounds::Const(3);
        let r = Bounds::Range { lo: 0, hi: 10 };
        // 3 * [0, 10] = [0, 30]
        assert_eq!(c.mul(&r), Bounds::Range { lo: 0, hi: 30 });
    }

    #[test]
    fn test_bounds_mul_by_zero() {
        let zero = Bounds::Const(0);
        let r = Bounds::Range { lo: 100, hi: 200 };
        assert_eq!(zero.mul(&r), Bounds::Const(0));
        assert_eq!(r.mul(&zero), Bounds::Const(0));
    }

    #[test]
    fn test_bounds_mul_by_one() {
        let one = Bounds::Const(1);
        let r = Bounds::Range { lo: 10, hi: 20 };
        assert_eq!(one.mul(&r), Bounds::Range { lo: 10, hi: 20 });
        assert_eq!(r.mul(&one), Bounds::Range { lo: 10, hi: 20 });
    }

    #[test]
    fn test_bounds_div_const_const() {
        let a = Bounds::Const(42);
        let b = Bounds::Const(6);
        assert_eq!(a.div(&b), Bounds::Const(7));
    }

    #[test]
    fn test_bounds_div_range_const() {
        let r = Bounds::Range { lo: 10, hi: 100 };
        let c = Bounds::Const(10);
        // [10, 100] / 10 = [1, 10]
        assert_eq!(r.div(&c), Bounds::Range { lo: 1, hi: 10 });
    }

    #[test]
    fn test_bounds_modulo_range() {
        let r = Bounds::Range { lo: 0, hi: 1000 };
        let c = Bounds::Const(100);
        // [0, 1000] % 100 = [0, 99]
        assert_eq!(r.modulo(&c), Bounds::Range { lo: 0, hi: 99 });
    }

    #[test]
    fn test_bounds_lt_disjoint_ranges() {
        let r1 = Bounds::Range { lo: 0, hi: 10 };
        let r2 = Bounds::Range { lo: 20, hi: 30 };
        // [0, 10] < [20, 30] is always true
        assert_eq!(r1.lt(&r2), Bounds::Const(1));
        // [20, 30] < [0, 10] is always false
        assert_eq!(r2.lt(&r1), Bounds::Const(0));
    }

    #[test]
    fn test_bounds_neq_disjoint_ranges() {
        let r1 = Bounds::Range { lo: 0, hi: 10 };
        let r2 = Bounds::Range { lo: 20, hi: 30 };
        // Disjoint ranges: always not equal
        assert_eq!(r1.neq(&r2), Bounds::Const(1));
    }

    #[test]
    fn test_bounds_eq_overlapping_ranges() {
        let r1 = Bounds::Range { lo: 0, hi: 20 };
        let r2 = Bounds::Range { lo: 10, hi: 30 };
        // Overlapping ranges: could be equal or not
        assert_eq!(r1.eq(&r2), Bounds::Bool);
    }

    #[test]
    fn test_bounds_incr_decr() {
        let c = Bounds::Const(10);
        assert_eq!(c.incr(), Bounds::Const(11));
        assert_eq!(c.decr(), Bounds::Const(9));

        let r = Bounds::Range { lo: 5, hi: 15 };
        assert_eq!(r.incr(), Bounds::Range { lo: 6, hi: 16 });
        assert_eq!(r.decr(), Bounds::Range { lo: 4, hi: 14 });
    }

    #[test]
    fn test_bounds_bitand() {
        let r1 = Bounds::Range { lo: 0, hi: 255 };
        let r2 = Bounds::Range { lo: 0, hi: 15 };
        // AND is bounded by smaller operand
        let result = r1.bitand(&r2);
        assert!(matches!(result, Bounds::Range { lo: 0, hi: 15 }));
    }
}
