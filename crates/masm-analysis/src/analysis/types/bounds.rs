//! Value bounds tracking for static analysis.
//!
//! This module provides the `Bounds` type for tracking what we know about
//! a value's possible range. This enables precise analysis of arithmetic
//! operations and range-based optimizations.

// ═══════════════════════════════════════════════════════════════════════════
// Constants
// ═══════════════════════════════════════════════════════════════════════════

pub const U32_MAX: u64 = 0xFFFF_FFFF;

/// Goldilocks prime field modulus: 2^64 - 2^32 + 1
pub const FIELD_MODULUS: u64 = 0xFFFF_FFFF_0000_0001;

// ═══════════════════════════════════════════════════════════════════════════
// Bounds Type
// ═══════════════════════════════════════════════════════════════════════════

/// Tracks what we know about a value's possible range.
#[derive(Clone, Debug, PartialEq, Default)]
pub enum Bounds {
    /// No constraints - full field element
    #[default]
    Field,
    /// Known exact value
    Const(u64),
    /// Known range [lo, hi] inclusive
    Range { lo: u64, hi: u64 },
    /// Boolean: exactly 0 or 1
    Bool,
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
    pub fn as_range(&self) -> Option<(u64, u64)> {
        match self {
            Bounds::Const(v) => Some((*v, *v)),
            Bounds::Range { lo, hi } => Some((*lo, *hi)),
            Bounds::Bool => Some((0, 1)),
            Bounds::Field => None,
        }
    }

    /// Add two bounds, propagating constants and ranges where possible.
    ///
    /// Field-aware: if the result might overflow u64 (i.e., exceed FIELD_MODULUS),
    /// we conservatively return `Bounds::Field` since the exact value depends on
    /// field reduction.
    pub fn add(&self, other: &Bounds) -> Bounds {
        match (self, other) {
            // Const + Const = Const (with field overflow check)
            (Bounds::Const(a), Bounds::Const(b)) => {
                match a.checked_add(*b) {
                    Some(sum) if sum < FIELD_MODULUS => Bounds::Const(sum),
                    _ => Bounds::Field, // Overflow or exceeds field
                }
            }

            // Const + Range or Range + Const = shifted Range
            (Bounds::Const(c), Bounds::Range { lo, hi })
            | (Bounds::Range { lo, hi }, Bounds::Const(c)) => {
                // Check for overflow
                match (lo.checked_add(*c), hi.checked_add(*c)) {
                    (Some(new_lo), Some(new_hi)) if new_hi < FIELD_MODULUS => {
                        if new_lo == new_hi {
                            Bounds::Const(new_lo)
                        } else {
                            Bounds::Range {
                                lo: new_lo,
                                hi: new_hi,
                            }
                        }
                    }
                    _ => Bounds::Field, // Overflow
                }
            }

            // Range + Range = widened Range
            (Bounds::Range { lo: l1, hi: h1 }, Bounds::Range { lo: l2, hi: h2 }) => {
                match (l1.checked_add(*l2), h1.checked_add(*h2)) {
                    (Some(new_lo), Some(new_hi)) if new_hi < FIELD_MODULUS => Bounds::Range {
                        lo: new_lo,
                        hi: new_hi,
                    },
                    _ => Bounds::Field, // Overflow
                }
            }

            // Bool operations
            (Bounds::Bool, Bounds::Bool) => Bounds::Range { lo: 0, hi: 2 },
            (Bounds::Bool, Bounds::Const(c)) | (Bounds::Const(c), Bounds::Bool) => {
                match c.checked_add(1) {
                    Some(new_hi) if new_hi < FIELD_MODULUS => Bounds::Range { lo: *c, hi: new_hi },
                    _ => Bounds::Field,
                }
            }

            _ => Bounds::Field,
        }
    }

    /// Subtract: self - other, propagating constants and ranges where possible.
    ///
    /// Field-aware: if `a < b`, then `a - b` in field arithmetic wraps to a large
    /// positive value near the field modulus. We conservatively return `Bounds::Field`
    /// when underflow is possible.
    pub fn sub(&self, other: &Bounds) -> Bounds {
        match (self, other) {
            // Const - Const = Const (with underflow check)
            (Bounds::Const(a), Bounds::Const(b)) => {
                if *a >= *b {
                    Bounds::Const(a - b)
                } else {
                    // Underflow wraps in field arithmetic
                    Bounds::Field
                }
            }

            // Range - Const = shifted Range (only if no underflow)
            (Bounds::Range { lo, hi }, Bounds::Const(c)) => {
                if *lo >= *c {
                    // No underflow possible
                    let new_lo = lo - c;
                    let new_hi = hi - c;
                    if new_lo == new_hi {
                        Bounds::Const(new_lo)
                    } else {
                        Bounds::Range {
                            lo: new_lo,
                            hi: new_hi,
                        }
                    }
                } else {
                    // May underflow
                    Bounds::Field
                }
            }

            // Const - Range: only valid if c >= hi (no underflow possible)
            (Bounds::Const(c), Bounds::Range { lo, hi }) => {
                if *c >= *hi {
                    // No underflow possible: result is [c - hi, c - lo]
                    Bounds::Range {
                        lo: c - hi,
                        hi: c - lo,
                    }
                } else {
                    // May underflow
                    Bounds::Field
                }
            }

            // Range - Range: only valid if l1 >= h2 (no underflow possible)
            (Bounds::Range { lo: l1, hi: h1 }, Bounds::Range { lo: l2, hi: h2 }) => {
                if *l1 >= *h2 {
                    // No underflow: [l1-h2, h1-l2]
                    Bounds::Range {
                        lo: l1 - h2,
                        hi: h1 - l2,
                    }
                } else {
                    Bounds::Field
                }
            }

            _ => Bounds::Field,
        }
    }

    /// Multiply two bounds, propagating constants and ranges where possible.
    ///
    /// Field-aware: multiplication can easily overflow, so we check using
    /// `checked_mul` and return `Bounds::Field` if the result exceeds the
    /// field modulus.
    pub fn mul(&self, other: &Bounds) -> Bounds {
        match (self, other) {
            // Const * Const = Const (with overflow check)
            (Bounds::Const(a), Bounds::Const(b)) => {
                match a.checked_mul(*b) {
                    Some(prod) if prod < FIELD_MODULUS => Bounds::Const(prod),
                    _ => Bounds::Field, // Overflow
                }
            }

            // Const(0) * anything = 0, anything * Const(0) = 0
            (Bounds::Const(0), _) | (_, Bounds::Const(0)) => Bounds::Const(0),

            // Const(1) * x = x, x * Const(1) = x
            (Bounds::Const(1), other) | (other, Bounds::Const(1)) => other.clone(),

            // Const * Range or Range * Const = scaled Range (with overflow check)
            (Bounds::Const(c), Bounds::Range { lo, hi })
            | (Bounds::Range { lo, hi }, Bounds::Const(c)) => {
                match (lo.checked_mul(*c), hi.checked_mul(*c)) {
                    (Some(new_lo), Some(new_hi)) if new_hi < FIELD_MODULUS => {
                        if new_lo == new_hi {
                            Bounds::Const(new_lo)
                        } else {
                            Bounds::Range {
                                lo: new_lo,
                                hi: new_hi,
                            }
                        }
                    }
                    _ => Bounds::Field, // Overflow
                }
            }

            // Bool * anything: result is in [0, max_other] (no overflow possible for 0 or 1)
            (Bounds::Bool, other) | (other, Bounds::Bool) => {
                if let Some((_, hi)) = other.as_range() {
                    if hi < FIELD_MODULUS {
                        Bounds::Range { lo: 0, hi }
                    } else {
                        Bounds::Field
                    }
                } else {
                    Bounds::Field
                }
            }

            // Range * Range: check for overflow
            (Bounds::Range { lo: l1, hi: h1 }, Bounds::Range { lo: l2, hi: h2 }) => {
                match (l1.checked_mul(*l2), h1.checked_mul(*h2)) {
                    (Some(new_lo), Some(new_hi)) if new_hi < FIELD_MODULUS => Bounds::Range {
                        lo: new_lo,
                        hi: new_hi,
                    },
                    _ => Bounds::Field, // Overflow
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

    /// Join two bounds into a single bounds that encompasses both.
    ///
    /// This is used for merging control flow paths (if/else branches).
    pub fn join(&self, other: &Bounds) -> Bounds {
        match (self, other) {
            // Same bounds
            (a, b) if a == b => a.clone(),

            // Const join: if same value, keep it; otherwise make a range
            (Bounds::Const(a), Bounds::Const(b)) => {
                let (lo, hi) = if a < b { (*a, *b) } else { (*b, *a) };
                Bounds::Range { lo, hi }
            }

            // Const and Range: extend range to include const
            (Bounds::Const(c), Bounds::Range { lo, hi })
            | (Bounds::Range { lo, hi }, Bounds::Const(c)) => Bounds::Range {
                lo: (*lo).min(*c),
                hi: (*hi).max(*c),
            },

            // Range join: union of ranges
            (Bounds::Range { lo: l1, hi: h1 }, Bounds::Range { lo: l2, hi: h2 }) => Bounds::Range {
                lo: (*l1).min(*l2),
                hi: (*h1).max(*h2),
            },

            // Bool joins
            (Bounds::Bool, Bounds::Bool) => Bounds::Bool,
            (Bounds::Bool, Bounds::Const(c)) | (Bounds::Const(c), Bounds::Bool) if *c <= 1 => {
                Bounds::Bool
            }
            (Bounds::Bool, Bounds::Const(c)) | (Bounds::Const(c), Bounds::Bool) => {
                Bounds::Range { lo: 0, hi: *c }
            }
            (Bounds::Bool, Bounds::Range { lo: _, hi })
            | (Bounds::Range { lo: _, hi }, Bounds::Bool) => {
                // Bool is [0, 1], so join with Range: lower bound is 0, upper bound is max(hi, 1)
                Bounds::Range {
                    lo: 0,
                    hi: (*hi).max(1),
                }
            }

            // Field dominates everything
            (Bounds::Field, _) | (_, Bounds::Field) => Bounds::Field,
        }
    }

    /// Widen bounds for fixed-point convergence in abstract interpretation.
    ///
    /// When iterating to a fixed point (e.g., analyzing while loops), if bounds
    /// keep changing, we need to widen to ensure termination. This operation
    /// is more aggressive than `join` - it jumps to a wider approximation.
    pub fn widen(&self, other: &Bounds) -> Bounds {
        match (self, other) {
            // Same bounds - no change
            (a, b) if a == b => a.clone(),

            // If either is already Field, stay Field
            (Bounds::Field, _) | (_, Bounds::Field) => Bounds::Field,

            // Const widening to Range or different Const
            (Bounds::Const(_), Bounds::Const(_)) => Bounds::Field,

            // Any Range change goes to Field for quick convergence
            (Bounds::Range { .. }, Bounds::Range { hi: h2, .. }) => {
                // If bounds are still "small", try to keep tracking
                if *h2 <= U32_MAX {
                    Bounds::u32()
                } else {
                    Bounds::Field
                }
            }

            // Const to Range: widen to the range or Field
            (Bounds::Const(_), Bounds::Range { hi, .. })
            | (Bounds::Range { hi, .. }, Bounds::Const(_)) => {
                if *hi <= U32_MAX {
                    Bounds::u32()
                } else {
                    Bounds::Field
                }
            }

            // Bool changes
            (Bounds::Bool, _) | (_, Bounds::Bool) => Bounds::Field,
        }
    }

    /// Right shift - divides by 2^shift_amount
    pub fn shr(&self, shift_amount: u64) -> Bounds {
        if shift_amount >= 64 {
            return Bounds::Const(0);
        }
        match self {
            Bounds::Const(v) => Bounds::Const(v >> shift_amount),
            Bounds::Range { lo, hi } => {
                let new_lo = lo >> shift_amount;
                let new_hi = hi >> shift_amount;
                if new_lo == new_hi {
                    Bounds::Const(new_lo)
                } else {
                    Bounds::Range {
                        lo: new_lo,
                        hi: new_hi,
                    }
                }
            }
            Bounds::Bool => Bounds::Bool, // 0 or 1 shifted right by any amount stays 0 or 0
            Bounds::Field => Bounds::Field,
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
    fn test_bounds_mul_const_const() {
        let a = Bounds::Const(7);
        let b = Bounds::Const(6);
        assert_eq!(a.mul(&b), Bounds::Const(42));
    }

    #[test]
    fn test_bounds_add_overflow() {
        // Values near field modulus should return Field on overflow
        let large = Bounds::Const(FIELD_MODULUS - 1);
        let one = Bounds::Const(2);
        // (FIELD_MODULUS - 1) + 2 exceeds FIELD_MODULUS
        assert_eq!(large.add(&one), Bounds::Field);

        // But staying under is fine
        let under = Bounds::Const(FIELD_MODULUS - 10);
        let small = Bounds::Const(5);
        assert_eq!(under.add(&small), Bounds::Const(FIELD_MODULUS - 5));
    }

    #[test]
    fn test_bounds_sub_underflow() {
        // 5 - 10 underflows in field arithmetic
        let a = Bounds::Const(5);
        let b = Bounds::Const(10);
        assert_eq!(a.sub(&b), Bounds::Field);

        // But 10 - 5 is fine
        assert_eq!(b.sub(&a), Bounds::Const(5));
    }

    #[test]
    fn test_bounds_join_same() {
        let c = Bounds::Const(10);
        assert_eq!(c.join(&c), Bounds::Const(10));

        let r = Bounds::Range { lo: 5, hi: 15 };
        assert_eq!(r.join(&r), Bounds::Range { lo: 5, hi: 15 });
    }

    #[test]
    fn test_bounds_widen_to_u32() {
        let r1 = Bounds::Range { lo: 0, hi: 100 };
        let r2 = Bounds::Range { lo: 0, hi: 200 };
        // Widening should jump to u32 bounds
        assert_eq!(r1.widen(&r2), Bounds::u32());
    }
}
