//! Value tracking types for provenance analysis.
//!
//! This module provides types for tracking where values come from (their origin)
//! and whether they have been validated.

use miden_debug_types::SourceSpan;

use super::bounds::Bounds;

// ═══════════════════════════════════════════════════════════════════════════
// Value Origin Tracking
// ═══════════════════════════════════════════════════════════════════════════

/// Tracks where a value originated from (its provenance).
#[derive(Clone, Debug, Default)]
pub enum ValueOrigin {
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
    #[default]
    Derived,
    /// Input parameter to procedure
    ProcInput { position: usize },
    /// Return value from procedure call
    ProcReturn { target: String },
}

impl ValueOrigin {
    /// Returns true if this origin is untrusted external input.
    pub fn is_untrusted(&self) -> bool {
        matches!(
            self,
            ValueOrigin::AdviceStack | ValueOrigin::AdviceMap | ValueOrigin::MerkleStore
        )
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Validation State
// ═══════════════════════════════════════════════════════════════════════════

/// Validation state of a taint.
#[derive(Clone, Debug, Default)]
pub enum ValidationState {
    /// Not validated
    #[default]
    None,
    /// Validated (e.g., by u32assert)
    Validated,
}

// ═══════════════════════════════════════════════════════════════════════════
// TrackedValue
// ═══════════════════════════════════════════════════════════════════════════

/// A tracked value with bounds, origin, and validation state.
///
/// This is the core type for tracking value metadata during analysis,
/// including where values came from and whether they've been validated.
#[derive(Clone, Debug)]
pub struct TrackedValue {
    /// Unique identifier for this tracked value
    pub id: u64,
    /// Known bounds on this value
    pub bounds: Bounds,
    /// Where this value originated from
    pub origin: ValueOrigin,
    /// Whether this value has been validated
    pub validated: ValidationState,
    /// Location where this value was created/loaded
    pub source_span: Option<SourceSpan>,
}

impl TrackedValue {
    /// Returns true if this value has been validated.
    pub fn is_validated(&self) -> bool {
        matches!(self.validated, ValidationState::Validated)
    }

    /// Mark this value as validated and constrain bounds to u32.
    pub fn apply_validation(&mut self) {
        self.validated = ValidationState::Validated;
        // Also constrain bounds to u32 after validation
        if matches!(self.bounds, Bounds::Field) {
            self.bounds = Bounds::u32();
        }
    }
}

impl Default for TrackedValue {
    fn default() -> Self {
        Self {
            id: 0,
            bounds: Bounds::Field,
            origin: ValueOrigin::Literal,
            validated: ValidationState::None,
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
    fn test_value_origin_untrusted() {
        assert!(ValueOrigin::AdviceStack.is_untrusted());
        assert!(ValueOrigin::AdviceMap.is_untrusted());
        assert!(ValueOrigin::MerkleStore.is_untrusted());
        assert!(!ValueOrigin::Literal.is_untrusted());
        assert!(!ValueOrigin::Memory.is_untrusted());
        assert!(!ValueOrigin::Derived.is_untrusted());
    }

    #[test]
    fn test_tracked_value_default() {
        let value = TrackedValue::default();
        assert_eq!(value.id, 0);
        assert_eq!(value.bounds, Bounds::Field);
        assert!(!value.is_validated());
    }

    #[test]
    fn test_tracked_value_validation() {
        let mut value = TrackedValue {
            id: 1,
            bounds: Bounds::Field,
            origin: ValueOrigin::AdviceStack,
            validated: ValidationState::None,
            source_span: None,
        };

        assert!(!value.is_validated());
        value.apply_validation();
        assert!(value.is_validated());
        assert!(value.bounds.is_u32());
    }
}
