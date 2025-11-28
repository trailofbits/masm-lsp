//! Taint tracking types for value provenance analysis.
//!
//! This module provides types for tracking where values come from (their source)
//! and whether they have been validated.

use miden_debug_types::SourceSpan;

use super::bounds::Bounds;

// ═══════════════════════════════════════════════════════════════════════════
// Source Tracking
// ═══════════════════════════════════════════════════════════════════════════

/// Tracks where a value came from (its provenance).
#[derive(Clone, Debug, Default)]
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
    #[default]
    Derived,
    /// Input parameter to procedure
    ProcInput { position: usize },
    /// Return value from procedure call
    ProcReturn { target: String },
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
// Taint
// ═══════════════════════════════════════════════════════════════════════════

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

impl Default for Taint {
    fn default() -> Self {
        Self {
            id: 0,
            bounds: Bounds::Field,
            source: Source::Literal,
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
    fn test_source_untrusted() {
        assert!(Source::AdviceStack.is_untrusted());
        assert!(Source::AdviceMap.is_untrusted());
        assert!(Source::MerkleStore.is_untrusted());
        assert!(!Source::Literal.is_untrusted());
        assert!(!Source::Memory.is_untrusted());
        assert!(!Source::Derived.is_untrusted());
    }

    #[test]
    fn test_taint_default() {
        let taint = Taint::default();
        assert_eq!(taint.id, 0);
        assert_eq!(taint.bounds, Bounds::Field);
        assert!(!taint.is_validated());
    }

    #[test]
    fn test_taint_validation() {
        let mut taint = Taint {
            id: 1,
            bounds: Bounds::Field,
            source: Source::AdviceStack,
            validated: ValidationState::None,
            source_span: None,
        };

        assert!(!taint.is_validated());
        taint.apply_validation();
        assert!(taint.is_validated());
        assert!(taint.bounds.is_u32());
    }
}
