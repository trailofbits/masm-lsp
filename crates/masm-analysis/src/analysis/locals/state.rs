//! Local variable initialization state tracking.
//!
//! This module provides types for tracking which local variables have been
//! initialized during procedure analysis.

use std::collections::BTreeMap;

// ═══════════════════════════════════════════════════════════════════════════
// LocalState - initialization state for a single local
// ═══════════════════════════════════════════════════════════════════════════

/// Initialization state for a single local variable index.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LocalState {
    /// Local has not been written on any path to this point
    Uninitialized,
    /// Local has been written on some but not all paths
    MaybeInitialized,
    /// Local has been written on all paths to this point
    Initialized,
}

impl LocalState {
    /// Lattice meet operation for control flow joins.
    ///
    /// At a join point (e.g., after if/else), the resulting state is:
    /// - Initialized only if both paths initialized
    /// - Uninitialized only if neither path initialized
    /// - MaybeInitialized otherwise
    pub fn meet(self, other: Self) -> Self {
        match (self, other) {
            (Self::Initialized, Self::Initialized) => Self::Initialized,
            (Self::Uninitialized, Self::Uninitialized) => Self::Uninitialized,
            _ => Self::MaybeInitialized,
        }
    }

    /// Returns true if this state indicates definite initialization.
    pub fn is_initialized(self) -> bool {
        matches!(self, Self::Initialized)
    }

    /// Returns true if this state indicates possible uninitialized access.
    pub fn may_be_uninitialized(self) -> bool {
        !matches!(self, Self::Initialized)
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// LocalsState - initialization state for all locals in a procedure
// ═══════════════════════════════════════════════════════════════════════════

/// Tracks initialization state for all local variables in a procedure.
///
/// Uses a sparse map where untracked indices are implicitly `Uninitialized`.
#[derive(Clone, Debug, Default)]
pub struct LocalsState {
    /// Map from local index to initialization state.
    /// Indices not in the map are implicitly `Uninitialized`.
    states: BTreeMap<u16, LocalState>,
}

impl LocalsState {
    /// Create a new empty locals state (all locals uninitialized).
    pub fn new() -> Self {
        Self::default()
    }

    /// Mark a single local as initialized.
    pub fn init_single(&mut self, idx: u16) {
        self.states.insert(idx, LocalState::Initialized);
    }

    /// Mark a word (4 consecutive locals) as initialized.
    ///
    /// This is used for `loc_storew` which writes 4 elements starting at `base_idx`.
    pub fn init_word(&mut self, base_idx: u16) {
        for i in 0..4 {
            self.states.insert(base_idx + i, LocalState::Initialized);
        }
    }

    /// Get the initialization state of a local.
    ///
    /// Returns `Uninitialized` for indices not explicitly tracked.
    pub fn get(&self, idx: u16) -> LocalState {
        self.states
            .get(&idx)
            .copied()
            .unwrap_or(LocalState::Uninitialized)
    }

    /// Check if a single local is safe to read (definitely initialized).
    pub fn is_initialized(&self, idx: u16) -> bool {
        self.get(idx).is_initialized()
    }

    /// Check if all locals in a word are safe to read.
    pub fn is_word_initialized(&self, base_idx: u16) -> bool {
        (0..4).all(|i| self.is_initialized(base_idx + i))
    }

    /// Find the first uninitialized local in a word range.
    ///
    /// Returns `Some((idx, state))` for the first problematic local,
    /// or `None` if all are initialized.
    pub fn first_uninitialized_in_word(&self, base_idx: u16) -> Option<(u16, LocalState)> {
        for i in 0..4 {
            let idx = base_idx + i;
            let state = self.get(idx);
            if state.may_be_uninitialized() {
                return Some((idx, state));
            }
        }
        None
    }

    /// Meet two states at a control flow join point.
    ///
    /// The result contains the meet of states from both paths:
    /// - A local is `Initialized` only if initialized in both states
    /// - A local is `Uninitialized` only if uninitialized in both states
    /// - Otherwise it's `MaybeInitialized`
    pub fn meet(&self, other: &Self) -> Self {
        let mut result = LocalsState::new();

        // Process all indices that appear in either state
        let all_indices: std::collections::BTreeSet<u16> = self
            .states
            .keys()
            .chain(other.states.keys())
            .copied()
            .collect();

        for idx in all_indices {
            let self_state = self.get(idx);
            let other_state = other.get(idx);
            let merged = self_state.meet(other_state);

            // Only store non-Uninitialized states (sparse representation)
            if merged != LocalState::Uninitialized {
                result.states.insert(idx, merged);
            }
        }

        result
    }

    /// Check if two states are equal.
    pub fn equals(&self, other: &Self) -> bool {
        // Compare all tracked indices
        let all_indices: std::collections::BTreeSet<u16> = self
            .states
            .keys()
            .chain(other.states.keys())
            .copied()
            .collect();

        for idx in all_indices {
            if self.get(idx) != other.get(idx) {
                return false;
            }
        }
        true
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Tests
// ═══════════════════════════════════════════════════════════════════════════

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_local_state_meet() {
        use LocalState::*;

        // Same states
        assert_eq!(Initialized.meet(Initialized), Initialized);
        assert_eq!(Uninitialized.meet(Uninitialized), Uninitialized);
        assert_eq!(MaybeInitialized.meet(MaybeInitialized), MaybeInitialized);

        // Mixed states -> MaybeInitialized
        assert_eq!(Initialized.meet(Uninitialized), MaybeInitialized);
        assert_eq!(Uninitialized.meet(Initialized), MaybeInitialized);
        assert_eq!(Initialized.meet(MaybeInitialized), MaybeInitialized);
        assert_eq!(MaybeInitialized.meet(Initialized), MaybeInitialized);
        assert_eq!(Uninitialized.meet(MaybeInitialized), MaybeInitialized);
        assert_eq!(MaybeInitialized.meet(Uninitialized), MaybeInitialized);
    }

    #[test]
    fn test_locals_state_new() {
        let state = LocalsState::new();
        assert_eq!(state.get(0), LocalState::Uninitialized);
        assert_eq!(state.get(100), LocalState::Uninitialized);
    }

    #[test]
    fn test_locals_state_init_single() {
        let mut state = LocalsState::new();
        state.init_single(5);

        assert_eq!(state.get(4), LocalState::Uninitialized);
        assert_eq!(state.get(5), LocalState::Initialized);
        assert_eq!(state.get(6), LocalState::Uninitialized);
    }

    #[test]
    fn test_locals_state_init_word() {
        let mut state = LocalsState::new();
        state.init_word(4);

        assert_eq!(state.get(3), LocalState::Uninitialized);
        assert_eq!(state.get(4), LocalState::Initialized);
        assert_eq!(state.get(5), LocalState::Initialized);
        assert_eq!(state.get(6), LocalState::Initialized);
        assert_eq!(state.get(7), LocalState::Initialized);
        assert_eq!(state.get(8), LocalState::Uninitialized);
    }

    #[test]
    fn test_locals_state_meet() {
        // State where local 0 is initialized
        let mut state1 = LocalsState::new();
        state1.init_single(0);
        state1.init_single(1);

        // State where local 1 is initialized
        let mut state2 = LocalsState::new();
        state2.init_single(1);
        state2.init_single(2);

        let merged = state1.meet(&state2);

        // Local 0: Initialized meet Uninitialized = MaybeInitialized
        assert_eq!(merged.get(0), LocalState::MaybeInitialized);
        // Local 1: Initialized meet Initialized = Initialized
        assert_eq!(merged.get(1), LocalState::Initialized);
        // Local 2: Uninitialized meet Initialized = MaybeInitialized
        assert_eq!(merged.get(2), LocalState::MaybeInitialized);
        // Local 3: Uninitialized meet Uninitialized = Uninitialized
        assert_eq!(merged.get(3), LocalState::Uninitialized);
    }

    #[test]
    fn test_is_word_initialized() {
        let mut state = LocalsState::new();

        // Partially initialized word
        state.init_single(0);
        state.init_single(1);
        assert!(!state.is_word_initialized(0));

        // Fully initialized word
        state.init_single(2);
        state.init_single(3);
        assert!(state.is_word_initialized(0));
    }

    #[test]
    fn test_first_uninitialized_in_word() {
        let mut state = LocalsState::new();
        state.init_single(4);
        state.init_single(5);

        // Word at 4: indices 4,5 initialized, 6,7 not
        let result = state.first_uninitialized_in_word(4);
        assert_eq!(result, Some((6, LocalState::Uninitialized)));

        // Fully initialize the word
        state.init_single(6);
        state.init_single(7);
        assert_eq!(state.first_uninitialized_in_word(4), None);
    }
}
