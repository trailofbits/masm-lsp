//! Analysis environment for uninitialized-local-read tracking.
//!
//! The [`LocalInitEnv`] tracks which frame cells may still be uninitialized
//! and maps SSA variables to abstract addresses that may refer to local
//! frame cells.

use std::collections::HashMap;

use masm_decompiler::ir::{IndexExpr, ValueId, Var, VarBase};

use super::domain::{AddrAbs, CellSet, FrameLayout};

/// Internal key used to track variable-specific analysis state.
///
/// Normal SSA values collapse by [`ValueId`], while lifted repeat loop inputs
/// remain distinct by both loop depth and subscript.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum TrackedVarKey {
    /// Concrete SSA value.
    Value(ValueId),
    /// Lifted repeat-loop input.
    LoopInput {
        /// Nesting depth of the repeat loop.
        loop_depth: usize,
        /// SSA subscript for this loop-input variable.
        subscript: IndexExpr,
    },
}

/// Analysis environment for one point in the program.
///
/// Tracks which local frame cells may be uninitialized and which SSA
/// variables hold addresses that refer to (parts of) the local frame.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct LocalInitEnv {
    /// Frame layout for the enclosing procedure.
    pub(crate) frame: FrameLayout,
    /// Set of frame cells that may still be uninitialized.
    pub(crate) maybe_uninit: CellSet,
    /// Address abstractions tracked per variable identity.
    var_addrs: HashMap<TrackedVarKey, AddrAbs>,
    /// Known constant values for variable identities (used for addr+const folding).
    var_constants: HashMap<TrackedVarKey, u64>,
}

impl LocalInitEnv {
    /// Create a new environment where all frame cells start uninitialized.
    pub(crate) fn new(frame: FrameLayout) -> Self {
        let maybe_uninit = frame.all_cells();
        Self {
            frame,
            maybe_uninit,
            var_addrs: HashMap::new(),
            var_constants: HashMap::new(),
        }
    }

    /// Join two environments at a control-flow merge point.
    ///
    /// The result conservatively over-approximates both environments:
    /// - `maybe_uninit` is the union (either path may leave a cell uninitialized).
    /// - Address maps: only keys present in **both** sides are retained,
    ///   and their `AddrAbs` values are joined. Keys present in only one side
    ///   are dropped (conservative: the other path does not track the address).
    pub(crate) fn join(&self, other: &Self) -> Self {
        let maybe_uninit = self.maybe_uninit.union(&other.maybe_uninit);

        let mut var_addrs = HashMap::new();
        for (key, self_addr) in &self.var_addrs {
            if let Some(other_addr) = other.var_addrs.get(key) {
                var_addrs.insert(key.clone(), self_addr.join(other_addr));
            }
            // Keys only in self are dropped (other path doesn't track them).
        }

        // Constants: keep only where both sides agree on the same value.
        let mut var_constants = HashMap::new();
        for (key, val) in &self.var_constants {
            if other.var_constants.get(key) == Some(val) {
                var_constants.insert(key.clone(), *val);
            }
        }

        Self {
            frame: self.frame,
            maybe_uninit,
            var_addrs,
            var_constants,
        }
    }

    /// Mark the given cells as definitely written (remove from `maybe_uninit`).
    pub(crate) fn mark_definitely_written(&mut self, cells: &CellSet) {
        self.maybe_uninit = self.maybe_uninit.difference(cells);
    }

    /// Return the subset of `cells` that may still be uninitialized.
    pub(crate) fn cells_maybe_uninit(&self, cells: &CellSet) -> CellSet {
        self.maybe_uninit.intersection(cells)
    }

    /// Look up the abstract address for a variable.
    pub(crate) fn addr_for_var(&self, var: &Var) -> Option<&AddrAbs> {
        self.var_addrs.get(&tracked_var_key(var))
    }

    /// Set the abstract address for a variable.
    pub(crate) fn set_addr_for_var(&mut self, var: &Var, addr: AddrAbs) {
        self.var_addrs.insert(tracked_var_key(var), addr);
    }

    /// Remove address tracking for a variable.
    pub(crate) fn clear_addr_for_var(&mut self, var: &Var) {
        self.var_addrs.remove(&tracked_var_key(var));
    }

    /// Record a known constant value for a variable.
    pub(crate) fn set_constant_for_var(&mut self, var: &Var, value: u64) {
        self.var_constants.insert(tracked_var_key(var), value);
    }

    /// Remove constant tracking for a variable.
    pub(crate) fn clear_constant_for_var(&mut self, var: &Var) {
        self.var_constants.remove(&tracked_var_key(var));
    }

    /// Look up the constant value for a variable, if known.
    pub(crate) fn constant_for_var(&self, var: &Var) -> Option<u64> {
        self.var_constants.get(&tracked_var_key(var)).copied()
    }
}

/// Convert a variable to the tracked key used by [`LocalInitEnv`].
fn tracked_var_key(var: &Var) -> TrackedVarKey {
    match &var.base {
        VarBase::Value(value_id) => TrackedVarKey::Value(*value_id),
        VarBase::LoopInput { loop_depth } => TrackedVarKey::LoopInput {
            loop_depth: *loop_depth,
            subscript: var.subscript.clone(),
        },
    }
}
