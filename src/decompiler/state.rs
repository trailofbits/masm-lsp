//! Decompiler state management.
//!
//! This module contains the symbolic stack state used during decompilation,
//! including dynamic input discovery and control flow merge handling.

use crate::analysis::stack_ops::StackLike;
use miden_debug_types::SourceSpan;

// ═══════════════════════════════════════════════════════════════════════════
// Named Value Stack
// ═══════════════════════════════════════════════════════════════════════════

/// A value on the symbolic stack with a name for pseudocode generation.
#[derive(Clone, Debug, Default)]
pub struct NamedValue {
    /// The variable name (e.g., "a_1", "v_2")
    pub name: String,
}

/// Saved stack state for control flow merge points.
#[derive(Clone, Debug)]
pub struct SavedStackState {
    /// The stack contents at the save point
    pub stack: Vec<NamedValue>,
}

/// Related information for a decompilation failure.
#[derive(Debug, Clone)]
pub struct FailureRelatedInfo {
    /// The span of the related location
    pub span: SourceSpan,
    /// Message explaining the related location
    pub message: String,
}

/// State for decompiling a procedure.
///
/// Supports dynamic input discovery: when an operation tries to access a stack
/// position that doesn't exist, a new input variable is automatically created.
#[derive(Debug)]
pub struct DecompilerState {
    /// The symbolic stack with named values
    pub stack: Vec<NamedValue>,
    /// Counter for generating variable names (v_1, v_2, ...)
    pub next_var_id: usize,
    /// Counter for dynamically discovered input variables
    /// This tracks how many "virtual" inputs exist below our current stack
    pub next_input_id: usize,
    /// Whether stack tracking has failed (e.g., after dynamic call)
    pub tracking_failed: bool,
    /// The span where tracking failed (for diagnostic reporting)
    pub failure_span: Option<SourceSpan>,
    /// The reason tracking failed
    pub failure_reason: Option<String>,
    /// Related information for the failure (e.g., root cause in called procedure)
    pub failure_related: Option<FailureRelatedInfo>,
    /// Counter for generating loop counter names (c_1, c_2, ...)
    pub next_counter_id: usize,
    /// Span of a loop that produced a dynamic/unknown number of stack items.
    /// When set, subsequent loops with non-zero net effect cannot be decompiled
    /// because we don't know the exact stack contents.
    pub dynamic_stack_source: Option<SourceSpan>,
}

/// Loop counter names in order of nesting depth.
pub const LOOP_COUNTER_NAMES: &[&str] = &["i", "j", "k", "l", "m", "n", "o", "p", "q"];

impl DecompilerState {
    /// Create a new state with procedure inputs on the stack.
    pub fn new(input_count: usize) -> Self {
        let mut stack = Vec::new();
        // Push inputs in reverse order so a_0 is on top after all pushes
        // Inputs are on stack with a_0 at top (position 0)
        // So we push them in order: a_(N-1) first (bottom), then ... a_1, a_0 (top)
        for i in (0..input_count).rev() {
            stack.push(NamedValue {
                name: format!("a_{}", i),
            });
        }
        Self {
            stack,
            next_var_id: 0,
            next_input_id: input_count,
            tracking_failed: false,
            failure_span: None,
            failure_reason: None,
            failure_related: None,
            next_counter_id: 0,
            dynamic_stack_source: None,
        }
    }

    /// Generate a new loop counter name (i, j, k, ...).
    pub fn new_counter(&mut self) -> String {
        let name = if self.next_counter_id < LOOP_COUNTER_NAMES.len() {
            LOOP_COUNTER_NAMES[self.next_counter_id].to_string()
        } else {
            // Fallback for deeply nested loops
            format!("i{}", self.next_counter_id - LOOP_COUNTER_NAMES.len())
        };
        self.next_counter_id += 1;
        name
    }

    /// Get the total number of inputs discovered (initial + dynamically discovered).
    pub fn total_inputs(&self) -> usize {
        self.next_input_id
    }

    /// Generate a new variable name (v_0, v_1, ...).
    pub fn new_var(&mut self) -> String {
        let name = format!("v_{}", self.next_var_id);
        self.next_var_id += 1;
        name
    }

    /// Generate a new input variable name (for dynamic discovery).
    pub fn new_input(&mut self) -> String {
        let name = format!("a_{}", self.next_input_id);
        self.next_input_id += 1;
        name
    }

    /// Push a new named value onto the stack.
    pub fn push_name(&mut self, name: String) {
        self.stack.push(NamedValue { name });
    }

    /// Pop a value from the stack, returning its name.
    /// If the stack is empty, dynamically discovers a new input.
    pub fn pop_name(&mut self) -> String {
        if let Some(v) = self.stack.pop() {
            v.name
        } else {
            // Stack underflow - discover a new input!
            // This input was "below" our visible stack when we started
            self.new_input()
        }
    }

    /// Peek at a value on the stack (0 = top), returning its name.
    /// If the position is beyond the current stack, returns a dynamically generated input name.
    pub fn peek_name(&self, n: usize) -> String {
        if n < self.stack.len() {
            self.stack[self.stack.len() - 1 - n].name.clone()
        } else {
            // Position is beyond our stack - this is accessing an input
            // Calculate which input index this would be
            let inputs_below = n - self.stack.len();
            format!("a_{}", self.next_input_id + inputs_below)
        }
    }

    /// Duplicate the value at position n onto the top and return its name.
    /// If n is beyond the stack, dynamically discovers inputs as needed.
    pub fn dup_name(&mut self, n: usize) -> String {
        self.ensure_depth(n + 1);
        let name = self.stack[self.stack.len() - 1 - n].name.clone();
        self.stack.push(NamedValue { name: name.clone() });
        name
    }

    /// Mark tracking as failed (e.g., after unknown procedure call).
    pub fn fail_tracking(&mut self, span: SourceSpan, reason: &str) {
        self.fail_tracking_with_related(span, reason, None);
    }

    /// Mark tracking as failed with optional related information.
    ///
    /// The related information can point to the root cause of the failure,
    /// such as a called procedure that couldn't be decompiled.
    pub fn fail_tracking_with_related(
        &mut self,
        span: SourceSpan,
        reason: &str,
        related: Option<FailureRelatedInfo>,
    ) {
        if !self.tracking_failed {
            // Only record the first failure
            self.tracking_failed = true;
            self.failure_span = Some(span);
            self.failure_reason = Some(reason.to_string());
            self.failure_related = related;
        }
        self.stack.clear();
    }

    /// Save the current stack state for later restoration or comparison.
    pub fn save_state(&self) -> SavedStackState {
        SavedStackState {
            stack: self.stack.clone(),
        }
    }

    /// Restore state from a saved snapshot.
    pub fn restore_state(&mut self, saved: &SavedStackState) {
        self.stack = saved.stack.clone();
        // Note: we intentionally DON'T restore next_var_id and next_input_id
        // because we want variable names to remain unique across branches
    }

    /// Get a mapping from current variable names to saved variable names at corresponding positions.
    ///
    /// This is used at control flow merge points to rename variables so that
    /// the same stack positions have consistent names.
    ///
    /// Returns a list of (current_name, target_name) pairs for variables that need renaming.
    pub fn get_rename_map(&self, saved: &SavedStackState) -> Vec<(String, String)> {
        let mut renames = Vec::new();
        let min_len = self.stack.len().min(saved.stack.len());

        // Compare positions from bottom of stack (oldest values) to top (newest)
        // Position 0 in the Vec is the bottom of the stack
        for i in 0..min_len {
            let current_name = &self.stack[i].name;
            let saved_name = &saved.stack[i].name;
            if current_name != saved_name {
                renames.push((current_name.clone(), saved_name.clone()));
            }
        }
        renames
    }

    /// Apply a rename map to the current stack state.
    pub fn apply_renames(&mut self, renames: &[(String, String)]) {
        for value in &mut self.stack {
            for (from, to) in renames {
                if &value.name == from {
                    value.name = to.clone();
                    break;
                }
            }
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// StackLike implementation for DecompilerState
// ─────────────────────────────────────────────────────────────────────────────

impl StackLike for DecompilerState {
    type Element = NamedValue;

    fn depth(&self) -> usize {
        self.stack.len()
    }

    fn push(&mut self, elem: NamedValue) {
        self.stack.push(elem);
    }

    fn pop(&mut self) -> NamedValue {
        self.stack.pop().unwrap_or_else(|| {
            // Stack underflow - discover a new input
            NamedValue {
                name: self.new_input(),
            }
        })
    }

    fn peek(&self, n: usize) -> Option<&NamedValue> {
        if n < self.stack.len() {
            Some(&self.stack[self.stack.len() - 1 - n])
        } else {
            None
        }
    }

    fn ensure_depth(&mut self, needed: usize) {
        while self.stack.len() < needed {
            // Add a new input at the bottom of the stack
            let input = NamedValue {
                name: self.new_input(),
            };
            self.stack.insert(0, input);
        }
    }

    fn swap(&mut self, a: usize, b: usize) {
        let max_pos = a.max(b);
        self.ensure_depth(max_pos + 1);
        let len = self.stack.len();
        let idx_a = len - 1 - a;
        let idx_b = len - 1 - b;
        self.stack.swap(idx_a, idx_b);
    }

    fn movup(&mut self, n: usize) {
        if n == 0 {
            return;
        }
        self.ensure_depth(n + 1);
        let len = self.stack.len();
        let idx = len - 1 - n;
        let elem = self.stack.remove(idx);
        self.stack.push(elem);
    }

    fn movdn(&mut self, n: usize) {
        if n == 0 {
            return;
        }
        self.ensure_depth(n + 1);
        let elem = self.stack.pop().unwrap();
        let len = self.stack.len();
        // Position n from top in final stack (len+1 elements) = index (len - n)
        let idx = len - n;
        self.stack.insert(idx, elem);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_decompiler_state_new() {
        let state = DecompilerState::new(3);
        assert_eq!(state.stack.len(), 3);
        assert_eq!(state.peek_name(0), "a_0");
        assert_eq!(state.peek_name(1), "a_1");
        assert_eq!(state.peek_name(2), "a_2");
    }

    #[test]
    fn test_decompiler_push_pop() {
        let mut state = DecompilerState::new(1);
        assert_eq!(state.peek_name(0), "a_0");

        state.push_name("v_0".to_string());
        assert_eq!(state.peek_name(0), "v_0");
        assert_eq!(state.peek_name(1), "a_0");

        assert_eq!(state.pop_name(), "v_0");
        assert_eq!(state.peek_name(0), "a_0");
    }

    #[test]
    fn test_decompiler_dup() {
        let mut state = DecompilerState::new(2);
        // Stack: [a_1, a_0] (a_0 on top)
        let duped = state.dup_name(1);
        assert_eq!(duped, "a_1");
    }

    #[test]
    fn test_decompiler_swap() {
        let mut state = DecompilerState::new(2);
        // Stack: [a_1, a_0] (a_0 on top)
        state.swap(0, 1);
        assert_eq!(state.peek_name(0), "a_1");
        assert_eq!(state.peek_name(1), "a_0");
    }

    #[test]
    fn test_dynamic_input_discovery_pop() {
        let mut state = DecompilerState::new(0); // Start with no inputs
        assert_eq!(state.total_inputs(), 0);

        // Pop from empty stack - should discover inputs
        let a = state.pop_name();
        assert_eq!(a, "a_0");
        assert_eq!(state.total_inputs(), 1);

        let b = state.pop_name();
        assert_eq!(b, "a_1");
        assert_eq!(state.total_inputs(), 2);
    }

    #[test]
    fn test_dynamic_input_discovery_dup() {
        let mut state = DecompilerState::new(1); // Start with 1 input
        assert_eq!(state.total_inputs(), 1);

        // Dup beyond current stack - should discover inputs
        let duped = state.dup_name(2); // Access position 2, but stack only has 1 element
        assert_eq!(duped, "a_2"); // Should be third input (0-indexed)
        assert_eq!(state.total_inputs(), 3);
    }

    #[test]
    fn test_dynamic_input_discovery_swap() {
        let mut state = DecompilerState::new(1); // Start with 1 input
        // Stack: [a_0]
        assert_eq!(state.total_inputs(), 1);

        // Swap beyond current stack - should discover inputs
        state.swap(0, 1);
        assert_eq!(state.total_inputs(), 2);
        assert_eq!(state.peek_name(0), "a_1");
        assert_eq!(state.peek_name(1), "a_0");
    }

    #[test]
    fn test_save_and_restore_state() {
        let mut state = DecompilerState::new(2);
        // Initial stack: [a_1, a_0] (a_0 on top)
        assert_eq!(state.peek_name(0), "a_0");
        assert_eq!(state.peek_name(1), "a_1");

        // Save the state
        let saved = state.save_state();

        // Modify the state by pushing a new variable
        let v = state.new_var();
        state.push_name(v);
        assert_eq!(state.peek_name(0), "v_0");
        assert_eq!(state.stack.len(), 3);

        // Restore the state
        state.restore_state(&saved);
        assert_eq!(state.peek_name(0), "a_0");
        assert_eq!(state.stack.len(), 2);
    }

    #[test]
    fn test_get_rename_map_matching_stacks() {
        let state = DecompilerState::new(2);
        let saved = state.save_state();

        // No changes, should produce empty rename map
        let renames = state.get_rename_map(&saved);
        assert!(renames.is_empty());
    }

    #[test]
    fn test_get_rename_map_different_stacks() {
        let mut state = DecompilerState::new(2);
        let saved = state.save_state();

        // Pop and push new variable at same position
        state.pop_name();
        let v = state.new_var();
        state.push_name(v);

        // Now state has [a_1, v_0] but saved has [a_1, a_0]
        // Position 1 (top) should need renaming: v_0 -> a_0
        let renames = state.get_rename_map(&saved);
        assert_eq!(renames.len(), 1);
        assert_eq!(renames[0], ("v_0".to_string(), "a_0".to_string()));
    }

    #[test]
    fn test_apply_renames() {
        let mut state = DecompilerState::new(0);
        state.push_name("v_0".to_string());
        state.push_name("v_1".to_string());

        let renames = vec![
            ("v_0".to_string(), "a_0".to_string()),
            ("v_1".to_string(), "a_1".to_string()),
        ];
        state.apply_renames(&renames);

        assert_eq!(state.peek_name(0), "a_1");
        assert_eq!(state.peek_name(1), "a_0");
    }

    #[test]
    fn test_counter_generation() {
        let mut state = DecompilerState::new(0);
        assert_eq!(state.new_counter(), "i");
        assert_eq!(state.new_counter(), "j");
        assert_eq!(state.new_counter(), "k");
    }
}
