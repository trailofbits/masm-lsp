//! Decompiler for generating pseudocode from Miden assembly.
//!
//! This module converts Miden assembly instructions into readable pseudocode
//! by tracking a symbolic stack with named variables.
//!
//! # Module Structure
//!
//! - `state`: Decompiler state management (symbolic stack, variable naming)
//! - `pseudocode`: Instruction-to-pseudocode conversion
//! - `collector`: AST visitor that collects hints for all procedures

mod collector;
mod pseudocode;
mod state;

// Re-export public API
pub use collector::{collect_decompilation_hints, DecompilationResult};
pub use pseudocode::{
    apply_counter_indexing, binary_op_pseudocode, extract_declaration_prefix,
    format_invocation_target, format_procedure_signature, rename_variable, ToPseudocode,
};
pub use state::{DecompilerState, FailureRelatedInfo, NamedValue, SavedStackState, LOOP_COUNTER_NAMES};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_loop_variable_consistency_simulation() {
        // Simulate what happens in a while loop:
        // Entry state: [cond, counter, result] where cond is on top
        // After body: [new_cond, new_counter, new_result]
        // We want to rename new_* to the original names

        let mut state = DecompilerState::new(0);
        state.push_name("result".to_string());
        state.push_name("counter".to_string());
        state.push_name("cond".to_string());

        // Save loop entry state (with condition)
        let loop_entry_state = state.save_state();

        // Pop condition (simulating while.true consuming it)
        state.pop_name();

        // Simulate loop body: modify counter and result, push new condition
        state.pop_name(); // pop counter
        state.push_name("v_0".to_string()); // new counter value
        state.pop_name(); // pop old result (now at top)
        state.push_name("v_1".to_string()); // new result
        // Swap to restore order
        use crate::analysis::stack_ops::StackLike;
        state.swap(0, 1);
        // Push new condition
        state.push_name("v_2".to_string());

        // Stack is now: [result=v_1, counter=v_0, cond=v_2]
        // We want to rename to: [result, counter, cond]

        let renames = state.get_rename_map(&loop_entry_state);

        // Should have 3 renames: v_1->result, v_0->counter, v_2->cond
        assert_eq!(renames.len(), 3);

        // Apply renames to state
        state.apply_renames(&renames);

        // Verify stack has consistent names
        assert_eq!(state.peek_name(0), "cond");
        assert_eq!(state.peek_name(1), "counter");
        assert_eq!(state.peek_name(2), "result");
    }

    #[test]
    fn test_if_else_branch_state_isolation() {
        // Simulate if/else where each branch gets the same entry state
        let mut state = DecompilerState::new(2);

        // Pop condition (simulating if.true consuming it)
        state.pop_name();

        // Save entry state (after condition popped)
        let entry_state = state.save_state();
        assert_eq!(state.peek_name(0), "a_1");

        // Simulate then-branch: push a new value
        let v_then = state.new_var();
        state.push_name(v_then);
        assert_eq!(state.peek_name(0), "v_0");

        // Save then-exit state
        let then_exit_state = state.save_state();

        // Restore entry state for else-branch
        state.restore_state(&entry_state);
        assert_eq!(state.peek_name(0), "a_1"); // Should be back to entry state

        // Simulate else-branch: push a new value (gets different name because next_var_id wasn't reset)
        let v_else = state.new_var();
        state.push_name(v_else);
        assert_eq!(state.peek_name(0), "v_1"); // Gets v_1, not v_0

        // Get rename map to make else-branch consistent with then-branch
        let renames = state.get_rename_map(&then_exit_state);
        assert_eq!(renames.len(), 1);
        assert_eq!(renames[0], ("v_1".to_string(), "v_0".to_string()));

        // Apply renames
        state.apply_renames(&renames);
        assert_eq!(state.peek_name(0), "v_0"); // Now consistent with then-branch
    }

    #[test]
    fn test_consuming_loop_output_indexing() {
        // Test the combined input and output indexing for the add loop example:
        // repeat.5
        //     movup.5
        //     add
        //     movdn.4
        // end
        //
        // This loop has net_effect = -1 (consuming), and creates 1 variable per iteration.
        // The pseudocode for "add" should be transformed from:
        //   "v_0 = a_0 + a_5"
        // to:
        //   "v_i = a_i + a_(5+i)"
        //
        // This shows that each iteration produces a distinct output (v_i) from distinct inputs.

        use super::pseudocode::{apply_counter_indexing, apply_output_indexing};

        let original = "v_0 = a_0 + a_5";
        let counter = "i";
        let net_effect = -1;
        let start_var_id = 0;
        let vars_per_iteration = 1;

        // Apply input indexing first
        let with_inputs = apply_counter_indexing(original, counter, net_effect);
        assert_eq!(with_inputs, "v_0 = a_i + a_(5+i)");

        // Then apply output indexing
        let with_outputs = apply_output_indexing(&with_inputs, counter, start_var_id, vars_per_iteration);
        assert_eq!(with_outputs, "v_i = a_i + a_(5+i)");
    }
}
