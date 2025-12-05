use super::*;
use miden_assembly_syntax::ast::Visibility;

#[test]
fn test_ssa_context_basic() {
    let mut ctx = SsaContext::new();

    let a0 = ctx.new_argument(0);
    let a1 = ctx.new_argument(1);
    let v0 = ctx.new_local();

    assert_eq!(ctx.get_value(a0).unwrap().display_name, "a_0");
    assert_eq!(ctx.get_value(a1).unwrap().display_name, "a_1");
    assert_eq!(ctx.get_value(v0).unwrap().display_name, "v_0");
}

#[test]
fn test_phi_resolution_simple() {
    let mut ctx = SsaContext::new();

    // Simulate: a_0 is updated in a loop
    // a_0 -> v_0 (updated value) -> phi merges them
    let a0 = ctx.new_argument(0);
    let v0 = ctx.new_local();

    // Create phi: the updated value (v0) should be treated as a_0
    ctx.add_phi(v0, vec![a0, v0]);
    ctx.resolve_names();

    // Both should now display as "a_0"
    assert_eq!(ctx.get_display_name(a0), "a_0");
    assert_eq!(ctx.get_display_name(v0), "a_0");
    assert!(ctx.same_variable(a0, v0));
}

#[test]
fn test_phi_resolution_chain() {
    let mut ctx = SsaContext::new();

    // Simulate multiple updates: a_0 -> v_0 -> v_1 -> ...
    let a0 = ctx.new_argument(0);
    let v0 = ctx.new_local();
    let v1 = ctx.new_local();

    // v0 is first update of a0
    ctx.add_phi(v0, vec![a0]);
    // v1 is update of v0
    ctx.add_phi(v1, vec![v0]);

    ctx.resolve_names();

    // All should display as "a_0"
    assert_eq!(ctx.get_display_name(a0), "a_0");
    assert_eq!(ctx.get_display_name(v0), "a_0");
    assert_eq!(ctx.get_display_name(v1), "a_0");
}

#[test]
fn test_phi_resolution_if_else() {
    let mut ctx = SsaContext::new();

    // Simulate if-else: a_0 might become v_0 or v_1 depending on branch
    let a0 = ctx.new_argument(0);
    let v0 = ctx.new_local(); // then-branch result
    let v1 = ctx.new_local(); // else-branch result
    let v2 = ctx.new_local(); // merge result

    // At merge point, v2 = phi(v0, v1), and both v0, v1 are updates of a0
    ctx.add_phi(v0, vec![a0]);
    ctx.add_phi(v1, vec![a0]);
    ctx.add_phi(v2, vec![v0, v1]);

    ctx.resolve_names();

    // All should display as "a_0"
    assert_eq!(ctx.get_display_name(a0), "a_0");
    assert_eq!(ctx.get_display_name(v0), "a_0");
    assert_eq!(ctx.get_display_name(v1), "a_0");
    assert_eq!(ctx.get_display_name(v2), "a_0");
}

#[test]
fn test_independent_variables() {
    let mut ctx = SsaContext::new();

    // Two independent variables
    let a0 = ctx.new_argument(0);
    let a1 = ctx.new_argument(1);
    let v0 = ctx.new_local(); // derived from a0
    let v1 = ctx.new_local(); // derived from a1

    ctx.add_phi(v0, vec![a0]);
    ctx.add_phi(v1, vec![a1]);

    ctx.resolve_names();

    assert_eq!(ctx.get_display_name(a0), "a_0");
    assert_eq!(ctx.get_display_name(v0), "a_0");
    assert_eq!(ctx.get_display_name(a1), "a_1");
    assert_eq!(ctx.get_display_name(v1), "a_1");

    assert!(ctx.same_variable(a0, v0));
    assert!(ctx.same_variable(a1, v1));
    assert!(!ctx.same_variable(a0, a1));
    assert!(!ctx.same_variable(v0, v1));
}

#[test]
fn test_ssa_stack_basic() {
    let mut ctx = SsaContext::new();
    let a0 = ctx.new_argument(0);
    let a1 = ctx.new_argument(1);

    let stack = SsaStack::with_arguments(&[a0, a1]);

    // a_0 should be on top
    assert_eq!(stack.peek(0), Some(a0));
    assert_eq!(stack.peek(1), Some(a1));
    assert_eq!(stack.depth(), 2);
}

#[test]
fn test_ssa_stack_operations() {
    let mut ctx = SsaContext::new();
    let a0 = ctx.new_argument(0);
    let a1 = ctx.new_argument(1);
    let v0 = ctx.new_local();

    let mut stack = SsaStack::with_arguments(&[a0, a1]);

    // Push v0
    stack.push(v0);
    assert_eq!(stack.peek(0), Some(v0));
    assert_eq!(stack.depth(), 3);

    // Pop v0
    assert_eq!(stack.pop(), Some(v0));
    assert_eq!(stack.depth(), 2);

    // Dup position 1
    stack.dup(1);
    assert_eq!(stack.peek(0), Some(a1));
    assert_eq!(stack.depth(), 3);
}

#[test]
fn test_pseudocode_template() {
    let mut ctx = SsaContext::new();
    let a0 = ctx.new_argument(0);
    let a1 = ctx.new_argument(1);
    let v0 = ctx.new_local();

    // Create template: "v_0 = a_0 + a_1"
    let mut builder = PseudocodeBuilder::new();
    builder.var(v0);
    builder.text(" = ");
    builder.var(a0);
    builder.text(" + ");
    builder.var(a1);
    let template = builder.build();

    let result = template.resolve(&ctx);
    assert_eq!(result, "v_0 = a_0 + a_1");
}

#[test]
fn test_pseudocode_template_with_phi() {
    let mut ctx = SsaContext::new();
    let a0 = ctx.new_argument(0);
    let v0 = ctx.new_local();

    // v0 is an update of a0 (via phi)
    ctx.add_phi(v0, vec![a0]);
    ctx.resolve_names();

    // Create template: "v_0 = a_0 + 1" (but v_0 should resolve to a_0)
    let mut builder = PseudocodeBuilder::new();
    builder.var(v0);
    builder.text(" = ");
    builder.var(a0);
    builder.text(" + 1");
    let template = builder.build();

    let result = template.resolve(&ctx);
    // After phi resolution, v0 displays as a_0
    assert_eq!(result, "a_0 = a_0 + 1");
}

#[test]
fn formats_signature_with_trailing_colon() {
    let signature = format_procedure_signature(
        "",
        "sub",
        Visibility::Private,
        1,
        Some(1),
        None,
        None,
        false,
    );
    assert_eq!(signature, "proc sub(a_0: felt) -> (r_0: felt):");
}

#[test]
fn test_apply_net_effect() {
    let mut state = DecompilerState::new(0);
    assert_eq!(state.depth(), 0);

    // Positive effect pushes new locals
    state.apply_net_effect(3).unwrap();
    assert_eq!(state.depth(), 3);

    // Negative effect pops existing values
    state.apply_net_effect(-2).unwrap();
    assert_eq!(state.depth(), 1);

    // Underflow should error and leave depth unchanged
    assert!(state.apply_net_effect(-5).is_err());
    assert_eq!(state.depth(), 1);
}

#[test]
fn test_ssa_decompiler_state_basic() {
    let mut state = DecompilerState::new(3);

    // Stack should have a_0, a_1, a_2 with a_0 on top
    assert_eq!(state.depth(), 3);

    let top = state.peek(0).unwrap();
    assert_eq!(state.display_name(top), "a_0");

    let second = state.peek(1).unwrap();
    assert_eq!(state.display_name(second), "a_1");
}

#[test]
fn test_ssa_decompiler_state_loop_phis() {
    let mut state = DecompilerState::new(2);

    // Simulate a loop: entry stack is [a_0, a_1]
    let entry_stack = state.save_stack();

    // Loop body: pop a_0, push v_0 (updated value)
    state.pop();
    let v0 = state.new_local();
    state.push(v0);

    // Create phi nodes at loop exit
    state.create_loop_phis(&entry_stack).unwrap();

    // Resolve names
    state.ctx.resolve_names();

    // v_0 should now display as a_0
    let top = state.peek(0).unwrap();
    assert_eq!(state.display_name(top), "a_0");
}

#[test]
fn test_ssa_decompiler_state_if_else_phis() {
    let mut state = DecompilerState::new(1);

    // Pop condition (simulating if.true consuming it)
    let _cond = state.pop();

    // Save entry state
    let entry_stack = state.save_stack();

    // Then branch: push v_0
    let v0 = state.new_local();
    state.push(v0);
    let then_stack = state.save_stack();

    // Restore for else branch
    state.restore_stack(&entry_stack);

    // Else branch: push v_1
    let v1 = state.new_local();
    state.push(v1);
    let else_stack = state.save_stack();

    // Create phi nodes at merge
    state.create_if_else_phis(&then_stack, &else_stack);

    // Resolve names
    state.ctx.resolve_names();

    // v_1 should now display as v_0 (same as then-branch)
    assert_eq!(state.ctx.get_display_name(v1), "v_0");
}

#[test]
fn test_memcopy_elements_simulation() {
    // Simulate the memcopy_elements procedure to verify SSA handling
    let mut state = DecompilerState::new(3); // a_0=n, a_1=read_ptr, a_2=write_ptr

    // neg: v_0 = -a_0
    let _a0 = state.pop();
    let v0 = state.new_local();
    state.push(v0);

    // dup neq.0: v_1 = (v_0 != 0)
    state.dup(0);
    let _dup_v0 = state.pop();
    let v1 = state.new_local();
    state.push(v1);

    // Save loop entry state (with condition on top)
    let loop_entry_stack = state.save_stack();

    // Pop condition for while.true
    state.pop();

    // Loop body simulation (simplified):
    // - read from a_1, write to a_2
    // - update v_0 (counter), a_1 (read_ptr), a_2 (write_ptr)
    // - compute new condition

    // For simplicity, just simulate the updates:
    // Stack after body: [new_cond, new_counter, new_read_ptr, new_write_ptr]

    // Pop old values
    state.pop(); // v_0
    state.pop(); // a_1
    state.pop(); // a_2

    // Push new values (in reverse order to match stack positions)
    let new_write_ptr = state.new_local(); // v_2
    state.push(new_write_ptr);
    let new_read_ptr = state.new_local(); // v_3
    state.push(new_read_ptr);
    let new_counter = state.new_local(); // v_4
    state.push(new_counter);
    let new_cond = state.new_local(); // v_5
    state.push(new_cond);

    // Create phi nodes for loop (comparing with loop_entry_stack which has condition)
    state.create_loop_phis(&loop_entry_stack).unwrap();

    // Resolve names
    state.ctx.resolve_names();

    // The new condition (v_5) should resolve to same name as original condition (v_1)
    // because they're at the same stack position
    assert_eq!(
        state.ctx.get_display_name(new_cond),
        state.ctx.get_display_name(v1)
    );

    // The new counter (v_4) should resolve to same name as original counter (v_0)
    assert_eq!(
        state.ctx.get_display_name(new_counter),
        state.ctx.get_display_name(v0)
    );

    // The new read_ptr (v_3) should resolve to a_1
    let a1_id = SsaId::new(1); // a_1 has ID 1
    assert_eq!(
        state.ctx.get_display_name(new_read_ptr),
        state.ctx.get_display_name(a1_id)
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests for Generic Helper Functions
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn test_generic_binary_op_ssa() {
    let mut state = DecompilerState::new(2);
    let output: PseudocodeTemplate = binary_op(&mut state, "+");

    let result = output.resolve(&state.ctx);
    // Same operand order as string version
    assert_eq!(result, "v_0 = a_1 + a_0");
}

#[test]
fn test_generic_binary_imm_op_ssa() {
    let mut state = DecompilerState::new(1);
    let output: PseudocodeTemplate = binary_imm_op(&mut state, "+", "42");

    let resolved = output.resolve(&state.ctx);
    assert_eq!(resolved, "v_0 = a_0 + 42");
}

#[test]
fn test_generic_binary_op_ssa_with_phi() {
    let mut state = DecompilerState::new(2);

    // Simulate a loop where a_0 is updated
    let entry_stack = state.save_stack();

    // Pop a_0, compute result, push back
    let _a0 = state.pop();
    let _a1 = state.peek(0).unwrap();
    let v0 = state.new_local();
    state.push(v0);

    // Create phi for loop
    state.create_loop_phis(&entry_stack).unwrap();
    state.ctx.resolve_names();

    // v_0 should display as a_0 after phi resolution
    assert_eq!(state.ctx.get_display_name(v0), "a_0");

    // Now use the binary_op helper - it will create v_1
    // Stack: [v_0 (displays as a_0), a_1]
    // Pop order: b=v_0 (a_0), a=a_1
    let output: PseudocodeTemplate = binary_op(&mut state, "+");
    let result = output.resolve(&state.ctx);

    // After phi resolution, v_0 displays as a_0
    // Format: v_1 = a op b = a_1 + a_0
    assert_eq!(result, "v_1 = a_1 + a_0");
}
