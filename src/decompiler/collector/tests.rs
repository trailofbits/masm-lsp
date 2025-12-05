use super::types::SsaHint;
use crate::decompiler::ssa::{
    binary_imm_op, binary_op, comparison_imm, unary_op, DecompilerState, PseudocodeTemplate,
    SsaContext,
};

/// Test the SsaHint structure directly.
#[test]
fn test_ssa_hint_resolve() {
    let mut ctx = SsaContext::new();
    let a0 = ctx.new_argument(0);
    let v0 = ctx.new_local();

    let template = PseudocodeTemplate::new()
        .ssa_ref(v0)
        .literal(" = ")
        .ssa_ref(a0)
        .literal(" + 1");

    let hint = SsaHint::new(5, template, 1);
    let resolved = hint.resolve(&ctx);

    assert_eq!(resolved, "    v_0 = a_0 + 1");
}

/// Test the SsaHint with phi resolution.
#[test]
fn test_ssa_hint_with_phi() {
    let mut ctx = SsaContext::new();
    let a0 = ctx.new_argument(0);
    let v0 = ctx.new_local();

    ctx.add_phi(v0, vec![a0]);
    ctx.resolve_names();

    let template = PseudocodeTemplate::new()
        .ssa_ref(v0)
        .literal(" = ")
        .ssa_ref(a0)
        .literal(" + 1");

    let hint = SsaHint::new(5, template, 0);
    let resolved = hint.resolve(&ctx);

    assert_eq!(resolved, "a_0 = a_0 + 1");
}

/// Test instruction_to_template for basic arithmetic.
#[test]
fn test_instruction_to_template_add() {
    let mut state = DecompilerState::new(2);

    let template = binary_op(&mut state, "+");
    let resolved = template.resolve(&state.ctx);

    assert_eq!(resolved, "v_0 = a_1 + a_0");
    assert_eq!(state.depth(), 1);
}

/// Test that loop phi resolution unifies variables.
#[test]
fn test_loop_phi_unification() {
    let mut state = DecompilerState::new(1);

    let entry_stack = state.save_stack();

    state.pop();
    let v0 = state.new_local();
    state.push(v0);

    state.create_loop_phis(&entry_stack).unwrap();
    state.ctx.resolve_names();

    assert_eq!(state.ctx.get_display_name(v0), "a_0");
}

/// Test memcopy_elements procedure to verify while loop condition unification.
#[test]
fn test_memcopy_elements_while_loop_condition() {
    let mut state = DecompilerState::new(3);

    let _neg_template: PseudocodeTemplate = unary_op(&mut state, "-");

    state.dup(0);

    let initial_cond_template: PseudocodeTemplate = comparison_imm(&mut state, "!=", "0");
    let initial_condition = state.peek(0).unwrap();

    let loop_entry_stack = state.save_stack();
    state.pop();

    let _read_ptr = state.peek(1).unwrap();
    let loaded = state.new_local();
    state.push(loaded);

    let _write_ptr = state.peek(3).unwrap();
    let _stored_val = state.pop();

    let _counter_update: PseudocodeTemplate = binary_imm_op(&mut state, "+", "1");

    state.movup(2);
    let _write_ptr_update: PseudocodeTemplate = binary_imm_op(&mut state, "+", "1");

    state.movup(2);
    let _read_ptr_update: PseudocodeTemplate = binary_imm_op(&mut state, "+", "1");

    state.movup(2);
    state.dup(0);

    let end_cond_template: PseudocodeTemplate = comparison_imm(&mut state, "!=", "0");
    let end_condition = state.peek(0).unwrap();

    assert_ne!(initial_condition, end_condition);

    state.create_loop_phis(&loop_entry_stack).unwrap();
    state.ctx.resolve_names();

    let initial_name = state.ctx.get_display_name(initial_condition);
    let end_name = state.ctx.get_display_name(end_condition);
    assert_eq!(initial_name, end_name);
    assert!(initial_name.starts_with("v_"));

    let counter_at_entry = loop_entry_stack[loop_entry_stack.len() - 2];
    let counter_at_end = state.peek(1).unwrap();
    let entry_counter_name = state.ctx.get_display_name(counter_at_entry);
    let end_counter_name = state.ctx.get_display_name(counter_at_end);

    assert_eq!(entry_counter_name, end_counter_name);

    println!("neg template: {}", _neg_template.resolve(&state.ctx));
    println!(
        "initial condition template: {}",
        initial_cond_template.resolve(&state.ctx)
    );
    println!(
        "end condition template: {}",
        end_cond_template.resolve(&state.ctx)
    );
}
