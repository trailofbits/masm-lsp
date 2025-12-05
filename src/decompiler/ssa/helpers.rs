use super::{
    pseudocode::{PseudocodeBuilder, PseudocodeTemplate},
    state::DecompilerState,
};

/// Generate pseudocode for a binary operation (e.g., add, sub, mul).
pub fn binary_op(state: &mut DecompilerState, op: &str) -> PseudocodeTemplate {
    let b = state.pop();
    let a = state.pop();
    let result = state.new_local();
    state.push(result);

    let mut output = PseudocodeBuilder::new();
    output
        .var(result)
        .text(" = ")
        .var(a)
        .text(" ")
        .text(op)
        .text(" ")
        .var(b);
    output.build()
}

/// Generate pseudocode for a binary operation with an immediate operand.
pub fn binary_imm_op(state: &mut DecompilerState, op: &str, imm: &str) -> PseudocodeTemplate {
    let a = state.pop();
    let result = state.new_local();
    state.push(result);

    let mut output = PseudocodeBuilder::new();
    output
        .var(result)
        .text(" = ")
        .var(a)
        .text(" ")
        .text(op)
        .text(" ")
        .text(imm);
    output.build()
}

/// Generate pseudocode for a comparison operation.
pub fn comparison(state: &mut DecompilerState, op: &str) -> PseudocodeTemplate {
    let b = state.pop();
    let a = state.pop();
    let result = state.new_local();
    state.push(result);

    let mut output = PseudocodeBuilder::new();
    output
        .var(result)
        .text(" = (")
        .var(a)
        .text(" ")
        .text(op)
        .text(" ")
        .var(b)
        .text(")");
    output.build()
}

/// Generate pseudocode for a comparison operation with an immediate operand.
pub fn comparison_imm(state: &mut DecompilerState, op: &str, imm: &str) -> PseudocodeTemplate {
    let a = state.pop();
    let result = state.new_local();
    state.push(result);

    let mut output = PseudocodeBuilder::new();
    output
        .var(result)
        .text(" = (")
        .var(a)
        .text(" ")
        .text(op)
        .text(" ")
        .text(imm)
        .text(")");
    output.build()
}

/// Generate pseudocode for a unary prefix operation (e.g., negation, not).
pub fn unary_op(state: &mut DecompilerState, op: &str) -> PseudocodeTemplate {
    let a = state.pop();
    let result = state.new_local();
    state.push(result);

    let mut output = PseudocodeBuilder::new();
    output.var(result).text(" = ").text(op).var(a);
    output.build()
}

/// Generate pseudocode for a unary function call (e.g., `clz`, `popcnt`).
pub fn unary_fn(state: &mut DecompilerState, fn_name: &str) -> PseudocodeTemplate {
    let a = state.pop();
    let result = state.new_local();
    state.push(result);

    let mut output = PseudocodeBuilder::new();
    output
        .var(result)
        .text(" = ")
        .text(fn_name)
        .text("(")
        .var(a)
        .text(")");
    output.build()
}

/// Generate pseudocode for a dup operation.
pub fn dup(state: &mut DecompilerState, n: usize) -> PseudocodeTemplate {
    let src = state.peek(n).unwrap();
    state.dup(n);
    let result = state.new_local();
    state.pop();
    state.push(result);

    let mut output = PseudocodeBuilder::new();
    output.var(result).text(" = ").var(src);
    output.build()
}

/// Generate pseudocode for an ext2 (quadratic extension) binary operation.
pub fn ext2_binary_op(state: &mut DecompilerState, op: &str) -> PseudocodeTemplate {
    let b1 = state.pop();
    let b0 = state.pop();
    let a1 = state.pop();
    let a0 = state.pop();
    let r0 = state.new_local();
    let r1 = state.new_local();
    state.push(r0);
    state.push(r1);

    let mut output = PseudocodeBuilder::new();
    output
        .text("(")
        .var(r0)
        .text(", ")
        .var(r1)
        .text(") = (")
        .var(a0)
        .text(", ")
        .var(a1)
        .text(") ")
        .text(op)
        .text(" (")
        .var(b0)
        .text(", ")
        .var(b1)
        .text(")");
    output.build()
}

/// Generate pseudocode for an ext2 unary prefix operation.
pub fn ext2_unary_op(state: &mut DecompilerState, op: &str) -> PseudocodeTemplate {
    let a1 = state.pop();
    let a0 = state.pop();
    let r0 = state.new_local();
    let r1 = state.new_local();
    state.push(r0);
    state.push(r1);

    let mut output = PseudocodeBuilder::new();
    output
        .text("(")
        .var(r0)
        .text(", ")
        .var(r1)
        .text(") = ")
        .text(op)
        .text("(")
        .var(a0)
        .text(", ")
        .var(a1)
        .text(")");
    output.build()
}

/// Generate pseudocode for an ext2 unary function call.
pub fn ext2_unary_fn(state: &mut DecompilerState, fn_name: &str) -> PseudocodeTemplate {
    let a1 = state.pop();
    let a0 = state.pop();
    let r0 = state.new_local();
    let r1 = state.new_local();
    state.push(r0);
    state.push(r1);

    let mut output = PseudocodeBuilder::new();
    output
        .text("(")
        .var(r0)
        .text(", ")
        .var(r1)
        .text(") = ")
        .text(fn_name)
        .text("((")
        .var(a0)
        .text(", ")
        .var(a1)
        .text("))");
    output.build()
}
