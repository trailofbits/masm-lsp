//! Disassembler for generating pseudocode from Miden assembly.
//!
//! This module converts Miden assembly instructions into readable pseudocode
//! by tracking a symbolic stack with named variables.

use std::collections::HashMap;

use miden_assembly_syntax::ast::visit::{self, Visit};
use miden_assembly_syntax::ast::{Block, Immediate, Instruction, InvocationTarget, Module, Op, Procedure};
use miden_debug_types::{DefaultSourceManager, SourceSpan, Spanned};
use tower_lsp::lsp_types::{
    Diagnostic, DiagnosticSeverity, InlayHint, InlayHintKind, InlayHintLabel, Position, Range,
};

use crate::analysis::{parse_procedure_signature, ContractStore, StackEffect};
use crate::diagnostics::span_to_range;

// ═══════════════════════════════════════════════════════════════════════════
// Named Value Stack
// ═══════════════════════════════════════════════════════════════════════════

/// A value on the symbolic stack with a name for pseudocode generation.
#[derive(Clone, Debug)]
struct NamedValue {
    /// The variable name (e.g., "a1", "v2")
    name: String,
}

/// State for disassembling a procedure.
///
/// Supports dynamic input discovery: when an operation tries to access a stack
/// position that doesn't exist, a new input variable is automatically created.
#[derive(Debug)]
struct DisassemblerState {
    /// The symbolic stack with named values
    stack: Vec<NamedValue>,
    /// Counter for generating variable names (v1, v2, ...)
    next_var_id: usize,
    /// Counter for dynamically discovered input variables
    /// This tracks how many "virtual" inputs exist below our current stack
    next_input_id: usize,
    /// The initial input count we started with (from contract or signature)
    initial_input_count: usize,
    /// Whether stack tracking has failed (e.g., after dynamic call)
    tracking_failed: bool,
    /// The span where tracking failed (for diagnostic reporting)
    failure_span: Option<SourceSpan>,
    /// The reason tracking failed
    failure_reason: Option<String>,
}

impl DisassemblerState {
    /// Create a new state with procedure inputs on the stack.
    fn new(input_count: usize) -> Self {
        let mut stack = Vec::new();
        // Push inputs in reverse order so a1 is on top after all pushes
        // Actually, inputs are already on stack with a1 at top (position 0)
        // So we push them in order: aN first (bottom), then ... a2, a1 (top)
        for i in (1..=input_count).rev() {
            stack.push(NamedValue {
                name: format!("a{}", i),
            });
        }
        Self {
            stack,
            next_var_id: 1,
            next_input_id: input_count + 1,
            initial_input_count: input_count,
            tracking_failed: false,
            failure_span: None,
            failure_reason: None,
        }
    }

    /// Get the total number of inputs discovered (initial + dynamically discovered).
    fn total_inputs(&self) -> usize {
        self.next_input_id - 1
    }

    /// Generate a new variable name.
    fn new_var(&mut self) -> String {
        let name = format!("v{}", self.next_var_id);
        self.next_var_id += 1;
        name
    }

    /// Generate a new input variable name (for dynamic discovery).
    fn new_input(&mut self) -> String {
        let name = format!("a{}", self.next_input_id);
        self.next_input_id += 1;
        name
    }

    /// Push a new named value onto the stack.
    fn push(&mut self, name: String) {
        self.stack.push(NamedValue { name });
    }

    /// Pop a value from the stack, returning its name.
    /// If the stack is empty, dynamically discovers a new input.
    fn pop(&mut self) -> String {
        if let Some(v) = self.stack.pop() {
            v.name
        } else {
            // Stack underflow - discover a new input!
            // This input was "below" our visible stack when we started
            self.new_input()
        }
    }

    /// Peek at a value on the stack (0 = top).
    /// If the position is beyond the current stack, returns a dynamically generated input name.
    fn peek(&self, n: usize) -> String {
        if n < self.stack.len() {
            self.stack[self.stack.len() - 1 - n].name.clone()
        } else {
            // Position is beyond our stack - this is accessing an input
            // Calculate which input index this would be
            let inputs_below = n - self.stack.len();
            format!("a{}", self.next_input_id + inputs_below)
        }
    }

    /// Duplicate the value at position n onto the top.
    /// If n is beyond the stack, dynamically discovers inputs as needed.
    fn dup(&mut self, n: usize) -> String {
        if n < self.stack.len() {
            self.stack[self.stack.len() - 1 - n].name.clone()
        } else {
            // Need to access beyond current stack - expand inputs
            // Each new input goes at the bottom (lower stack positions)
            // If we have [a1] and need position 2, the original stack would have been:
            // [a3, a2, a1] where a3 is at the bottom (position 2 from top)
            let inputs_needed = n - self.stack.len() + 1;
            // Add inputs at the bottom in reverse order so that
            // higher position numbers get higher argument numbers
            for _ in 0..inputs_needed {
                let input = NamedValue {
                    name: self.new_input(),
                };
                self.stack.insert(0, input);
            }
            // Now we can access it
            self.stack[self.stack.len() - 1 - n].name.clone()
        }
    }

    /// Ensure the stack has at least `needed` elements, expanding with inputs if necessary.
    fn ensure_stack_depth(&mut self, needed: usize) {
        while self.stack.len() < needed {
            // Add a new input at the bottom of the stack
            let input = NamedValue {
                name: self.new_input(),
            };
            self.stack.insert(0, input);
        }
    }

    /// Swap values at positions a and b (0 = top).
    /// Expands the stack with inputs if needed.
    fn swap(&mut self, a: usize, b: usize) {
        let max_pos = a.max(b);
        self.ensure_stack_depth(max_pos + 1);
        let len = self.stack.len();
        let idx_a = len - 1 - a;
        let idx_b = len - 1 - b;
        self.stack.swap(idx_a, idx_b);
    }

    /// Move the element at position n to the top.
    /// Expands the stack with inputs if needed.
    fn movup(&mut self, n: usize) {
        if n == 0 {
            return;
        }
        self.ensure_stack_depth(n + 1);
        let len = self.stack.len();
        let idx = len - 1 - n;
        let elem = self.stack.remove(idx);
        self.stack.push(elem);
    }

    /// Move the top element to position n.
    /// Expands the stack with inputs if needed.
    fn movdn(&mut self, n: usize) {
        if n == 0 {
            return;
        }
        self.ensure_stack_depth(n + 1);
        let elem = self.stack.pop().unwrap();
        let len = self.stack.len();
        let idx = len + 1 - n;
        self.stack.insert(idx, elem);
    }

    /// Swap two words (4-element groups) at word positions a and b.
    fn swapw(&mut self, a: usize, b: usize) {
        // Word positions: word 0 = stack[top-3..top], word 1 = stack[top-7..top-4], etc.
        for i in 0..4 {
            self.swap(a * 4 + i, b * 4 + i);
        }
    }

    /// Move the word at position n to the top (movupw).
    fn movupw(&mut self, word_pos: usize) {
        // Move 4 elements as a group
        for _ in 0..4 {
            self.movup(word_pos * 4);
        }
    }

    /// Move the top word to position n (movdnw).
    fn movdnw(&mut self, word_pos: usize) {
        // Move top 4 elements down as a group
        for _ in 0..4 {
            self.movdn(word_pos * 4);
        }
    }

    /// Mark tracking as failed (e.g., after unknown procedure call).
    fn fail_tracking(&mut self, span: SourceSpan, reason: &str) {
        if !self.tracking_failed {
            // Only record the first failure
            self.tracking_failed = true;
            self.failure_span = Some(span);
            self.failure_reason = Some(reason.to_string());
        }
        self.stack.clear();
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Pseudocode Generation
// ═══════════════════════════════════════════════════════════════════════════

/// Generate pseudocode for an instruction.
///
/// Returns `Some(pseudocode)` if the instruction produces output,
/// or `None` for instructions that just manipulate the stack.
fn generate_pseudocode(
    inst: &Instruction,
    state: &mut DisassemblerState,
    span: SourceSpan,
    contracts: Option<&ContractStore>,
) -> Option<String> {
    if state.tracking_failed {
        return None;
    }

    match inst {
        // ─────────────────────────────────────────────────────────────────────
        // Push operations
        // ─────────────────────────────────────────────────────────────────────
        Instruction::Push(imm) => {
            let value = format_push_immediate(imm);
            let var = state.new_var();
            state.push(var.clone());
            Some(format!("{} = {}", var, value))
        }

        Instruction::PushFeltList(values) => {
            if values.is_empty() {
                None
            } else {
                let mut vars = Vec::new();
                let mut vals = Vec::new();
                for v in values {
                    let var = state.new_var();
                    state.push(var.clone());
                    vars.push(var);
                    vals.push(v.as_int().to_string());
                }
                // Reverse to show in stack order (first pushed = leftmost)
                vars.reverse();
                vals.reverse();
                Some(format!("[{}] = [{}]", vars.join(", "), vals.join(", ")))
            }
        }

        // ─────────────────────────────────────────────────────────────────────
        // Stack manipulation - Dup
        // ─────────────────────────────────────────────────────────────────────
        Instruction::Dup0 => dup_pseudocode(state, 0),
        Instruction::Dup1 => dup_pseudocode(state, 1),
        Instruction::Dup2 => dup_pseudocode(state, 2),
        Instruction::Dup3 => dup_pseudocode(state, 3),
        Instruction::Dup4 => dup_pseudocode(state, 4),
        Instruction::Dup5 => dup_pseudocode(state, 5),
        Instruction::Dup6 => dup_pseudocode(state, 6),
        Instruction::Dup7 => dup_pseudocode(state, 7),
        Instruction::Dup8 => dup_pseudocode(state, 8),
        Instruction::Dup9 => dup_pseudocode(state, 9),
        Instruction::Dup10 => dup_pseudocode(state, 10),
        Instruction::Dup11 => dup_pseudocode(state, 11),
        Instruction::Dup12 => dup_pseudocode(state, 12),
        Instruction::Dup13 => dup_pseudocode(state, 13),
        Instruction::Dup14 => dup_pseudocode(state, 14),
        Instruction::Dup15 => dup_pseudocode(state, 15),

        // Word dup (duplicate 4 elements as a word)
        Instruction::DupW0 => dupw_pseudocode(state, 0),
        Instruction::DupW1 => dupw_pseudocode(state, 1),
        Instruction::DupW2 => dupw_pseudocode(state, 2),
        Instruction::DupW3 => dupw_pseudocode(state, 3),

        // ─────────────────────────────────────────────────────────────────────
        // Stack manipulation - Drop
        // ─────────────────────────────────────────────────────────────────────
        Instruction::Drop => {
            state.pop();
            None
        }
        Instruction::DropW => {
            for _ in 0..4 {
                state.pop();
            }
            None
        }

        // ─────────────────────────────────────────────────────────────────────
        // Stack manipulation - Swap
        // ─────────────────────────────────────────────────────────────────────
        Instruction::Swap1 => { state.swap(0, 1); None }
        Instruction::Swap2 => { state.swap(0, 2); None }
        Instruction::Swap3 => { state.swap(0, 3); None }
        Instruction::Swap4 => { state.swap(0, 4); None }
        Instruction::Swap5 => { state.swap(0, 5); None }
        Instruction::Swap6 => { state.swap(0, 6); None }
        Instruction::Swap7 => { state.swap(0, 7); None }
        Instruction::Swap8 => { state.swap(0, 8); None }
        Instruction::Swap9 => { state.swap(0, 9); None }
        Instruction::Swap10 => { state.swap(0, 10); None }
        Instruction::Swap11 => { state.swap(0, 11); None }
        Instruction::Swap12 => { state.swap(0, 12); None }
        Instruction::Swap13 => { state.swap(0, 13); None }
        Instruction::Swap14 => { state.swap(0, 14); None }
        Instruction::Swap15 => { state.swap(0, 15); None }

        // Word swap (swap 4-element words)
        Instruction::SwapW1 => { state.swapw(0, 1); None }
        Instruction::SwapW2 => { state.swapw(0, 2); None }
        Instruction::SwapW3 => { state.swapw(0, 3); None }
        Instruction::SwapDw => {
            // Swap double words (positions 0-7 with 8-15)
            for i in 0..4 {
                state.swap(i, i + 8);
            }
            for i in 4..8 {
                state.swap(i, i + 4);
            }
            None
        }

        // ─────────────────────────────────────────────────────────────────────
        // Stack manipulation - Move
        // ─────────────────────────────────────────────────────────────────────
        Instruction::MovUp2 => { state.movup(2); None }
        Instruction::MovUp3 => { state.movup(3); None }
        Instruction::MovUp4 => { state.movup(4); None }
        Instruction::MovUp5 => { state.movup(5); None }
        Instruction::MovUp6 => { state.movup(6); None }
        Instruction::MovUp7 => { state.movup(7); None }
        Instruction::MovUp8 => { state.movup(8); None }
        Instruction::MovUp9 => { state.movup(9); None }
        Instruction::MovUp10 => { state.movup(10); None }
        Instruction::MovUp11 => { state.movup(11); None }
        Instruction::MovUp12 => { state.movup(12); None }
        Instruction::MovUp13 => { state.movup(13); None }
        Instruction::MovUp14 => { state.movup(14); None }
        Instruction::MovUp15 => { state.movup(15); None }

        Instruction::MovDn2 => { state.movdn(2); None }
        Instruction::MovDn3 => { state.movdn(3); None }
        Instruction::MovDn4 => { state.movdn(4); None }
        Instruction::MovDn5 => { state.movdn(5); None }
        Instruction::MovDn6 => { state.movdn(6); None }
        Instruction::MovDn7 => { state.movdn(7); None }
        Instruction::MovDn8 => { state.movdn(8); None }
        Instruction::MovDn9 => { state.movdn(9); None }
        Instruction::MovDn10 => { state.movdn(10); None }
        Instruction::MovDn11 => { state.movdn(11); None }
        Instruction::MovDn12 => { state.movdn(12); None }
        Instruction::MovDn13 => { state.movdn(13); None }
        Instruction::MovDn14 => { state.movdn(14); None }
        Instruction::MovDn15 => { state.movdn(15); None }

        // Word move operations
        Instruction::MovUpW2 => { state.movupw(2); None }
        Instruction::MovUpW3 => { state.movupw(3); None }
        Instruction::MovDnW2 => { state.movdnw(2); None }
        Instruction::MovDnW3 => { state.movdnw(3); None }

        // Reverse operations
        Instruction::Reversew => {
            // Reverse top 4 elements
            state.swap(0, 3);
            state.swap(1, 2);
            None
        }
        Instruction::Reversedw => {
            // Reverse top 8 elements
            state.swap(0, 7);
            state.swap(1, 6);
            state.swap(2, 5);
            state.swap(3, 4);
            None
        }

        // ─────────────────────────────────────────────────────────────────────
        // Arithmetic operations
        // ─────────────────────────────────────────────────────────────────────
        Instruction::Add => binary_op_pseudocode(state, "+"),
        Instruction::Sub => binary_op_pseudocode(state, "-"),
        Instruction::Mul => binary_op_pseudocode(state, "*"),
        Instruction::Div => binary_op_pseudocode(state, "/"),

        Instruction::AddImm(imm) => binary_imm_op_pseudocode(state, "+", format_felt_immediate(imm)),
        Instruction::SubImm(imm) => binary_imm_op_pseudocode(state, "-", format_felt_immediate(imm)),
        Instruction::MulImm(imm) => binary_imm_op_pseudocode(state, "*", format_felt_immediate(imm)),
        Instruction::DivImm(imm) => binary_imm_op_pseudocode(state, "/", format_felt_immediate(imm)),

        Instruction::Neg => unary_op_pseudocode(state, "-"),
        Instruction::Inv => {
            let a = state.pop();
            let var = state.new_var();
            state.push(var.clone());
            Some(format!("{} = 1/{}", var, a))
        }
        Instruction::Incr => {
            let a = state.pop();
            let var = state.new_var();
            state.push(var.clone());
            Some(format!("{} = {} + 1", var, a))
        }

        // ─────────────────────────────────────────────────────────────────────
        // Comparison operations
        // ─────────────────────────────────────────────────────────────────────
        Instruction::Eq => binary_op_pseudocode(state, "=="),
        Instruction::Neq => binary_op_pseudocode(state, "!="),
        Instruction::Lt => binary_op_pseudocode(state, "<"),
        Instruction::Lte => binary_op_pseudocode(state, "<="),
        Instruction::Gt => binary_op_pseudocode(state, ">"),
        Instruction::Gte => binary_op_pseudocode(state, ">="),

        Instruction::EqImm(imm) => binary_imm_op_pseudocode(state, "==", format_felt_immediate(imm)),
        Instruction::NeqImm(imm) => binary_imm_op_pseudocode(state, "!=", format_felt_immediate(imm)),

        // ─────────────────────────────────────────────────────────────────────
        // Boolean operations
        // ─────────────────────────────────────────────────────────────────────
        Instruction::And => binary_op_pseudocode(state, "&&"),
        Instruction::Or => binary_op_pseudocode(state, "||"),
        Instruction::Xor => binary_op_pseudocode(state, "^"),
        Instruction::Not => unary_op_pseudocode(state, "!"),

        // ─────────────────────────────────────────────────────────────────────
        // u32 operations
        // ─────────────────────────────────────────────────────────────────────
        Instruction::U32And => binary_op_pseudocode(state, "&"),
        Instruction::U32Or => binary_op_pseudocode(state, "|"),
        Instruction::U32Xor => binary_op_pseudocode(state, "^"),
        Instruction::U32Not => unary_op_pseudocode(state, "~"),

        Instruction::U32WrappingAdd => binary_op_pseudocode(state, "+"),
        Instruction::U32WrappingSub => binary_op_pseudocode(state, "-"),
        Instruction::U32WrappingMul => binary_op_pseudocode(state, "*"),
        Instruction::U32Div => binary_op_pseudocode(state, "/"),
        Instruction::U32Mod => binary_op_pseudocode(state, "%"),

        Instruction::U32WrappingAddImm(imm) => binary_imm_op_pseudocode(state, "+", format_u32_immediate(imm)),
        Instruction::U32WrappingSubImm(imm) => binary_imm_op_pseudocode(state, "-", format_u32_immediate(imm)),
        Instruction::U32WrappingMulImm(imm) => binary_imm_op_pseudocode(state, "*", format_u32_immediate(imm)),
        Instruction::U32DivImm(imm) => binary_imm_op_pseudocode(state, "/", format_u32_immediate(imm)),
        Instruction::U32ModImm(imm) => binary_imm_op_pseudocode(state, "%", format_u32_immediate(imm)),

        Instruction::U32Shl => binary_op_pseudocode(state, "<<"),
        Instruction::U32Shr => binary_op_pseudocode(state, ">>"),
        Instruction::U32ShlImm(imm) => binary_imm_op_pseudocode(state, "<<", format_u8_immediate(imm)),
        Instruction::U32ShrImm(imm) => binary_imm_op_pseudocode(state, ">>", format_u8_immediate(imm)),

        Instruction::U32Rotl => binary_op_pseudocode(state, "rotl"),
        Instruction::U32Rotr => binary_op_pseudocode(state, "rotr"),
        Instruction::U32RotlImm(imm) => binary_imm_op_pseudocode(state, "rotl", format_u8_immediate(imm)),
        Instruction::U32RotrImm(imm) => binary_imm_op_pseudocode(state, "rotr", format_u8_immediate(imm)),

        Instruction::U32Lt => binary_op_pseudocode(state, "<"),
        Instruction::U32Lte => binary_op_pseudocode(state, "<="),
        Instruction::U32Gt => binary_op_pseudocode(state, ">"),
        Instruction::U32Gte => binary_op_pseudocode(state, ">="),
        Instruction::U32Min => binary_op_pseudocode(state, "min"),
        Instruction::U32Max => binary_op_pseudocode(state, "max"),

        // u32 special operations
        Instruction::U32Popcnt => unary_fn_pseudocode(state, "popcnt"),
        Instruction::U32Clz => unary_fn_pseudocode(state, "clz"),
        Instruction::U32Ctz => unary_fn_pseudocode(state, "ctz"),
        Instruction::U32Clo => unary_fn_pseudocode(state, "clo"),
        Instruction::U32Cto => unary_fn_pseudocode(state, "cto"),

        // u32 overflowing operations (produce 2 values)
        Instruction::U32OverflowingAdd => {
            let b = state.pop();
            let a = state.pop();
            let overflow = state.new_var();
            let result = state.new_var();
            state.push(overflow.clone());
            state.push(result.clone());
            Some(format!("{}, {} = {} + {} (overflow)", result, overflow, a, b))
        }
        Instruction::U32OverflowingSub => {
            let b = state.pop();
            let a = state.pop();
            let underflow = state.new_var();
            let result = state.new_var();
            state.push(underflow.clone());
            state.push(result.clone());
            Some(format!("{}, {} = {} - {} (underflow)", result, underflow, a, b))
        }
        Instruction::U32OverflowingMul => {
            let b = state.pop();
            let a = state.pop();
            let overflow = state.new_var();
            let result = state.new_var();
            state.push(overflow.clone());
            state.push(result.clone());
            Some(format!("{}, {} = {} * {} (overflow)", result, overflow, a, b))
        }

        Instruction::U32DivMod => {
            let b = state.pop();
            let a = state.pop();
            let remainder = state.new_var();
            let quotient = state.new_var();
            state.push(remainder.clone());
            state.push(quotient.clone());
            Some(format!("{}, {} = divmod({}, {})", quotient, remainder, a, b))
        }

        Instruction::U32Split => {
            let a = state.pop();
            let lo = state.new_var();
            let hi = state.new_var();
            state.push(lo.clone());
            state.push(hi.clone());
            Some(format!("{}, {} = split({})", hi, lo, a))
        }

        Instruction::U32Cast => unary_fn_pseudocode(state, "u32"),

        // u32 assertions - no output, just validation
        Instruction::U32Assert | Instruction::U32AssertWithError(_) |
        Instruction::U32Assert2 | Instruction::U32Assert2WithError(_) |
        Instruction::U32AssertW | Instruction::U32AssertWWithError(_) => None,

        // u32 test - pushes bool without popping
        Instruction::U32Test | Instruction::U32TestW => {
            let var = state.new_var();
            state.push(var.clone());
            Some(format!("{} = is_u32(top)", var))
        }

        // u32 divmod with immediate
        Instruction::U32DivModImm(imm) => {
            let a = state.pop();
            let divisor = format_u32_immediate(imm);
            let remainder = state.new_var();
            let quotient = state.new_var();
            state.push(remainder.clone());
            state.push(quotient.clone());
            Some(format!("{}, {} = divmod({}, {})", quotient, remainder, a, divisor))
        }

        // u32 overflowing with immediate
        Instruction::U32OverflowingAddImm(imm) => {
            let a = state.pop();
            let b = format_u32_immediate(imm);
            let overflow = state.new_var();
            let result = state.new_var();
            state.push(overflow.clone());
            state.push(result.clone());
            Some(format!("{}, {} = {} + {} (overflow)", result, overflow, a, b))
        }
        Instruction::U32OverflowingSubImm(imm) => {
            let a = state.pop();
            let b = format_u32_immediate(imm);
            let underflow = state.new_var();
            let result = state.new_var();
            state.push(underflow.clone());
            state.push(result.clone());
            Some(format!("{}, {} = {} - {} (underflow)", result, underflow, a, b))
        }
        Instruction::U32OverflowingMulImm(imm) => {
            let a = state.pop();
            let b = format_u32_immediate(imm);
            let overflow = state.new_var();
            let result = state.new_var();
            state.push(overflow.clone());
            state.push(result.clone());
            Some(format!("{}, {} = {} * {} (overflow)", result, overflow, a, b))
        }

        // u32 ternary operations
        Instruction::U32OverflowingAdd3 => {
            let c = state.pop();
            let b = state.pop();
            let a = state.pop();
            let carry = state.new_var();
            let result = state.new_var();
            state.push(carry.clone());
            state.push(result.clone());
            Some(format!("{}, {} = {} + {} + {} (carry)", result, carry, a, b, c))
        }
        Instruction::U32WrappingAdd3 => {
            let c = state.pop();
            let b = state.pop();
            let a = state.pop();
            let result = state.new_var();
            state.push(result.clone());
            Some(format!("{} = {} + {} + {}", result, a, b, c))
        }
        Instruction::U32OverflowingMadd => {
            let c = state.pop();
            let b = state.pop();
            let a = state.pop();
            let overflow = state.new_var();
            let result = state.new_var();
            state.push(overflow.clone());
            state.push(result.clone());
            Some(format!("{}, {} = {} * {} + {} (overflow)", result, overflow, a, b, c))
        }
        Instruction::U32WrappingMadd => {
            let c = state.pop();
            let b = state.pop();
            let a = state.pop();
            let result = state.new_var();
            state.push(result.clone());
            Some(format!("{} = {} * {} + {}", result, a, b, c))
        }

        // ─────────────────────────────────────────────────────────────────────
        // Memory operations
        // ─────────────────────────────────────────────────────────────────────
        Instruction::MemLoad => {
            let addr = state.pop();
            let var = state.new_var();
            state.push(var.clone());
            Some(format!("{} = mem[{}]", var, addr))
        }
        Instruction::MemLoadImm(imm) => {
            let addr = format_u32_immediate(imm);
            let var = state.new_var();
            state.push(var.clone());
            Some(format!("{} = mem[{}]", var, addr))
        }
        Instruction::MemStore => {
            let addr = state.pop();
            let val = state.pop();
            Some(format!("mem[{}] = {}", addr, val))
        }
        Instruction::MemStoreImm(imm) => {
            let addr = format_u32_immediate(imm);
            let val = state.pop();
            Some(format!("mem[{}] = {}", addr, val))
        }

        // Word memory operations
        Instruction::MemLoadWBe | Instruction::MemLoadWLe => {
            let addr = state.pop();
            let mut vars = Vec::new();
            for _ in 0..4 {
                let var = state.new_var();
                state.push(var.clone());
                vars.push(var);
            }
            vars.reverse();
            Some(format!("[{}] = mem_w[{}]", vars.join(", "), addr))
        }
        Instruction::MemLoadWBeImm(imm) | Instruction::MemLoadWLeImm(imm) => {
            let addr = format_u32_immediate(imm);
            let mut vars = Vec::new();
            for _ in 0..4 {
                let var = state.new_var();
                state.push(var.clone());
                vars.push(var);
            }
            vars.reverse();
            Some(format!("[{}] = mem_w[{}]", vars.join(", "), addr))
        }
        Instruction::MemStoreWBe | Instruction::MemStoreWLe => {
            let addr = state.pop();
            let mut vals = Vec::new();
            for _ in 0..4 {
                vals.push(state.pop());
            }
            vals.reverse();
            Some(format!("mem_w[{}] = [{}]", addr, vals.join(", ")))
        }
        Instruction::MemStoreWBeImm(imm) | Instruction::MemStoreWLeImm(imm) => {
            let addr = format_u32_immediate(imm);
            let mut vals = Vec::new();
            for _ in 0..4 {
                vals.push(state.pop());
            }
            vals.reverse();
            Some(format!("mem_w[{}] = [{}]", addr, vals.join(", ")))
        }
        Instruction::MemStream => {
            // Pop 12, push 12 (memory streaming)
            for _ in 0..12 {
                state.pop();
            }
            let mut vars = Vec::new();
            for _ in 0..12 {
                let var = state.new_var();
                state.push(var.clone());
                vars.push(var);
            }
            vars.reverse();
            Some(format!("[{}] = mem_stream()", vars.join(", ")))
        }

        // Local memory - single element
        Instruction::LocLoad(idx) => {
            let idx_val = format_u16_immediate(idx);
            let var = state.new_var();
            state.push(var.clone());
            Some(format!("{} = local[{}]", var, idx_val))
        }
        Instruction::LocStore(idx) => {
            let idx_val = format_u16_immediate(idx);
            let val = state.pop();
            Some(format!("local[{}] = {}", idx_val, val))
        }

        // Local memory - word level (4 elements)
        Instruction::LocLoadWBe(idx) | Instruction::LocLoadWLe(idx) => {
            let idx_val = format_u16_immediate(idx);
            let mut vars = Vec::new();
            for _ in 0..4 {
                let var = state.new_var();
                state.push(var.clone());
                vars.push(var);
            }
            vars.reverse();
            Some(format!("[{}] = local_w[{}]", vars.join(", "), idx_val))
        }
        Instruction::LocStoreWBe(idx) | Instruction::LocStoreWLe(idx) => {
            let idx_val = format_u16_immediate(idx);
            let mut vals = Vec::new();
            for _ in 0..4 {
                vals.push(state.pop());
            }
            vals.reverse();
            Some(format!("local_w[{}] = [{}]", idx_val, vals.join(", ")))
        }
        Instruction::Locaddr(idx) => {
            let idx_val = format_u16_immediate(idx);
            let var = state.new_var();
            state.push(var.clone());
            Some(format!("{} = &local[{}]", var, idx_val))
        }

        // ─────────────────────────────────────────────────────────────────────
        // Advice operations
        // ─────────────────────────────────────────────────────────────────────
        Instruction::AdvPush(n) => {
            let count = match n {
                Immediate::Value(v) => v.into_inner() as usize,
                _ => 1,
            };
            let mut vars = Vec::new();
            for _ in 0..count {
                let var = state.new_var();
                state.push(var.clone());
                vars.push(var);
            }
            vars.reverse();
            Some(format!("[{}] = advice()", vars.join(", ")))
        }
        Instruction::AdvLoadW => {
            // Pop 4 values (address), push 4 values (loaded word)
            for _ in 0..4 {
                state.pop();
            }
            let mut vars = Vec::new();
            for _ in 0..4 {
                let var = state.new_var();
                state.push(var.clone());
                vars.push(var);
            }
            vars.reverse();
            Some(format!("[{}] = advice_w()", vars.join(", ")))
        }
        Instruction::AdvPipe => {
            // Pop 8, push 8 (hasher state + word)
            for _ in 0..8 {
                state.pop();
            }
            let mut vars = Vec::new();
            for _ in 0..8 {
                let var = state.new_var();
                state.push(var.clone());
                vars.push(var);
            }
            vars.reverse();
            Some(format!("[{}] = advice_pipe()", vars.join(", ")))
        }

        // ─────────────────────────────────────────────────────────────────────
        // Cryptographic operations (intrinsics)
        // ─────────────────────────────────────────────────────────────────────
        Instruction::Hash => {
            let mut args = Vec::new();
            for _ in 0..4 {
                args.push(state.pop());
            }
            args.reverse();
            let mut vars = Vec::new();
            for _ in 0..4 {
                let var = state.new_var();
                state.push(var.clone());
                vars.push(var);
            }
            vars.reverse();
            Some(format!("[{}] = hash([{}])", vars.join(", "), args.join(", ")))
        }
        Instruction::HMerge => {
            let mut args = Vec::new();
            for _ in 0..8 {
                args.push(state.pop());
            }
            args.reverse();
            let mut vars = Vec::new();
            for _ in 0..4 {
                let var = state.new_var();
                state.push(var.clone());
                vars.push(var);
            }
            vars.reverse();
            Some(format!("[{}] = hmerge(...)", vars.join(", ")))
        }
        Instruction::HPerm => {
            // Pop 12, push 12
            for _ in 0..12 {
                state.pop();
            }
            for _ in 0..12 {
                let var = state.new_var();
                state.push(var.clone());
            }
            Some("... = hperm(...)".to_string())
        }

        // ─────────────────────────────────────────────────────────────────────
        // Merkle tree operations
        // ─────────────────────────────────────────────────────────────────────
        Instruction::MTreeGet => {
            // Pop depth, index (2), push value word (4) - root word stays
            state.pop(); // depth
            state.pop(); // index
            let mut vars = Vec::new();
            for _ in 0..4 {
                let var = state.new_var();
                state.push(var.clone());
                vars.push(var);
            }
            vars.reverse();
            Some(format!("[{}] = mtree_get()", vars.join(", ")))
        }
        Instruction::MTreeSet => {
            // Pop depth, index (2), push new root word (4) - old root and value stay
            state.pop(); // depth
            state.pop(); // index
            let mut vars = Vec::new();
            for _ in 0..4 {
                let var = state.new_var();
                state.push(var.clone());
                vars.push(var);
            }
            vars.reverse();
            Some(format!("[{}] = mtree_set()", vars.join(", ")))
        }
        Instruction::MTreeMerge => {
            // Pop 8 (two words), push 4 (merged hash)
            for _ in 0..8 {
                state.pop();
            }
            let mut vars = Vec::new();
            for _ in 0..4 {
                let var = state.new_var();
                state.push(var.clone());
                vars.push(var);
            }
            vars.reverse();
            Some(format!("[{}] = mtree_merge()", vars.join(", ")))
        }
        Instruction::MTreeVerify | Instruction::MTreeVerifyWithError(_) => {
            // No stack change, just verification
            Some("mtree_verify()".to_string())
        }

        // ─────────────────────────────────────────────────────────────────────
        // Extension field operations (quadratic extension)
        // ─────────────────────────────────────────────────────────────────────
        Instruction::Ext2Add => ext2_binary_op_pseudocode(state, "+"),
        Instruction::Ext2Sub => ext2_binary_op_pseudocode(state, "-"),
        Instruction::Ext2Mul => ext2_binary_op_pseudocode(state, "*"),
        Instruction::Ext2Div => ext2_binary_op_pseudocode(state, "/"),
        Instruction::Ext2Neg => ext2_unary_op_pseudocode(state, "-"),
        Instruction::Ext2Inv => ext2_unary_fn_pseudocode(state, "inv"),

        // ─────────────────────────────────────────────────────────────────────
        // Assertions
        // ─────────────────────────────────────────────────────────────────────
        Instruction::Assert | Instruction::AssertWithError(_) => {
            let cond = state.pop();
            Some(format!("assert({})", cond))
        }
        Instruction::AssertEq | Instruction::AssertEqWithError(_) => {
            let b = state.pop();
            let a = state.pop();
            Some(format!("assert({} == {})", a, b))
        }
        Instruction::Assertz | Instruction::AssertzWithError(_) => {
            let val = state.pop();
            Some(format!("assert({} == 0)", val))
        }
        Instruction::AssertEqw | Instruction::AssertEqwWithError(_) => {
            // Pop two words (8 elements)
            // Word B is on top (positions 0-3), Word A is below (positions 4-7)
            let b3 = state.pop();
            let b2 = state.pop();
            let b1 = state.pop();
            let b0 = state.pop();
            let a3 = state.pop();
            let a2 = state.pop();
            let a1 = state.pop();
            let a0 = state.pop();
            Some(format!(
                "assert([{}, {}, {}, {}] == [{}, {}, {}, {}])",
                a0, a1, a2, a3, b0, b1, b2, b3
            ))
        }

        // ─────────────────────────────────────────────────────────────────────
        // Procedure calls
        // ─────────────────────────────────────────────────────────────────────
        Instruction::Exec(target) | Instruction::Call(target) | Instruction::SysCall(target) => {
            let name = format_invocation_target(target);

            // Try to look up the stack effect from contracts
            let stack_effect = contracts.and_then(|c| {
                lookup_contract_for_target(c, target).map(|contract| &contract.stack_effect)
            });

            match stack_effect {
                Some(StackEffect::Known { inputs, outputs }) => {
                    // We know the stack effect - apply it
                    let mut args = Vec::new();
                    for _ in 0..*inputs {
                        args.push(state.pop());
                    }
                    args.reverse();

                    let mut results = Vec::new();
                    for _ in 0..*outputs {
                        let var = state.new_var();
                        state.push(var.clone());
                        results.push(var);
                    }
                    results.reverse();

                    if results.is_empty() {
                        Some(format!("{}({})", name, args.join(", ")))
                    } else if results.len() == 1 {
                        Some(format!("{} = {}({})", results[0], name, args.join(", ")))
                    } else {
                        Some(format!("[{}] = {}({})", results.join(", "), name, args.join(", ")))
                    }
                }
                _ => {
                    // Unknown stack effect - fail tracking
                    state.fail_tracking(span, &format!("procedure call to '{}' has unknown stack effect", name));
                    Some(format!("call {}", name))
                }
            }
        }

        Instruction::DynExec | Instruction::DynCall => {
            // Pop target hash, unknown effect
            for _ in 0..4 {
                state.pop();
            }
            state.fail_tracking(span, "dynamic call has unknown stack effect");
            Some("call <dynamic>".to_string())
        }

        // ─────────────────────────────────────────────────────────────────────
        // Conditionals
        // ─────────────────────────────────────────────────────────────────────
        Instruction::CSwap => {
            let cond = state.pop();
            // Conditionally swap top two elements
            Some(format!("cswap({})", cond))
        }
        Instruction::CSwapW => {
            let cond = state.pop();
            // Conditionally swap top two words (8 elements)
            Some(format!("cswapw({})", cond))
        }
        Instruction::CDrop => {
            let cond = state.pop();
            state.pop(); // Drop one of two values
            Some(format!("cdrop({})", cond))
        }
        Instruction::CDropW => {
            let cond = state.pop();
            // Drop one of two words (4 elements)
            for _ in 0..4 {
                state.pop();
            }
            Some(format!("cdropw({})", cond))
        }

        // ─────────────────────────────────────────────────────────────────────
        // Other operations
        // ─────────────────────────────────────────────────────────────────────
        Instruction::Sdepth => {
            let var = state.new_var();
            state.push(var.clone());
            Some(format!("{} = stack_depth()", var))
        }
        Instruction::Clk => {
            let var = state.new_var();
            state.push(var.clone());
            Some(format!("{} = clock()", var))
        }
        Instruction::Caller => {
            let mut vars = Vec::new();
            for _ in 0..4 {
                let var = state.new_var();
                state.push(var.clone());
                vars.push(var);
            }
            vars.reverse();
            Some(format!("[{}] = caller()", vars.join(", ")))
        }

        Instruction::ILog2 => unary_fn_pseudocode(state, "ilog2"),
        Instruction::Pow2 => unary_fn_pseudocode(state, "pow2"),
        Instruction::Exp => binary_op_pseudocode(state, "**"),
        Instruction::ExpImm(imm) => binary_imm_op_pseudocode(state, "**", format_felt_immediate(imm)),
        Instruction::ExpBitLength(imm) => {
            let base = state.pop();
            let var = state.new_var();
            state.push(var.clone());
            Some(format!("{} = {}**<{}-bit exp>", var, base, imm))
        }
        Instruction::IsOdd => unary_fn_pseudocode(state, "is_odd"),

        // Procedure reference
        Instruction::ProcRef(target) => {
            let name = match target {
                InvocationTarget::Symbol(ident) => ident.as_str().to_string(),
                InvocationTarget::Path(path) => path.inner().as_str().to_string(),
                InvocationTarget::MastRoot(root) => format!("{:?}", root),
            };
            let mut vars = Vec::new();
            for _ in 0..4 {
                let var = state.new_var();
                state.push(var.clone());
                vars.push(var);
            }
            vars.reverse();
            Some(format!("[{}] = &{}", vars.join(", "), name))
        }

        // Word equality
        Instruction::Eqw => {
            // Pop two words (8 elements), push bool
            // Word B is on top (positions 0-3), Word A is below (positions 4-7)
            let b3 = state.pop();
            let b2 = state.pop();
            let b1 = state.pop();
            let b0 = state.pop();
            let a3 = state.pop();
            let a2 = state.pop();
            let a1 = state.pop();
            let a0 = state.pop();
            let var = state.new_var();
            state.push(var.clone());
            Some(format!(
                "{} = [{}, {}, {}, {}] == [{}, {}, {}, {}]",
                var, a0, a1, a2, a3, b0, b1, b2, b3
            ))
        }

        // Pad with zeros
        Instruction::PadW => {
            for _ in 0..4 {
                let var = state.new_var();
                state.push(var);
            }
            None // Just stack manipulation
        }

        // Push slice
        Instruction::PushSlice(_, range) => {
            let count = range.len();
            let mut vars = Vec::new();
            for _ in 0..count {
                let var = state.new_var();
                state.push(var.clone());
                vars.push(var);
            }
            vars.reverse();
            Some(format!("[{}] = slice", vars.join(", ")))
        }

        // No-ops
        Instruction::Nop | Instruction::Breakpoint | Instruction::Debug(_) |
        Instruction::Emit | Instruction::EmitImm(_) | Instruction::Trace(_) |
        Instruction::SysEvent(_) => None,

        // Complex STARK operations - fail tracking (unknown effects)
        Instruction::FriExt2Fold4 | Instruction::HornerBase | Instruction::HornerExt |
        Instruction::EvalCircuit | Instruction::LogPrecompile => {
            state.fail_tracking(span, &format!("complex STARK operation: {}", inst));
            Some(format!("{}", inst))
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Helper functions
// ═══════════════════════════════════════════════════════════════════════════

fn dup_pseudocode(state: &mut DisassemblerState, n: usize) -> Option<String> {
    let src_name = state.dup(n);
    let var = state.new_var();
    state.push(var.clone());
    Some(format!("{} = {}", var, src_name))
}

fn dupw_pseudocode(state: &mut DisassemblerState, word_idx: usize) -> Option<String> {
    // DupW duplicates a word (4 elements) at word position word_idx
    // Word 0 = positions 0-3, Word 1 = positions 4-7, etc.
    let base = word_idx * 4;
    let mut vars = Vec::new();
    for i in 0..4 {
        let src = state.dup(base + 3 - i); // Dup in reverse order to maintain stack order
        let var = state.new_var();
        state.push(var.clone());
        vars.push((var, src));
    }
    // No pseudocode output for pure stack manipulation
    None
}

fn binary_op_pseudocode(state: &mut DisassemblerState, op: &str) -> Option<String> {
    let b = state.pop();
    let a = state.pop();
    let var = state.new_var();
    state.push(var.clone());
    Some(format!("{} = {} {} {}", var, a, op, b))
}

fn binary_imm_op_pseudocode(state: &mut DisassemblerState, op: &str, imm: String) -> Option<String> {
    let a = state.pop();
    let var = state.new_var();
    state.push(var.clone());
    Some(format!("{} = {} {} {}", var, a, op, imm))
}

fn unary_op_pseudocode(state: &mut DisassemblerState, op: &str) -> Option<String> {
    let a = state.pop();
    let var = state.new_var();
    state.push(var.clone());
    Some(format!("{} = {}{}", var, op, a))
}

fn unary_fn_pseudocode(state: &mut DisassemblerState, fn_name: &str) -> Option<String> {
    let a = state.pop();
    let var = state.new_var();
    state.push(var.clone());
    Some(format!("{} = {}({})", var, fn_name, a))
}

// Extension field (ext2) operations - work on 2-element values
fn ext2_binary_op_pseudocode(state: &mut DisassemblerState, op: &str) -> Option<String> {
    // Pop 4 elements (2 ext2 values), push 2 (1 ext2 result)
    let b1 = state.pop();
    let b0 = state.pop();
    let a1 = state.pop();
    let a0 = state.pop();
    let var0 = state.new_var();
    let var1 = state.new_var();
    state.push(var0.clone());
    state.push(var1.clone());
    Some(format!("[{}, {}] = [{}, {}] {} [{}, {}]", var0, var1, a0, a1, op, b0, b1))
}

fn ext2_unary_op_pseudocode(state: &mut DisassemblerState, op: &str) -> Option<String> {
    // Pop 2, push 2
    let a1 = state.pop();
    let a0 = state.pop();
    let var0 = state.new_var();
    let var1 = state.new_var();
    state.push(var0.clone());
    state.push(var1.clone());
    Some(format!("[{}, {}] = {}[{}, {}]", var0, var1, op, a0, a1))
}

fn ext2_unary_fn_pseudocode(state: &mut DisassemblerState, fn_name: &str) -> Option<String> {
    // Pop 2, push 2
    let a1 = state.pop();
    let a0 = state.pop();
    let var0 = state.new_var();
    let var1 = state.new_var();
    state.push(var0.clone());
    state.push(var1.clone());
    Some(format!("[{}, {}] = {}([{}, {}])", var0, var1, fn_name, a0, a1))
}

fn format_push_immediate(imm: &Immediate<miden_assembly_syntax::parser::PushValue>) -> String {
    format!("{imm}")
}

fn format_felt_immediate(imm: &Immediate<miden_assembly_syntax::Felt>) -> String {
    format!("{imm}")
}

fn format_u32_immediate(imm: &Immediate<u32>) -> String {
    format!("{imm}")
}

fn format_u16_immediate(imm: &Immediate<u16>) -> String {
    format!("{imm}")
}

fn format_u8_immediate(imm: &Immediate<u8>) -> String {
    format!("{imm}")
}

fn format_invocation_target(target: &InvocationTarget) -> String {
    match target {
        InvocationTarget::Symbol(ident) => ident.as_str().to_string(),
        InvocationTarget::Path(path) => path.inner().as_str().to_string(),
        InvocationTarget::MastRoot(root) => format!("0x{}", root.inner()),
    }
}

/// Format a procedure signature for the declaration hint.
///
/// Examples:
/// - `proc foo()` - no inputs, unknown outputs
/// - `pub proc foo(a1, a2)` - 2 inputs, unknown outputs
/// - `export.bar(a1, a2) -> r1` - 2 inputs, 1 output
/// - `proc foo(a1) -> r1, r2, r3` - 1 input, 3 outputs
fn format_procedure_signature(
    prefix: &str,
    name: &str,
    inputs: usize,
    outputs: Option<usize>,
) -> String {
    // Format input arguments: a1, a2, a3, ...
    let args: Vec<String> = (1..=inputs).map(|i| format!("a{}", i)).collect();
    let args_str = args.join(", ");

    // Build the full signature with prefix
    let full_name = if prefix.is_empty() {
        name.to_string()
    } else {
        format!("{} {}", prefix, name)
    };

    // Format return values: r1, r2, r3, ...
    match outputs {
        Some(0) => format!("{}({})", full_name, args_str),
        Some(n) => {
            let returns: Vec<String> = (1..=n).map(|i| format!("r{}", i)).collect();
            format!("{}({}) -> {}", full_name, args_str, returns.join(", "))
        }
        None => {
            // Unknown outputs - just show inputs
            format!("{}({})", full_name, args_str)
        }
    }
}

/// Extract the declaration prefix (proc, pub proc, export) from source text at a given line.
///
/// Returns the prefix string (e.g., "proc", "pub proc", "export") or empty string if not found.
fn extract_declaration_prefix(source_text: &str, line: u32) -> String {
    let Some(line_text) = source_text.lines().nth(line as usize) else {
        return String::new();
    };

    let trimmed = line_text.trim_start();

    // Check for various declaration prefixes in order of specificity
    if trimmed.starts_with("export.") {
        "export".to_string()
    } else if trimmed.starts_with("pub proc") {
        "pub proc".to_string()
    } else if trimmed.starts_with("proc") {
        "proc".to_string()
    } else {
        String::new()
    }
}

/// Rename a variable in a pseudocode string (whole word replacement).
///
/// This replaces occurrences of `old_name` with `new_name` only when `old_name`
/// appears as a complete identifier (not part of another identifier).
fn rename_variable(text: &str, old_name: &str, new_name: &str) -> String {
    let mut result = String::new();
    let mut chars = text.char_indices().peekable();
    let old_chars: Vec<char> = old_name.chars().collect();

    while let Some((i, c)) = chars.next() {
        // Check if we're at a potential match start
        if text[i..].starts_with(old_name) {
            // Check character before (must not be alphanumeric or underscore)
            let before_ok = if i == 0 {
                true
            } else {
                let prev_char = text[..i].chars().last().unwrap();
                !prev_char.is_alphanumeric() && prev_char != '_'
            };

            // Check character after (must not be alphanumeric or underscore)
            let after_pos = i + old_name.len();
            let after_ok = if after_pos >= text.len() {
                true
            } else {
                let next_char = text[after_pos..].chars().next().unwrap();
                !next_char.is_alphanumeric() && next_char != '_'
            };

            if before_ok && after_ok {
                // This is a whole word match - replace it
                result.push_str(new_name);
                // Skip the old_name characters
                for _ in 1..old_chars.len() {
                    chars.next();
                }
                continue;
            }
        }
        result.push(c);
    }

    result
}

/// Look up a contract for an invocation target.
fn lookup_contract_for_target<'a>(
    contracts: &'a ContractStore,
    target: &InvocationTarget,
) -> Option<&'a crate::analysis::ProcContract> {
    match target {
        InvocationTarget::Symbol(ident) => contracts.get_by_name(ident.as_str()),
        InvocationTarget::Path(path) => contracts.get_by_suffix(path.inner().as_str()),
        InvocationTarget::MastRoot(_) => None, // Can't look up by MAST root
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Hint Collection
// ═══════════════════════════════════════════════════════════════════════════

/// Data for a procedure being disassembled.
#[allow(dead_code)]
struct ProcedureDisassembly {
    /// The procedure being disassembled
    name: String,
    /// Number of inputs to the procedure
    input_count: usize,
}

/// A tracking failure that should be reported as a diagnostic.
struct TrackingFailure {
    span: SourceSpan,
    reason: String,
    proc_name: String,
}

/// Collector that visits procedures and instructions.
struct DisassemblyCollector<'a> {
    /// Current procedure context
    #[allow(dead_code)]
    current_proc: Option<ProcedureDisassembly>,
    /// Current disassembler state
    state: Option<DisassemblerState>,
    /// Collected hints: (line, hint_text)
    hints: Vec<(u32, String)>,
    /// Index of the first hint for the current procedure (for renaming returns)
    proc_hint_start: usize,
    /// Number of outputs for current procedure (for renaming returns)
    proc_outputs: Option<usize>,
    /// Line number of the procedure declaration (for updating signature)
    proc_decl_line: Option<u32>,
    /// Collected tracking failures for diagnostics
    failures: Vec<TrackingFailure>,
    /// Source manager for span conversion
    sources: &'a DefaultSourceManager,
    /// Contract store for looking up procedure stack effects
    contracts: Option<&'a ContractStore>,
    /// Source text for extracting declaration prefixes
    source_text: &'a str,
    /// Current indentation level (number of 4-space indents)
    indent_level: usize,
}

impl<'a> DisassemblyCollector<'a> {
    fn new(
        sources: &'a DefaultSourceManager,
        contracts: Option<&'a ContractStore>,
        source_text: &'a str,
    ) -> Self {
        Self {
            current_proc: None,
            state: None,
            hints: Vec::new(),
            proc_hint_start: 0,
            proc_outputs: None,
            proc_decl_line: None,
            failures: Vec::new(),
            sources,
            contracts,
            source_text,
            indent_level: 0,
        }
    }

    /// Get the current indentation string.
    fn indent(&self) -> String {
        "    ".repeat(self.indent_level)
    }

    /// Visit all operations in a block, handling control flow.
    fn visit_block(&mut self, block: &Block) {
        for op in block.iter() {
            self.visit_op(op);
        }
    }

    /// Visit a single operation, generating hints and handling control flow.
    fn visit_op(&mut self, op: &Op) {
        match op {
            Op::Inst(inst) => {
                if let Some(ref mut state) = self.state {
                    if let Some(pseudocode) = generate_pseudocode(inst.inner(), state, inst.span(), self.contracts) {
                        if let Some(range) = span_to_range(self.sources, inst.span()) {
                            let indented = format!("{}{}", self.indent(), pseudocode);
                            self.hints.push((range.start.line, indented));
                        }
                    }
                }
            }
            Op::If { then_blk, else_blk, .. } => {
                // Get condition variable from top of stack (if.true consumes it)
                let condition = self.state.as_ref()
                    .and_then(|s| s.stack.last())
                    .map(|v| v.name.clone())
                    .unwrap_or_else(|| "?".to_string());

                // Pop the condition from the stack
                if let Some(ref mut state) = self.state {
                    state.stack.pop();
                }

                // Generate hint for "if.true" with condition
                if let Some(range) = span_to_range(self.sources, op.span()) {
                    let hint = format!("{}if {}:", self.indent(), condition);
                    self.hints.push((range.start.line, hint));
                }

                // Visit then block with increased indent
                self.indent_level += 1;
                self.visit_block(then_blk);
                self.indent_level -= 1;

                // Generate hint for "else" if else block is non-empty
                if !else_blk.is_empty() {
                    if let Some(range) = span_to_range(self.sources, else_blk.span()) {
                        // The else keyword is on the line before the else block starts
                        let else_line = range.start.line.saturating_sub(1);
                        let hint = format!("{}else", self.indent());
                        self.hints.push((else_line, hint));
                    }

                    self.indent_level += 1;
                    self.visit_block(else_blk);
                    self.indent_level -= 1;
                }

                // Generate hint for "end"
                if let Some(range) = span_to_range(self.sources, op.span()) {
                    let hint = format!("{}end", self.indent());
                    self.hints.push((range.end.line, hint));
                }
            }
            Op::While { body, .. } => {
                // Get condition variable from top of stack
                // Note: while.true checks condition each iteration, we show the current top
                let condition = self.state.as_ref()
                    .and_then(|s| s.stack.last())
                    .map(|v| v.name.clone())
                    .unwrap_or_else(|| "?".to_string());

                // Generate hint for "while.true" with condition
                if let Some(range) = span_to_range(self.sources, op.span()) {
                    let hint = format!("{}while {}:", self.indent(), condition);
                    self.hints.push((range.start.line, hint));
                }

                // Visit body with increased indent
                self.indent_level += 1;
                self.visit_block(body);
                self.indent_level -= 1;

                // Generate hint for "end"
                if let Some(range) = span_to_range(self.sources, op.span()) {
                    let hint = format!("{}end", self.indent());
                    self.hints.push((range.end.line, hint));
                }
            }
            Op::Repeat { count, body, .. } => {
                // Generate hint for "repeat.N"
                if let Some(range) = span_to_range(self.sources, op.span()) {
                    let hint = format!("{}repeat {} times:", self.indent(), count);
                    self.hints.push((range.start.line, hint));
                }

                // Visit body with increased indent
                self.indent_level += 1;
                self.visit_block(body);
                self.indent_level -= 1;

                // Generate hint for "end"
                if let Some(range) = span_to_range(self.sources, op.span()) {
                    let hint = format!("{}end", self.indent());
                    self.hints.push((range.end.line, hint));
                }
            }
        }
    }
}

impl<'a> Visit for DisassemblyCollector<'a> {
    fn visit_procedure(&mut self, proc: &Procedure) -> core::ops::ControlFlow<()> {
        let proc_name = proc.name().as_str().to_string();
        let decl_range = span_to_range(self.sources, proc.name().span());
        let decl_line = decl_range.as_ref().map(|r| r.start.line);

        // Priority for determining input/output counts:
        // 1. Explicit signature with type annotations (highest priority)
        // 2. Contract store (inferred from instructions)
        // 3. Default to 0 (dynamic discovery will find inputs as needed)

        // Try to parse explicit signature from source text
        let parsed_sig = decl_line.and_then(|line| {
            parse_procedure_signature(self.source_text.lines().nth(line as usize)?)
        });

        // Get contract from store
        let contract_effect = self
            .contracts
            .and_then(|c| c.get_by_name(&proc_name))
            .map(|contract| contract.stack_effect.clone());

        // Determine initial input/output counts
        let (initial_input_count, output_count) = if let Some(sig) = &parsed_sig {
            // Explicit signature has priority
            (sig.inputs, Some(sig.outputs))
        } else {
            // Fall back to contract inference
            match &contract_effect {
                Some(StackEffect::Known { inputs, outputs }) => (*inputs, Some(*outputs)),
                Some(StackEffect::KnownInputs { inputs }) => (*inputs, None),
                _ => (0, None), // Will be discovered dynamically
            }
        };

        // Track where this procedure's hints start (for renaming returns and updating signature)
        self.proc_hint_start = self.hints.len();
        self.proc_outputs = output_count;
        self.proc_decl_line = decl_line;

        // Generate initial signature hint for the procedure declaration
        if let Some(line) = decl_line {
            let decl_prefix = extract_declaration_prefix(self.source_text, line);
            let signature = format_procedure_signature(&decl_prefix, &proc_name, initial_input_count, output_count);
            self.hints.push((line, signature));
        }

        self.current_proc = Some(ProcedureDisassembly {
            name: proc_name.clone(),
            input_count: initial_input_count,
        });
        self.state = Some(DisassemblerState::new(initial_input_count));

        // Visit procedure body with indentation
        self.indent_level = 1;
        self.visit_block(proc.body());
        self.indent_level = 0;

        // After visiting, check if tracking failed and record the failure
        if let Some(ref state) = self.state {
            if state.tracking_failed {
                if let (Some(span), Some(reason)) = (state.failure_span, state.failure_reason.clone()) {
                    self.failures.push(TrackingFailure {
                        span,
                        reason,
                        proc_name: proc_name.clone(),
                    });
                }
            }
        }

        // Check if we discovered more inputs than initially expected
        // and update the signature hint accordingly
        if let Some(ref state) = self.state {
            let discovered_inputs = state.total_inputs();
            if discovered_inputs > initial_input_count {
                // Update the signature hint with the discovered input count
                if let Some(line) = self.proc_decl_line {
                    // Find and update the signature hint for this procedure
                    if let Some(sig_hint) = self.hints.iter_mut().find(|(l, _)| *l == line) {
                        let decl_prefix = extract_declaration_prefix(self.source_text, line);
                        sig_hint.1 = format_procedure_signature(&decl_prefix, &proc_name, discovered_inputs, output_count);
                    }
                }
            }
        }

        // Rename final stack values to r1, r2, ... if we know the output count
        if let (Some(ref state), Some(outputs)) = (&self.state, self.proc_outputs) {
            if !state.tracking_failed && outputs > 0 {
                // Build a map from final stack variable names to return names
                let mut rename_map: Vec<(String, String)> = Vec::new();
                for (i, named_value) in state.stack.iter().rev().take(outputs).enumerate() {
                    let return_name = format!("r{}", i + 1);
                    if named_value.name != return_name {
                        rename_map.push((named_value.name.clone(), return_name));
                    }
                }

                // Apply renames to all hints for this procedure
                for hint in &mut self.hints[self.proc_hint_start..] {
                    for (old_name, new_name) in &rename_map {
                        // Replace as whole word (with word boundaries)
                        hint.1 = rename_variable(&hint.1, old_name, new_name);
                    }
                }
            }
        }

        // Clear state for next procedure
        self.current_proc = None;
        self.state = None;
        self.proc_outputs = None;
        self.proc_decl_line = None;

        core::ops::ControlFlow::Continue(())
    }
}

/// Result of disassembly hint collection.
pub struct DisassemblyResult {
    /// Inlay hints for pseudocode
    pub hints: Vec<InlayHint>,
    /// Diagnostics for tracking failures
    pub diagnostics: Vec<Diagnostic>,
}

/// Collect disassembly hints for all procedures in a module.
pub fn collect_disassembly_hints(
    module: &Module,
    sources: &DefaultSourceManager,
    visible_range: &Range,
    tab_count: usize,
    source_text: &str,
    contracts: Option<&ContractStore>,
) -> DisassemblyResult {
    let mut collector = DisassemblyCollector::new(sources, contracts, source_text);
    let _ = visit::visit_module(&mut collector, module);

    // Fixed padding (in columns) between instruction end and hint.
    let padding = (tab_count.max(1)) as u32;

    // Pre-compute line lengths (trimmed of trailing whitespace) for positioning hints
    let line_lengths: Vec<u32> = source_text
        .lines()
        .map(|line| line.trim_end().len() as u32)
        .collect();

    // Group hints by line (in case multiple instructions on same line)
    let mut line_hints: HashMap<u32, Vec<String>> = HashMap::new();
    for (line, hint) in collector.hints {
        // Filter by visible range
        if line >= visible_range.start.line && line <= visible_range.end.line {
            line_hints.entry(line).or_default().push(hint);
        }
    }

    // Convert to InlayHints
    let mut hints: Vec<InlayHint> = line_hints
        .into_iter()
        .map(|(line_num, pseudocodes)| {
            let line_end = line_lengths.get(line_num as usize).copied().unwrap_or(0);
            let label = pseudocodes.join("; ");

            InlayHint {
                position: Position {
                    line: line_num,
                    character: line_end.saturating_add(padding),
                },
                label: InlayHintLabel::String(label),
                kind: Some(InlayHintKind::TYPE),
                text_edits: None,
                tooltip: None,
                padding_left: None,
                padding_right: None,
                data: None,
            }
        })
        .collect();

    // Sort hints by position
    hints.sort_by(|a, b| {
        a.position
            .line
            .cmp(&b.position.line)
            .then_with(|| a.position.character.cmp(&b.position.character))
    });

    // Convert tracking failures to diagnostics
    let diagnostics: Vec<Diagnostic> = collector
        .failures
        .into_iter()
        .filter_map(|failure| {
            let range = span_to_range(sources, failure.span)?;
            Some(Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::HINT),
                code: None,
                code_description: None,
                source: Some("masm-disassembler".to_string()),
                message: format!(
                    "Pseudocode unavailable beyond this point in '{}': {}",
                    failure.proc_name, failure.reason
                ),
                related_information: None,
                tags: None,
                data: None,
            })
        })
        .collect();

    DisassemblyResult { hints, diagnostics }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_disassembler_state_new() {
        let state = DisassemblerState::new(3);
        assert_eq!(state.stack.len(), 3);
        assert_eq!(state.peek(0), "a1");
        assert_eq!(state.peek(1), "a2");
        assert_eq!(state.peek(2), "a3");
    }

    #[test]
    fn test_disassembler_push_pop() {
        let mut state = DisassemblerState::new(1);
        assert_eq!(state.peek(0), "a1");

        state.push("v1".to_string());
        assert_eq!(state.peek(0), "v1");
        assert_eq!(state.peek(1), "a1");

        assert_eq!(state.pop(), "v1");
        assert_eq!(state.peek(0), "a1");
    }

    #[test]
    fn test_disassembler_dup() {
        let mut state = DisassemblerState::new(2);
        // Stack: [a2, a1] (a1 on top)
        let duped = state.dup(1);
        assert_eq!(duped, "a2");
    }

    #[test]
    fn test_disassembler_swap() {
        let mut state = DisassemblerState::new(2);
        // Stack: [a2, a1] (a1 on top)
        state.swap(0, 1);
        assert_eq!(state.peek(0), "a2");
        assert_eq!(state.peek(1), "a1");
    }

    #[test]
    fn test_binary_op_pseudocode() {
        let mut state = DisassemblerState::new(2);
        // Stack: [a2, a1] (a1 on top)
        let result = binary_op_pseudocode(&mut state, "+");
        assert_eq!(result, Some("v1 = a2 + a1".to_string()));
        assert_eq!(state.peek(0), "v1");
    }

    #[test]
    fn test_dynamic_input_discovery_pop() {
        let mut state = DisassemblerState::new(0); // Start with no inputs
        assert_eq!(state.total_inputs(), 0);

        // Pop from empty stack - should discover inputs
        let a = state.pop();
        assert_eq!(a, "a1");
        assert_eq!(state.total_inputs(), 1);

        let b = state.pop();
        assert_eq!(b, "a2");
        assert_eq!(state.total_inputs(), 2);
    }

    #[test]
    fn test_dynamic_input_discovery_dup() {
        let mut state = DisassemblerState::new(1); // Start with 1 input
        assert_eq!(state.total_inputs(), 1);

        // Dup beyond current stack - should discover inputs
        let duped = state.dup(2); // Access position 2, but stack only has 1 element
        assert_eq!(duped, "a3"); // Should be third input
        assert_eq!(state.total_inputs(), 3);
    }

    #[test]
    fn test_dynamic_input_discovery_swap() {
        let mut state = DisassemblerState::new(1); // Start with 1 input
        // Stack: [a1]
        assert_eq!(state.total_inputs(), 1);

        // Swap beyond current stack - should discover inputs
        state.swap(0, 1);
        assert_eq!(state.total_inputs(), 2);
        assert_eq!(state.peek(0), "a2");
        assert_eq!(state.peek(1), "a1");
    }

    #[test]
    fn test_format_procedure_signature_no_args() {
        assert_eq!(
            format_procedure_signature("proc", "foo", 0, Some(0)),
            "proc foo()"
        );
    }

    #[test]
    fn test_format_procedure_signature_with_args() {
        assert_eq!(
            format_procedure_signature("proc", "bar", 3, None),
            "proc bar(a1, a2, a3)"
        );
    }

    #[test]
    fn test_format_procedure_signature_with_return() {
        assert_eq!(
            format_procedure_signature("pub proc", "baz", 2, Some(1)),
            "pub proc baz(a1, a2) -> r1"
        );
    }

    #[test]
    fn test_format_procedure_signature_multiple_returns() {
        assert_eq!(
            format_procedure_signature("proc", "maj", 3, Some(2)),
            "proc maj(a1, a2, a3) -> r1, r2"
        );
    }

    #[test]
    fn test_format_procedure_signature_no_args_with_return() {
        assert_eq!(
            format_procedure_signature("export", "get_value", 0, Some(1)),
            "export get_value() -> r1"
        );
    }

    #[test]
    fn test_format_procedure_signature_no_prefix() {
        assert_eq!(
            format_procedure_signature("", "internal", 1, Some(1)),
            "internal(a1) -> r1"
        );
    }

    #[test]
    fn test_extract_declaration_prefix_proc() {
        assert_eq!(extract_declaration_prefix("proc foo", 0), "proc");
        assert_eq!(extract_declaration_prefix("  proc bar", 0), "proc");
    }

    #[test]
    fn test_extract_declaration_prefix_pub_proc() {
        assert_eq!(extract_declaration_prefix("pub proc foo", 0), "pub proc");
        assert_eq!(extract_declaration_prefix("  pub proc bar", 0), "pub proc");
    }

    #[test]
    fn test_extract_declaration_prefix_export() {
        assert_eq!(extract_declaration_prefix("export.foo", 0), "export");
    }

    #[test]
    fn test_extract_declaration_prefix_multiline() {
        let source = "begin\n  proc double\n    dup.0\nend";
        assert_eq!(extract_declaration_prefix(source, 1), "proc");
    }

    #[test]
    fn test_rename_variable_simple() {
        assert_eq!(rename_variable("v1 = a1", "v1", "r1"), "r1 = a1");
    }

    #[test]
    fn test_rename_variable_in_expression() {
        assert_eq!(
            rename_variable("v2 = a1 + v1", "v2", "r1"),
            "r1 = a1 + v1"
        );
    }

    #[test]
    fn test_rename_variable_whole_word_only() {
        // Should not rename v1 inside v12
        assert_eq!(rename_variable("v12 = v1", "v1", "r1"), "v12 = r1");
    }

    #[test]
    fn test_rename_variable_multiple_occurrences() {
        assert_eq!(
            rename_variable("v1 = v1 + v1", "v1", "r1"),
            "r1 = r1 + r1"
        );
    }
}
