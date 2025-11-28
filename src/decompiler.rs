//! Decompiler for generating pseudocode from Miden assembly.
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

/// Saved stack state for control flow merge points.
#[derive(Clone, Debug)]
struct SavedStackState {
    /// The stack contents at the save point
    stack: Vec<NamedValue>,
}

/// State for decompiling a procedure.
///
/// Supports dynamic input discovery: when an operation tries to access a stack
/// position that doesn't exist, a new input variable is automatically created.
#[derive(Debug)]
struct DecompilerState {
    /// The symbolic stack with named values
    stack: Vec<NamedValue>,
    /// Counter for generating variable names (v1, v2, ...)
    next_var_id: usize,
    /// Counter for dynamically discovered input variables
    /// This tracks how many "virtual" inputs exist below our current stack
    next_input_id: usize,
    /// Whether stack tracking has failed (e.g., after dynamic call)
    tracking_failed: bool,
    /// The span where tracking failed (for diagnostic reporting)
    failure_span: Option<SourceSpan>,
    /// The reason tracking failed
    failure_reason: Option<String>,
    /// Counter for generating loop counter names (c1, c2, ...)
    next_counter_id: usize,
    /// Span of a loop that produced a dynamic/unknown number of stack items.
    /// When set, subsequent loops with non-zero net effect cannot be decompiled
    /// because we don't know the exact stack contents.
    dynamic_stack_source: Option<SourceSpan>,
}

/// Loop counter names in order of nesting depth.
const LOOP_COUNTER_NAMES: &[&str] = &["i", "j", "k", "l", "m", "n", "o", "p", "q"];

impl DecompilerState {
    /// Create a new state with procedure inputs on the stack.
    fn new(input_count: usize) -> Self {
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
            next_counter_id: 0,
            dynamic_stack_source: None,
        }
    }

    /// Generate a new loop counter name (i, j, k, ...).
    fn new_counter(&mut self) -> String {
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
    fn total_inputs(&self) -> usize {
        self.next_input_id
    }

    /// Generate a new variable name (v_0, v_1, ...).
    fn new_var(&mut self) -> String {
        let name = format!("v_{}", self.next_var_id);
        self.next_var_id += 1;
        name
    }

    /// Generate a new input variable name (for dynamic discovery).
    fn new_input(&mut self) -> String {
        let name = format!("a_{}", self.next_input_id);
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

    /// Save the current stack state for later restoration or comparison.
    fn save_state(&self) -> SavedStackState {
        SavedStackState {
            stack: self.stack.clone(),
        }
    }

    /// Restore state from a saved snapshot.
    fn restore_state(&mut self, saved: &SavedStackState) {
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
    fn get_rename_map(&self, saved: &SavedStackState) -> Vec<(String, String)> {
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
    fn apply_renames(&mut self, renames: &[(String, String)]) {
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

// ═══════════════════════════════════════════════════════════════════════════
// Pseudocode Generation
// ═══════════════════════════════════════════════════════════════════════════

/// Generate pseudocode for an instruction.
///
/// Returns `Some(pseudocode)` if the instruction produces output,
/// or `None` for instructions that just manipulate the stack.
fn generate_pseudocode(
    inst: &Instruction,
    state: &mut DecompilerState,
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
                Some(format!("({}) = ({})", vars.join(", "), vals.join(", ")))
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
        Instruction::Eq => comparison_pseudocode(state, "=="),
        Instruction::Neq => comparison_pseudocode(state, "!="),
        Instruction::Lt => comparison_pseudocode(state, "<"),
        Instruction::Lte => comparison_pseudocode(state, "<="),
        Instruction::Gt => comparison_pseudocode(state, ">"),
        Instruction::Gte => comparison_pseudocode(state, ">="),

        Instruction::EqImm(imm) => comparison_imm_pseudocode(state, "==", format_felt_immediate(imm)),
        Instruction::NeqImm(imm) => comparison_imm_pseudocode(state, "!=", format_felt_immediate(imm)),

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

        Instruction::U32Lt => comparison_pseudocode(state, "<"),
        Instruction::U32Lte => comparison_pseudocode(state, "<="),
        Instruction::U32Gt => comparison_pseudocode(state, ">"),
        Instruction::U32Gte => comparison_pseudocode(state, ">="),
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
            Some(format!("({}, {}) = {} + {} (overflow)", result, overflow, a, b))
        }
        Instruction::U32OverflowingSub => {
            let b = state.pop();
            let a = state.pop();
            let underflow = state.new_var();
            let result = state.new_var();
            state.push(underflow.clone());
            state.push(result.clone());
            Some(format!("({}, {}) = {} - {} (underflow)", result, underflow, a, b))
        }
        Instruction::U32OverflowingMul => {
            let b = state.pop();
            let a = state.pop();
            let overflow = state.new_var();
            let result = state.new_var();
            state.push(overflow.clone());
            state.push(result.clone());
            Some(format!("({}, {}) = {} * {} (overflow)", result, overflow, a, b))
        }

        Instruction::U32DivMod => {
            let b = state.pop();
            let a = state.pop();
            let remainder = state.new_var();
            let quotient = state.new_var();
            state.push(remainder.clone());
            state.push(quotient.clone());
            Some(format!("({}, {}) = divmod({}, {})", quotient, remainder, a, b))
        }

        Instruction::U32Split => {
            let a = state.pop();
            let lo = state.new_var();
            let hi = state.new_var();
            state.push(lo.clone());
            state.push(hi.clone());
            Some(format!("({}, {}) = split({})", hi, lo, a))
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
            Some(format!("({}, {}) = divmod({}, {})", quotient, remainder, a, divisor))
        }

        // u32 overflowing with immediate
        Instruction::U32OverflowingAddImm(imm) => {
            let a = state.pop();
            let b = format_u32_immediate(imm);
            let overflow = state.new_var();
            let result = state.new_var();
            state.push(overflow.clone());
            state.push(result.clone());
            Some(format!("({}, {}) = {} + {} (overflow)", result, overflow, a, b))
        }
        Instruction::U32OverflowingSubImm(imm) => {
            let a = state.pop();
            let b = format_u32_immediate(imm);
            let underflow = state.new_var();
            let result = state.new_var();
            state.push(underflow.clone());
            state.push(result.clone());
            Some(format!("({}, {}) = {} - {} (underflow)", result, underflow, a, b))
        }
        Instruction::U32OverflowingMulImm(imm) => {
            let a = state.pop();
            let b = format_u32_immediate(imm);
            let overflow = state.new_var();
            let result = state.new_var();
            state.push(overflow.clone());
            state.push(result.clone());
            Some(format!("({}, {}) = {} * {} (overflow)", result, overflow, a, b))
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
            Some(format!("({}, {}) = {} + {} + {} (carry)", result, carry, a, b, c))
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
            Some(format!("({}, {}) = {} * {} + {} (overflow)", result, overflow, a, b, c))
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
            Some(format!("({}) = mem_w[{}]", vars.join(", "), addr))
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
            Some(format!("({}) = mem_w[{}]", vars.join(", "), addr))
        }
        Instruction::MemStoreWBe | Instruction::MemStoreWLe => {
            let addr = state.pop();
            let mut vals = Vec::new();
            for _ in 0..4 {
                vals.push(state.pop());
            }
            vals.reverse();
            Some(format!("mem_w[{}] = ({})", addr, vals.join(", ")))
        }
        Instruction::MemStoreWBeImm(imm) | Instruction::MemStoreWLeImm(imm) => {
            let addr = format_u32_immediate(imm);
            let mut vals = Vec::new();
            for _ in 0..4 {
                vals.push(state.pop());
            }
            vals.reverse();
            Some(format!("mem_w[{}] = ({})", addr, vals.join(", ")))
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
            Some(format!("({}) = mem_stream()", vars.join(", ")))
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
            Some(format!("({}) = local_w[{}]", vars.join(", "), idx_val))
        }
        Instruction::LocStoreWBe(idx) | Instruction::LocStoreWLe(idx) => {
            let idx_val = format_u16_immediate(idx);
            let mut vals = Vec::new();
            for _ in 0..4 {
                vals.push(state.pop());
            }
            vals.reverse();
            Some(format!("local_w[{}] = ({})", idx_val, vals.join(", ")))
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
            Some(format!("({}) = advice()", vars.join(", ")))
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
            Some(format!("({}) = advice_w()", vars.join(", ")))
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
            Some(format!("({}) = advice_pipe()", vars.join(", ")))
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
            Some(format!("({}) = hash(({}))", vars.join(", "), args.join(", ")))
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
            Some(format!("({}) = hmerge(...)", vars.join(", ")))
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
            Some(format!("({}) = mtree_get()", vars.join(", ")))
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
            Some(format!("({}) = mtree_set()", vars.join(", ")))
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
            Some(format!("({}) = mtree_merge()", vars.join(", ")))
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
                "assert(({}, {}, {}, {}) == ({}, {}, {}, {}))",
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
                        Some(format!("({}) = {}({})", results.join(", "), name, args.join(", ")))
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
            Some(format!("({}) = caller()", vars.join(", ")))
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
            Some(format!("({}) = &{}", vars.join(", "), name))
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
                "{} = ({}, {}, {}, {}) == ({}, {}, {}, {})",
                var, a0, a1, a2, a3, b0, b1, b2, b3
            ))
        }

        // Pad with zeros
        Instruction::PadW => {
            let mut vars = Vec::new();
            for _ in 0..4 {
                let var = state.new_var();
                state.push(var.clone());
                vars.push(var);
            }
            vars.reverse();
            Some(format!("({}) = (0, 0, 0, 0)", vars.join(", ")))
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
            Some(format!("({}) = slice", vars.join(", ")))
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

fn dup_pseudocode(state: &mut DecompilerState, n: usize) -> Option<String> {
    let src_name = state.dup(n);
    let var = state.new_var();
    state.push(var.clone());
    Some(format!("{} = {}", var, src_name))
}

fn dupw_pseudocode(state: &mut DecompilerState, word_idx: usize) -> Option<String> {
    // DupW duplicates a word (4 elements) at word position word_idx
    // Word 0 = positions 0-3, Word 1 = positions 4-7, etc.
    let base = word_idx * 4;

    // Collect the source values before duplicating
    let sources: Vec<String> = (0..4).map(|i| state.peek(base + i)).collect();

    // Perform the duplication
    let mut new_vars = Vec::new();
    for i in 0..4 {
        let _ = state.dup(base + 3 - i); // Dup in reverse order to maintain stack order
        let var = state.new_var();
        state.push(var.clone());
        new_vars.push(var);
    }
    new_vars.reverse();

    Some(format!(
        "({}) = ({})",
        new_vars.join(", "),
        sources.join(", ")
    ))
}

fn binary_op_pseudocode(state: &mut DecompilerState, op: &str) -> Option<String> {
    let b = state.pop();
    let a = state.pop();
    let var = state.new_var();
    state.push(var.clone());
    Some(format!("{} = {} {} {}", var, a, op, b))
}

fn binary_imm_op_pseudocode(state: &mut DecompilerState, op: &str, imm: String) -> Option<String> {
    let a = state.pop();
    let var = state.new_var();
    state.push(var.clone());
    Some(format!("{} = {} {} {}", var, a, op, imm))
}

/// Like binary_op_pseudocode but wraps the expression in parentheses for boolean comparisons.
fn comparison_pseudocode(state: &mut DecompilerState, op: &str) -> Option<String> {
    let b = state.pop();
    let a = state.pop();
    let var = state.new_var();
    state.push(var.clone());
    Some(format!("{} = ({} {} {})", var, a, op, b))
}

/// Like binary_imm_op_pseudocode but wraps the expression in parentheses for boolean comparisons.
fn comparison_imm_pseudocode(state: &mut DecompilerState, op: &str, imm: String) -> Option<String> {
    let a = state.pop();
    let var = state.new_var();
    state.push(var.clone());
    Some(format!("{} = ({} {} {})", var, a, op, imm))
}

fn unary_op_pseudocode(state: &mut DecompilerState, op: &str) -> Option<String> {
    let a = state.pop();
    let var = state.new_var();
    state.push(var.clone());
    Some(format!("{} = {}{}", var, op, a))
}

fn unary_fn_pseudocode(state: &mut DecompilerState, fn_name: &str) -> Option<String> {
    let a = state.pop();
    let var = state.new_var();
    state.push(var.clone());
    Some(format!("{} = {}({})", var, fn_name, a))
}

// Extension field (ext2) operations - work on 2-element values
fn ext2_binary_op_pseudocode(state: &mut DecompilerState, op: &str) -> Option<String> {
    // Pop 4 elements (2 ext2 values), push 2 (1 ext2 result)
    let b1 = state.pop();
    let b0 = state.pop();
    let a1 = state.pop();
    let a0 = state.pop();
    let var0 = state.new_var();
    let var1 = state.new_var();
    state.push(var0.clone());
    state.push(var1.clone());
    Some(format!("({}, {}) = ({}, {}) {} ({}, {})", var0, var1, a0, a1, op, b0, b1))
}

fn ext2_unary_op_pseudocode(state: &mut DecompilerState, op: &str) -> Option<String> {
    // Pop 2, push 2
    let a1 = state.pop();
    let a0 = state.pop();
    let var0 = state.new_var();
    let var1 = state.new_var();
    state.push(var0.clone());
    state.push(var1.clone());
    Some(format!("({}, {}) = {}({}, {})", var0, var1, op, a0, a1))
}

fn ext2_unary_fn_pseudocode(state: &mut DecompilerState, fn_name: &str) -> Option<String> {
    // Pop 2, push 2
    let a1 = state.pop();
    let a0 = state.pop();
    let var0 = state.new_var();
    let var1 = state.new_var();
    state.push(var0.clone());
    state.push(var1.clone());
    Some(format!("({}, {}) = {}(({}, {}))", var0, var1, fn_name, a0, a1))
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
/// - `pub proc foo(a_0, a_1)` - 2 inputs, unknown outputs
/// - `export.bar(a_0, a_1) -> r_0` - 2 inputs, 1 output
/// - `proc foo(a_0) -> r_0, r_1, r_2` - 1 input, 3 outputs
fn format_procedure_signature(
    prefix: &str,
    name: &str,
    inputs: usize,
    outputs: Option<usize>,
) -> String {
    // Format input arguments: a_0, a_1, a_2, ... (0-indexed)
    let args: Vec<String> = (0..inputs).map(|i| format!("a_{}", i)).collect();
    let args_str = args.join(", ");

    // Build the full signature with prefix
    let full_name = if prefix.is_empty() {
        name.to_string()
    } else {
        format!("{} {}", prefix, name)
    };

    // Format return values: r_0, r_1, r_2, ... (0-indexed)
    match outputs {
        Some(0) => format!("{}({})", full_name, args_str),
        Some(n) => {
            let returns: Vec<String> = (0..n).map(|i| format!("r_{}", i)).collect();
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

/// Apply counter indexing to input variables in a hint string.
///
/// For loops with non-zero net stack effect, input variable references like `a_0`, `a_1`
/// need to be converted to indexed form like `a_(0+i)`, `a_(1+i)` to show that
/// different iterations access different stack positions.
///
/// `net_effect` is the per-iteration change: negative means consuming, positive means producing.
fn apply_counter_indexing(text: &str, counter: &str, net_effect: i32) -> String {
    let mut result = String::new();
    let chars: Vec<char> = text.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        // Look for 'a_' followed by digits (input variable pattern: a_0, a_1, etc.)
        if chars[i] == 'a' && i + 2 < chars.len() && chars[i + 1] == '_' && chars[i + 2].is_ascii_digit() {
            // Check that this is a word boundary (not part of a larger identifier)
            let before_ok = i == 0 || (!chars[i - 1].is_alphanumeric() && chars[i - 1] != '_');

            if before_ok {
                // Parse the number following 'a_'
                let num_start = i + 2;
                let mut num_end = num_start;
                while num_end < chars.len() && chars[num_end].is_ascii_digit() {
                    num_end += 1;
                }

                // Check word boundary after the number (must not be followed by alphanumeric)
                let after_ok = num_end >= chars.len()
                    || (!chars[num_end].is_alphanumeric());

                if after_ok {
                    let num_str: String = chars[num_start..num_end].iter().collect();
                    if let Ok(num) = num_str.parse::<i32>() {
                        // Generate indexed form using a_(...) notation
                        // Simplify when base is 0: a_(0+i) -> a_i
                        let indexed = if net_effect < 0 {
                            // Consuming loop: positions shift forward each iteration
                            let effect_abs = -net_effect;
                            if num == 0 {
                                // Simplify: a_(0+i) -> a_i, a_(0+i*2) -> a_(i*2)
                                if effect_abs == 1 {
                                    format!("a_{}", counter)
                                } else {
                                    format!("a_({}*{})", counter, effect_abs)
                                }
                            } else if effect_abs == 1 {
                                format!("a_({}+{})", num, counter)
                            } else {
                                format!("a_({}+{}*{})", num, counter, effect_abs)
                            }
                        } else {
                            // Producing loop: positions shift backward each iteration
                            if num == 0 {
                                // Simplify: a_(0-i) -> a_(-i)
                                if net_effect == 1 {
                                    format!("a_(-{})", counter)
                                } else {
                                    format!("a_(-{}*{})", counter, net_effect)
                                }
                            } else if net_effect == 1 {
                                format!("a_({}-{})", num, counter)
                            } else {
                                format!("a_({}-{}*{})", num, counter, net_effect)
                            }
                        };
                        result.push_str(&indexed);
                        i = num_end;
                        continue;
                    }
                }
            }
        }
        result.push(chars[i]);
        i += 1;
    }

    result
}

// ═══════════════════════════════════════════════════════════════════════════
// Hint Collection
// ═══════════════════════════════════════════════════════════════════════════

/// Data for a procedure being decompiled.
#[allow(dead_code)]
struct ProcedureDecompilation {
    /// The procedure being decompiled
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
struct DecompilationCollector<'a> {
    /// Current procedure context
    #[allow(dead_code)]
    current_proc: Option<ProcedureDecompilation>,
    /// Current decompiler state
    state: Option<DecompilerState>,
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

impl<'a> DecompilationCollector<'a> {
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

                // Pop the condition from the stack and save state for else branch
                let entry_state = if let Some(ref mut state) = self.state {
                    state.stack.pop();
                    Some(state.save_state())
                } else {
                    None
                };

                // Generate hint for "if.true" with condition
                if let Some(range) = span_to_range(self.sources, op.span()) {
                    let hint = format!("{}if {}:", self.indent(), condition);
                    self.hints.push((range.start.line, hint));
                }

                // Track where then-branch hints start for potential renaming
                let _then_hint_start = self.hints.len();

                // Visit then block with increased indent
                self.indent_level += 1;
                self.visit_block(then_blk);
                self.indent_level -= 1;

                // Save then-branch exit state for merge
                let then_exit_state = self.state.as_ref().map(|s| s.save_state());

                // Generate hint for "else" if else block is non-empty
                if !else_blk.is_empty() {
                    if let Some(range) = span_to_range(self.sources, else_blk.span()) {
                        // The else keyword is on the line before the else block starts
                        let else_line = range.start.line.saturating_sub(1);
                        let hint = format!("{}else", self.indent());
                        self.hints.push((else_line, hint));
                    }

                    // Restore entry state for else branch (crucial fix!)
                    if let (Some(ref mut state), Some(ref saved)) = (&mut self.state, &entry_state) {
                        state.restore_state(saved);
                    }

                    // Track where else-branch hints start for potential renaming
                    let else_hint_start = self.hints.len();

                    self.indent_level += 1;
                    self.visit_block(else_blk);
                    self.indent_level -= 1;

                    // At merge point: rename else-branch variables to match then-branch positions
                    // This ensures consistent variable names after the if/else
                    if let (Some(ref mut state), Some(ref then_state)) = (&mut self.state, &then_exit_state) {
                        let renames = state.get_rename_map(then_state);
                        if !renames.is_empty() {
                            // Apply renames to else-branch hints
                            for hint in &mut self.hints[else_hint_start..] {
                                for (from, to) in &renames {
                                    hint.1 = rename_variable(&hint.1, from, to);
                                }
                            }
                            // Update state to use the then-branch variable names
                            state.apply_renames(&renames);
                        }
                    }
                } else {
                    // No else branch - restore then-branch exit state as the merge state
                    // (it's already there, but if we had modified state we'd need this)
                }

                // Generate hint for "end"
                if let Some(range) = span_to_range(self.sources, op.span()) {
                    let hint = format!("{}end", self.indent());
                    self.hints.push((range.end.line, hint));
                }
            }
            Op::While { body, .. } => {
                // Get condition variable from top of stack
                // while.true checks and CONSUMES the condition each iteration
                let condition = self.state.as_ref()
                    .and_then(|s| s.stack.last())
                    .map(|v| v.name.clone())
                    .unwrap_or_else(|| "?".to_string());

                // Save the loop entry state (with condition on top)
                // This represents the expected stack shape at each iteration start
                let loop_entry_state = self.state.as_ref().map(|s| s.save_state());

                // Pop the condition from the stack (while.true consumes it)
                // and record entry depth (after condition pop) for net effect calculation
                let entry_depth = if let Some(ref mut state) = self.state {
                    state.stack.pop();
                    state.stack.len()
                } else {
                    0
                };

                // Track where while loop hint will be (index for potential modification)
                let while_hint_idx = self.hints.len();

                // Generate hint for "while.true" with condition (may be updated later with counter init)
                if let Some(range) = span_to_range(self.sources, op.span()) {
                    let hint = format!("{}while {}:", self.indent(), condition);
                    self.hints.push((range.start.line, hint));
                }

                // Track where loop body hints start for potential modification
                let loop_body_hint_start = self.hints.len();

                // Visit body with increased indent
                self.indent_level += 1;
                self.visit_block(body);
                self.indent_level -= 1;

                // Calculate net stack effect per iteration
                // Note: at loop end, there's a new condition on top, so we compare to entry_depth + 1
                let exit_depth = self.state.as_ref().map(|s| s.stack.len()).unwrap_or(0);
                let net_effect = exit_depth as i32 - (entry_depth as i32 + 1); // +1 for the new condition

                if net_effect != 0 {
                    // Check if a previous loop produced dynamic stack content
                    // If so, we can't reliably decompile this loop
                    if let Some(ref state) = self.state {
                        if state.dynamic_stack_source.is_some() {
                            // Fail decompilation - we have dynamic stack content from a previous loop
                            if let Some(ref mut state) = self.state {
                                state.fail_tracking(
                                    op.span(),
                                    "loop with variable iteration count preceded by another loop that modified stack size"
                                );
                            }
                            // Remove hints generated for this loop
                            self.hints.truncate(while_hint_idx);
                            return;
                        }
                    }

                    // Non-zero net effect: stack positions shift each iteration
                    // Generate a counter and apply indexing
                    let counter = self.state.as_mut().map(|s| s.new_counter()).unwrap_or_else(|| "i".to_string());

                    // Update the while hint to include counter initialization
                    // Format: "while i = 0; condition:" instead of separate "i = 0" line
                    if while_hint_idx < self.hints.len() {
                        self.hints[while_hint_idx].1 = format!("{}while {} = 0; {}:", self.indent(), counter, condition);
                    }

                    // Apply counter indexing to input variable references in loop body
                    for hint in &mut self.hints[loop_body_hint_start..] {
                        hint.1 = apply_counter_indexing(&hint.1, &counter, net_effect);
                    }

                    // Mark stack as dynamic for subsequent loops
                    if let Some(ref mut state) = self.state {
                        state.dynamic_stack_source = Some(op.span());
                    }
                } else {
                    // Zero net effect: stack shape is preserved, use renaming for consistency
                    if let (Some(ref mut state), Some(ref entry_state)) = (&mut self.state, &loop_entry_state) {
                        let renames = state.get_rename_map(entry_state);
                        if !renames.is_empty() {
                            // Apply renames to all hints generated in the loop body
                            for hint in &mut self.hints[loop_body_hint_start..] {
                                for (from, to) in &renames {
                                    hint.1 = rename_variable(&hint.1, from, to);
                                }
                            }
                            // Update state to use the entry variable names
                            state.apply_renames(&renames);
                        }
                    }
                }

                // Generate hint for "end"
                if let Some(range) = span_to_range(self.sources, op.span()) {
                    let hint = format!("{}end", self.indent());
                    self.hints.push((range.end.line, hint));
                }
            }
            Op::Repeat { count, body, .. } => {
                // Track where repeat loop hint will be (index for potential removal on failure)
                let repeat_hint_idx = self.hints.len();

                // Generate a loop counter for this repeat loop
                let counter = self.state.as_mut().map(|s| s.new_counter()).unwrap_or_else(|| "i".to_string());

                // Save the loop entry state for calculating net effect
                let loop_entry_state = self.state.as_ref().map(|s| s.save_state());
                let entry_depth = self.state.as_ref().map(|s| s.stack.len()).unwrap_or(0);

                // Generate hint for "for c in 0..N:" (replaces "repeat N times:")
                if let Some(range) = span_to_range(self.sources, op.span()) {
                    let hint = format!("{}for {} in 0..{}:", self.indent(), counter, count);
                    self.hints.push((range.start.line, hint));
                }

                // Track where loop body hints start for potential modification
                let loop_body_hint_start = self.hints.len();

                // Visit body with increased indent
                self.indent_level += 1;
                self.visit_block(body);
                self.indent_level -= 1;

                // Calculate net stack effect per iteration
                let exit_depth = self.state.as_ref().map(|s| s.stack.len()).unwrap_or(0);
                let net_effect = exit_depth as i32 - entry_depth as i32;

                if net_effect != 0 {
                    // Check if a previous loop produced dynamic stack content
                    // If so, we can't reliably decompile this loop
                    if let Some(ref state) = self.state {
                        if state.dynamic_stack_source.is_some() {
                            // Fail decompilation - we have dynamic stack content from a previous loop
                            if let Some(ref mut state) = self.state {
                                state.fail_tracking(
                                    op.span(),
                                    "loop with non-zero stack effect preceded by another loop that modified stack size"
                                );
                            }
                            // Remove hints generated for this loop
                            self.hints.truncate(repeat_hint_idx);
                            return;
                        }
                    }

                    // Non-zero net effect: stack positions shift each iteration
                    // Apply counter indexing to input variable references
                    for hint in &mut self.hints[loop_body_hint_start..] {
                        hint.1 = apply_counter_indexing(&hint.1, &counter, net_effect);
                    }

                    // Mark stack as dynamic for subsequent loops
                    if let Some(ref mut state) = self.state {
                        state.dynamic_stack_source = Some(op.span());
                    }
                } else {
                    // Zero net effect: stack shape is preserved, use renaming for consistency
                    if let (Some(ref mut state), Some(ref entry_state)) = (&mut self.state, &loop_entry_state) {
                        let renames = state.get_rename_map(entry_state);
                        if !renames.is_empty() {
                            // Apply renames to all hints generated in the loop body
                            for hint in &mut self.hints[loop_body_hint_start..] {
                                for (from, to) in &renames {
                                    hint.1 = rename_variable(&hint.1, from, to);
                                }
                            }
                            // Update state to use the entry variable names
                            state.apply_renames(&renames);
                        }
                    }
                }

                // Generate hint for "end"
                if let Some(range) = span_to_range(self.sources, op.span()) {
                    let hint = format!("{}end", self.indent());
                    self.hints.push((range.end.line, hint));
                }
            }
        }
    }
}

impl<'a> Visit for DecompilationCollector<'a> {
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

        self.current_proc = Some(ProcedureDecompilation {
            name: proc_name.clone(),
            input_count: initial_input_count,
        });
        self.state = Some(DecompilerState::new(initial_input_count));

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
                // Remove all hints for this procedure when decompilation fails
                self.hints.truncate(self.proc_hint_start);
                // Clear state and return early - skip signature updates and return renaming
                self.current_proc = None;
                self.state = None;
                self.proc_outputs = None;
                self.proc_decl_line = None;
                return core::ops::ControlFlow::Continue(());
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

        // Rename final stack values to r_0, r_1, ... if we know the output count
        if let (Some(ref state), Some(outputs)) = (&self.state, self.proc_outputs) {
            if !state.tracking_failed && outputs > 0 {
                // Build a map from final stack variable names to return names (0-indexed)
                let mut rename_map: Vec<(String, String)> = Vec::new();
                for (i, named_value) in state.stack.iter().rev().take(outputs).enumerate() {
                    let return_name = format!("r_{}", i);
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

/// Result of decompilation hint collection.
pub struct DecompilationResult {
    /// Inlay hints for pseudocode
    pub hints: Vec<InlayHint>,
    /// Diagnostics for tracking failures
    pub diagnostics: Vec<Diagnostic>,
}

/// Collect decompilation hints for all procedures in a module.
pub fn collect_decompilation_hints(
    module: &Module,
    sources: &DefaultSourceManager,
    visible_range: &Range,
    tab_count: usize,
    source_text: &str,
    contracts: Option<&ContractStore>,
) -> DecompilationResult {
    let mut collector = DecompilationCollector::new(sources, contracts, source_text);
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
                source: Some("masm-decompiler".to_string()),
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

    DecompilationResult { hints, diagnostics }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_decompiler_state_new() {
        let state = DecompilerState::new(3);
        assert_eq!(state.stack.len(), 3);
        assert_eq!(state.peek(0), "a_0");
        assert_eq!(state.peek(1), "a_1");
        assert_eq!(state.peek(2), "a_2");
    }

    #[test]
    fn test_decompiler_push_pop() {
        let mut state = DecompilerState::new(1);
        assert_eq!(state.peek(0), "a_0");

        state.push("v_0".to_string());
        assert_eq!(state.peek(0), "v_0");
        assert_eq!(state.peek(1), "a_0");

        assert_eq!(state.pop(), "v_0");
        assert_eq!(state.peek(0), "a_0");
    }

    #[test]
    fn test_decompiler_dup() {
        let mut state = DecompilerState::new(2);
        // Stack: [a_1, a_0] (a_0 on top)
        let duped = state.dup(1);
        assert_eq!(duped, "a_1");
    }

    #[test]
    fn test_decompiler_swap() {
        let mut state = DecompilerState::new(2);
        // Stack: [a_1, a_0] (a_0 on top)
        state.swap(0, 1);
        assert_eq!(state.peek(0), "a_1");
        assert_eq!(state.peek(1), "a_0");
    }

    #[test]
    fn test_binary_op_pseudocode() {
        let mut state = DecompilerState::new(2);
        // Stack: [a_1, a_0] (a_0 on top)
        let result = binary_op_pseudocode(&mut state, "+");
        assert_eq!(result, Some("v_0 = a_1 + a_0".to_string()));
        assert_eq!(state.peek(0), "v_0");
    }

    #[test]
    fn test_dynamic_input_discovery_pop() {
        let mut state = DecompilerState::new(0); // Start with no inputs
        assert_eq!(state.total_inputs(), 0);

        // Pop from empty stack - should discover inputs
        let a = state.pop();
        assert_eq!(a, "a_0");
        assert_eq!(state.total_inputs(), 1);

        let b = state.pop();
        assert_eq!(b, "a_1");
        assert_eq!(state.total_inputs(), 2);
    }

    #[test]
    fn test_dynamic_input_discovery_dup() {
        let mut state = DecompilerState::new(1); // Start with 1 input
        assert_eq!(state.total_inputs(), 1);

        // Dup beyond current stack - should discover inputs
        let duped = state.dup(2); // Access position 2, but stack only has 1 element
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
        assert_eq!(state.peek(0), "a_1");
        assert_eq!(state.peek(1), "a_0");
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
            "proc bar(a_0, a_1, a_2)"
        );
    }

    #[test]
    fn test_format_procedure_signature_with_return() {
        assert_eq!(
            format_procedure_signature("pub proc", "baz", 2, Some(1)),
            "pub proc baz(a_0, a_1) -> r_0"
        );
    }

    #[test]
    fn test_format_procedure_signature_multiple_returns() {
        assert_eq!(
            format_procedure_signature("proc", "maj", 3, Some(2)),
            "proc maj(a_0, a_1, a_2) -> r_0, r_1"
        );
    }

    #[test]
    fn test_format_procedure_signature_no_args_with_return() {
        assert_eq!(
            format_procedure_signature("export", "get_value", 0, Some(1)),
            "export get_value() -> r_0"
        );
    }

    #[test]
    fn test_format_procedure_signature_no_prefix() {
        assert_eq!(
            format_procedure_signature("", "internal", 1, Some(1)),
            "internal(a_0) -> r_0"
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
        assert_eq!(rename_variable("v_0 = a_0", "v_0", "r_0"), "r_0 = a_0");
    }

    #[test]
    fn test_rename_variable_in_expression() {
        assert_eq!(
            rename_variable("v_1 = a_0 + v_0", "v_1", "r_0"),
            "r_0 = a_0 + v_0"
        );
    }

    #[test]
    fn test_rename_variable_whole_word_only() {
        // Should not rename v_1 inside v_12
        assert_eq!(rename_variable("v_12 = v_1", "v_1", "r_0"), "v_12 = r_0");
    }

    #[test]
    fn test_rename_variable_multiple_occurrences() {
        assert_eq!(
            rename_variable("v_0 = v_0 + v_0", "v_0", "r_0"),
            "r_0 = r_0 + r_0"
        );
    }

    #[test]
    fn test_save_and_restore_state() {
        let mut state = DecompilerState::new(2);
        // Initial stack: [a_1, a_0] (a_0 on top)
        assert_eq!(state.peek(0), "a_0");
        assert_eq!(state.peek(1), "a_1");

        // Save the state
        let saved = state.save_state();

        // Modify the state by pushing a new variable
        let v = state.new_var();
        state.push(v);
        assert_eq!(state.peek(0), "v_0");
        assert_eq!(state.stack.len(), 3);

        // Restore the state
        state.restore_state(&saved);
        assert_eq!(state.peek(0), "a_0");
        assert_eq!(state.stack.len(), 2);
    }

    #[test]
    fn test_get_rename_map_matching_stacks() {
        let mut state = DecompilerState::new(2);
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
        state.pop();
        let v = state.new_var();
        state.push(v);

        // Now state has [a_1, v_0] but saved has [a_1, a_0]
        // Position 1 (top) should need renaming: v_0 -> a_0
        let renames = state.get_rename_map(&saved);
        assert_eq!(renames.len(), 1);
        assert_eq!(renames[0], ("v_0".to_string(), "a_0".to_string()));
    }

    #[test]
    fn test_apply_renames() {
        let mut state = DecompilerState::new(0);
        state.push("v_0".to_string());
        state.push("v_1".to_string());

        let renames = vec![
            ("v_0".to_string(), "a_0".to_string()),
            ("v_1".to_string(), "a_1".to_string()),
        ];
        state.apply_renames(&renames);

        assert_eq!(state.peek(0), "a_1");
        assert_eq!(state.peek(1), "a_0");
    }

    #[test]
    fn test_loop_variable_consistency_simulation() {
        // Simulate what happens in a while loop:
        // Entry state: [cond, counter, result] where cond is on top
        // After body: [new_cond, new_counter, new_result]
        // We want to rename new_* to the original names

        let mut state = DecompilerState::new(0);
        state.push("result".to_string());
        state.push("counter".to_string());
        state.push("cond".to_string());

        // Save loop entry state (with condition)
        let loop_entry_state = state.save_state();

        // Pop condition (simulating while.true consuming it)
        state.pop();

        // Simulate loop body: modify counter and result, push new condition
        state.pop(); // pop counter
        state.push("v_0".to_string()); // new counter value
        state.pop(); // pop old result (now at top)
        state.push("v_1".to_string()); // new result
        // Swap to restore order
        state.swap(0, 1);
        // Push new condition
        state.push("v_2".to_string());

        // Stack is now: [result=v_1, counter=v_0, cond=v_2]
        // We want to rename to: [result, counter, cond]

        let renames = state.get_rename_map(&loop_entry_state);

        // Should have 3 renames: v_1->result, v_0->counter, v_2->cond
        assert_eq!(renames.len(), 3);

        // Apply renames to state
        state.apply_renames(&renames);

        // Verify stack has consistent names
        assert_eq!(state.peek(0), "cond");
        assert_eq!(state.peek(1), "counter");
        assert_eq!(state.peek(2), "result");
    }

    #[test]
    fn test_if_else_branch_state_isolation() {
        // Simulate if/else where each branch gets the same entry state
        let mut state = DecompilerState::new(2);

        // Pop condition (simulating if.true consuming it)
        state.pop();

        // Save entry state (after condition popped)
        let entry_state = state.save_state();
        assert_eq!(state.peek(0), "a_1");

        // Simulate then-branch: push a new value
        let v_then = state.new_var();
        state.push(v_then);
        assert_eq!(state.peek(0), "v_0");

        // Save then-exit state
        let then_exit_state = state.save_state();

        // Restore entry state for else-branch
        state.restore_state(&entry_state);
        assert_eq!(state.peek(0), "a_1"); // Should be back to entry state

        // Simulate else-branch: push a new value (gets different name because next_var_id wasn't reset)
        let v_else = state.new_var();
        state.push(v_else);
        assert_eq!(state.peek(0), "v_1"); // Gets v_1, not v_0

        // Get rename map to make else-branch consistent with then-branch
        let renames = state.get_rename_map(&then_exit_state);
        assert_eq!(renames.len(), 1);
        assert_eq!(renames[0], ("v_1".to_string(), "v_0".to_string()));

        // Apply renames
        state.apply_renames(&renames);
        assert_eq!(state.peek(0), "v_0"); // Now consistent with then-branch
    }

    #[test]
    fn test_counter_generation() {
        let mut state = DecompilerState::new(0);
        assert_eq!(state.new_counter(), "i");
        assert_eq!(state.new_counter(), "j");
        assert_eq!(state.new_counter(), "k");
    }

    #[test]
    fn test_apply_counter_indexing_consuming_loop() {
        // For a consuming loop (net effect -1), a_0 -> a_i (simplified), a_5 -> a_(5+i)
        let result = apply_counter_indexing("v_0 = a_0 + a_5", "i", -1);
        assert_eq!(result, "v_0 = a_i + a_(5+i)");
    }

    #[test]
    fn test_apply_counter_indexing_consuming_loop_larger_effect() {
        // For a loop that consumes 2 per iteration: a_0 -> a_(i*2), a_5 -> a_(5+i*2)
        let result = apply_counter_indexing("v_0 = a_0 + a_5", "i", -2);
        assert_eq!(result, "v_0 = a_(i*2) + a_(5+i*2)");
    }

    #[test]
    fn test_apply_counter_indexing_producing_loop() {
        // For a producing loop (net effect +1), a_0 -> a_(-i), a_5 -> a_(5-i)
        let result = apply_counter_indexing("v_0 = a_0 + a_5", "i", 1);
        assert_eq!(result, "v_0 = a_(-i) + a_(5-i)");
    }

    #[test]
    fn test_apply_counter_indexing_preserves_non_input_vars() {
        // Should not modify v_0, r_0, etc. - only a_{N} patterns
        let result = apply_counter_indexing("v_0 = a_0 + v_1", "i", -1);
        assert_eq!(result, "v_0 = a_i + v_1");
    }

    #[test]
    fn test_apply_counter_indexing_word_boundaries() {
        // Should not match 'a_' in the middle of other patterns
        let result = apply_counter_indexing("data = a_0", "i", -1);
        assert_eq!(result, "data = a_i");
    }

    #[test]
    fn test_apply_counter_indexing_nested_counter() {
        // With nested loops, use appropriate counter
        let result = apply_counter_indexing("v_0 = a_0 + a_1", "j", -1);
        assert_eq!(result, "v_0 = a_j + a_(1+j)");
    }
}
