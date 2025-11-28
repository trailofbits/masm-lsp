//! Pseudocode generation for Miden assembly instructions.
//!
//! This module contains the main `generate_pseudocode` function and all helper
//! functions for converting instructions to readable pseudocode.

use miden_assembly_syntax::ast::{Immediate, Instruction, InvocationTarget};
use miden_debug_types::SourceSpan;

use crate::analysis::{ContractStore, StackEffect};

use super::state::DecompilerState;

// ═══════════════════════════════════════════════════════════════════════════
// Main Pseudocode Generation
// ═══════════════════════════════════════════════════════════════════════════

/// Generate pseudocode for an instruction.
///
/// Returns `Some(pseudocode)` if the instruction produces output,
/// or `None` for instructions that just manipulate the stack.
pub fn generate_pseudocode(
    inst: &Instruction,
    state: &mut DecompilerState,
    span: SourceSpan,
    contracts: Option<&ContractStore>,
) -> Option<String> {
    use crate::analysis::stack_ops::StackLike;

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
            state.push_name(var.clone());
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
                    state.push_name(var.clone());
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
            state.pop_name();
            None
        }
        Instruction::DropW => {
            for _ in 0..4 {
                state.pop_name();
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
            // FIX: Previous implementation was incorrect
            for i in 0..8 {
                state.swap(i, i + 8);
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
            let a = state.pop_name();
            let var = state.new_var();
            state.push_name(var.clone());
            Some(format!("{} = 1/{}", var, a))
        }
        Instruction::Incr => {
            let a = state.pop_name();
            let var = state.new_var();
            state.push_name(var.clone());
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
            let b = state.pop_name();
            let a = state.pop_name();
            let overflow = state.new_var();
            let result = state.new_var();
            state.push_name(overflow.clone());
            state.push_name(result.clone());
            Some(format!("({}, {}) = {} + {} (overflow)", result, overflow, a, b))
        }
        Instruction::U32OverflowingSub => {
            let b = state.pop_name();
            let a = state.pop_name();
            let underflow = state.new_var();
            let result = state.new_var();
            state.push_name(underflow.clone());
            state.push_name(result.clone());
            Some(format!("({}, {}) = {} - {} (underflow)", result, underflow, a, b))
        }
        Instruction::U32OverflowingMul => {
            let b = state.pop_name();
            let a = state.pop_name();
            let overflow = state.new_var();
            let result = state.new_var();
            state.push_name(overflow.clone());
            state.push_name(result.clone());
            Some(format!("({}, {}) = {} * {} (overflow)", result, overflow, a, b))
        }

        Instruction::U32DivMod => {
            let b = state.pop_name();
            let a = state.pop_name();
            let remainder = state.new_var();
            let quotient = state.new_var();
            state.push_name(remainder.clone());
            state.push_name(quotient.clone());
            Some(format!("({}, {}) = divmod({}, {})", quotient, remainder, a, b))
        }

        Instruction::U32Split => {
            let a = state.pop_name();
            let lo = state.new_var();
            let hi = state.new_var();
            state.push_name(lo.clone());
            state.push_name(hi.clone());
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
            state.push_name(var.clone());
            Some(format!("{} = is_u32(top)", var))
        }

        // u32 divmod with immediate
        Instruction::U32DivModImm(imm) => {
            let a = state.pop_name();
            let divisor = format_u32_immediate(imm);
            let remainder = state.new_var();
            let quotient = state.new_var();
            state.push_name(remainder.clone());
            state.push_name(quotient.clone());
            Some(format!("({}, {}) = divmod({}, {})", quotient, remainder, a, divisor))
        }

        // u32 overflowing with immediate
        Instruction::U32OverflowingAddImm(imm) => {
            let a = state.pop_name();
            let b = format_u32_immediate(imm);
            let overflow = state.new_var();
            let result = state.new_var();
            state.push_name(overflow.clone());
            state.push_name(result.clone());
            Some(format!("({}, {}) = {} + {} (overflow)", result, overflow, a, b))
        }
        Instruction::U32OverflowingSubImm(imm) => {
            let a = state.pop_name();
            let b = format_u32_immediate(imm);
            let underflow = state.new_var();
            let result = state.new_var();
            state.push_name(underflow.clone());
            state.push_name(result.clone());
            Some(format!("({}, {}) = {} - {} (underflow)", result, underflow, a, b))
        }
        Instruction::U32OverflowingMulImm(imm) => {
            let a = state.pop_name();
            let b = format_u32_immediate(imm);
            let overflow = state.new_var();
            let result = state.new_var();
            state.push_name(overflow.clone());
            state.push_name(result.clone());
            Some(format!("({}, {}) = {} * {} (overflow)", result, overflow, a, b))
        }

        // u32 ternary operations
        Instruction::U32OverflowingAdd3 => {
            let c = state.pop_name();
            let b = state.pop_name();
            let a = state.pop_name();
            let carry = state.new_var();
            let result = state.new_var();
            state.push_name(carry.clone());
            state.push_name(result.clone());
            Some(format!("({}, {}) = {} + {} + {} (carry)", result, carry, a, b, c))
        }
        Instruction::U32WrappingAdd3 => {
            let c = state.pop_name();
            let b = state.pop_name();
            let a = state.pop_name();
            let result = state.new_var();
            state.push_name(result.clone());
            Some(format!("{} = {} + {} + {}", result, a, b, c))
        }
        Instruction::U32OverflowingMadd => {
            let c = state.pop_name();
            let b = state.pop_name();
            let a = state.pop_name();
            let overflow = state.new_var();
            let result = state.new_var();
            state.push_name(overflow.clone());
            state.push_name(result.clone());
            Some(format!("({}, {}) = {} * {} + {} (overflow)", result, overflow, a, b, c))
        }
        Instruction::U32WrappingMadd => {
            let c = state.pop_name();
            let b = state.pop_name();
            let a = state.pop_name();
            let result = state.new_var();
            state.push_name(result.clone());
            Some(format!("{} = {} * {} + {}", result, a, b, c))
        }

        // ─────────────────────────────────────────────────────────────────────
        // Memory operations
        // ─────────────────────────────────────────────────────────────────────
        Instruction::MemLoad => {
            let addr = state.pop_name();
            let var = state.new_var();
            state.push_name(var.clone());
            Some(format!("{} = mem[{}]", var, addr))
        }
        Instruction::MemLoadImm(imm) => {
            let addr = format_u32_immediate(imm);
            let var = state.new_var();
            state.push_name(var.clone());
            Some(format!("{} = mem[{}]", var, addr))
        }
        Instruction::MemStore => {
            let addr = state.pop_name();
            let val = state.pop_name();
            Some(format!("mem[{}] = {}", addr, val))
        }
        Instruction::MemStoreImm(imm) => {
            let addr = format_u32_immediate(imm);
            let val = state.pop_name();
            Some(format!("mem[{}] = {}", addr, val))
        }

        // Word memory operations
        Instruction::MemLoadWBe | Instruction::MemLoadWLe => {
            let addr = state.pop_name();
            let mut vars = Vec::new();
            for _ in 0..4 {
                let var = state.new_var();
                state.push_name(var.clone());
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
                state.push_name(var.clone());
                vars.push(var);
            }
            vars.reverse();
            Some(format!("({}) = mem_w[{}]", vars.join(", "), addr))
        }
        Instruction::MemStoreWBe | Instruction::MemStoreWLe => {
            let addr = state.pop_name();
            let mut vals = Vec::new();
            for _ in 0..4 {
                vals.push(state.pop_name());
            }
            vals.reverse();
            Some(format!("mem_w[{}] = ({})", addr, vals.join(", ")))
        }
        Instruction::MemStoreWBeImm(imm) | Instruction::MemStoreWLeImm(imm) => {
            let addr = format_u32_immediate(imm);
            let mut vals = Vec::new();
            for _ in 0..4 {
                vals.push(state.pop_name());
            }
            vals.reverse();
            Some(format!("mem_w[{}] = ({})", addr, vals.join(", ")))
        }
        Instruction::MemStream => {
            // Pop 12, push 12 (memory streaming)
            for _ in 0..12 {
                state.pop_name();
            }
            let mut vars = Vec::new();
            for _ in 0..12 {
                let var = state.new_var();
                state.push_name(var.clone());
                vars.push(var);
            }
            vars.reverse();
            Some(format!("({}) = mem_stream()", vars.join(", ")))
        }

        // Local memory - single element
        Instruction::LocLoad(idx) => {
            let idx_val = format_u16_immediate(idx);
            let var = state.new_var();
            state.push_name(var.clone());
            Some(format!("{} = local[{}]", var, idx_val))
        }
        Instruction::LocStore(idx) => {
            let idx_val = format_u16_immediate(idx);
            let val = state.pop_name();
            Some(format!("local[{}] = {}", idx_val, val))
        }

        // Local memory - word level (4 elements)
        Instruction::LocLoadWBe(idx) | Instruction::LocLoadWLe(idx) => {
            let idx_val = format_u16_immediate(idx);
            let mut vars = Vec::new();
            for _ in 0..4 {
                let var = state.new_var();
                state.push_name(var.clone());
                vars.push(var);
            }
            vars.reverse();
            Some(format!("({}) = local_w[{}]", vars.join(", "), idx_val))
        }
        Instruction::LocStoreWBe(idx) | Instruction::LocStoreWLe(idx) => {
            let idx_val = format_u16_immediate(idx);
            let mut vals = Vec::new();
            for _ in 0..4 {
                vals.push(state.pop_name());
            }
            vals.reverse();
            Some(format!("local_w[{}] = ({})", idx_val, vals.join(", ")))
        }
        Instruction::Locaddr(idx) => {
            let idx_val = format_u16_immediate(idx);
            let var = state.new_var();
            state.push_name(var.clone());
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
                state.push_name(var.clone());
                vars.push(var);
            }
            vars.reverse();
            Some(format!("({}) = advice()", vars.join(", ")))
        }
        Instruction::AdvLoadW => {
            // Pop 4 values (address), push 4 values (loaded word)
            for _ in 0..4 {
                state.pop_name();
            }
            let mut vars = Vec::new();
            for _ in 0..4 {
                let var = state.new_var();
                state.push_name(var.clone());
                vars.push(var);
            }
            vars.reverse();
            Some(format!("({}) = advice_w()", vars.join(", ")))
        }
        Instruction::AdvPipe => {
            // Pop 8, push 8 (hasher state + word)
            for _ in 0..8 {
                state.pop_name();
            }
            let mut vars = Vec::new();
            for _ in 0..8 {
                let var = state.new_var();
                state.push_name(var.clone());
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
                args.push(state.pop_name());
            }
            args.reverse();
            let mut vars = Vec::new();
            for _ in 0..4 {
                let var = state.new_var();
                state.push_name(var.clone());
                vars.push(var);
            }
            vars.reverse();
            Some(format!("({}) = hash(({}))", vars.join(", "), args.join(", ")))
        }
        Instruction::HMerge => {
            let mut args = Vec::new();
            for _ in 0..8 {
                args.push(state.pop_name());
            }
            args.reverse();
            let mut vars = Vec::new();
            for _ in 0..4 {
                let var = state.new_var();
                state.push_name(var.clone());
                vars.push(var);
            }
            vars.reverse();
            Some(format!("({}) = hmerge(...)", vars.join(", ")))
        }
        Instruction::HPerm => {
            // Pop 12, push 12
            for _ in 0..12 {
                state.pop_name();
            }
            for _ in 0..12 {
                let var = state.new_var();
                state.push_name(var.clone());
            }
            Some("... = hperm(...)".to_string())
        }

        // ─────────────────────────────────────────────────────────────────────
        // Merkle tree operations
        // ─────────────────────────────────────────────────────────────────────
        Instruction::MTreeGet => {
            // Pop depth, index (2), push value word (4) - root word stays
            state.pop_name(); // depth
            state.pop_name(); // index
            let mut vars = Vec::new();
            for _ in 0..4 {
                let var = state.new_var();
                state.push_name(var.clone());
                vars.push(var);
            }
            vars.reverse();
            Some(format!("({}) = mtree_get()", vars.join(", ")))
        }
        Instruction::MTreeSet => {
            // Pop depth, index (2), push new root word (4) - old root and value stay
            state.pop_name(); // depth
            state.pop_name(); // index
            let mut vars = Vec::new();
            for _ in 0..4 {
                let var = state.new_var();
                state.push_name(var.clone());
                vars.push(var);
            }
            vars.reverse();
            Some(format!("({}) = mtree_set()", vars.join(", ")))
        }
        Instruction::MTreeMerge => {
            // Pop 8 (two words), push 4 (merged hash)
            for _ in 0..8 {
                state.pop_name();
            }
            let mut vars = Vec::new();
            for _ in 0..4 {
                let var = state.new_var();
                state.push_name(var.clone());
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
            let cond = state.pop_name();
            Some(format!("assert({})", cond))
        }
        Instruction::AssertEq | Instruction::AssertEqWithError(_) => {
            let b = state.pop_name();
            let a = state.pop_name();
            Some(format!("assert({} == {})", a, b))
        }
        Instruction::Assertz | Instruction::AssertzWithError(_) => {
            let val = state.pop_name();
            Some(format!("assert({} == 0)", val))
        }
        Instruction::AssertEqw | Instruction::AssertEqwWithError(_) => {
            // Pop two words (8 elements)
            // Word B is on top (positions 0-3), Word A is below (positions 4-7)
            let b3 = state.pop_name();
            let b2 = state.pop_name();
            let b1 = state.pop_name();
            let b0 = state.pop_name();
            let a3 = state.pop_name();
            let a2 = state.pop_name();
            let a1 = state.pop_name();
            let a0 = state.pop_name();
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
                        args.push(state.pop_name());
                    }
                    args.reverse();

                    let mut results = Vec::new();
                    for _ in 0..*outputs {
                        let var = state.new_var();
                        state.push_name(var.clone());
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
                state.pop_name();
            }
            state.fail_tracking(span, "dynamic call has unknown stack effect");
            Some("call <dynamic>".to_string())
        }

        // ─────────────────────────────────────────────────────────────────────
        // Conditionals
        // ─────────────────────────────────────────────────────────────────────
        Instruction::CSwap => {
            let cond = state.pop_name();
            // Conditionally swap top two elements
            Some(format!("cswap({})", cond))
        }
        Instruction::CSwapW => {
            let cond = state.pop_name();
            // Conditionally swap top two words (8 elements)
            Some(format!("cswapw({})", cond))
        }
        Instruction::CDrop => {
            let cond = state.pop_name();
            state.pop_name(); // Drop one of two values
            Some(format!("cdrop({})", cond))
        }
        Instruction::CDropW => {
            let cond = state.pop_name();
            // Drop one of two words (4 elements)
            for _ in 0..4 {
                state.pop_name();
            }
            Some(format!("cdropw({})", cond))
        }

        // ─────────────────────────────────────────────────────────────────────
        // Other operations
        // ─────────────────────────────────────────────────────────────────────
        Instruction::Sdepth => {
            let var = state.new_var();
            state.push_name(var.clone());
            Some(format!("{} = stack_depth()", var))
        }
        Instruction::Clk => {
            let var = state.new_var();
            state.push_name(var.clone());
            Some(format!("{} = clock()", var))
        }
        Instruction::Caller => {
            let mut vars = Vec::new();
            for _ in 0..4 {
                let var = state.new_var();
                state.push_name(var.clone());
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
            let base = state.pop_name();
            let var = state.new_var();
            state.push_name(var.clone());
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
                state.push_name(var.clone());
                vars.push(var);
            }
            vars.reverse();
            Some(format!("({}) = &{}", vars.join(", "), name))
        }

        // Word equality
        Instruction::Eqw => {
            // Pop two words (8 elements), push bool
            // Word B is on top (positions 0-3), Word A is below (positions 4-7)
            let b3 = state.pop_name();
            let b2 = state.pop_name();
            let b1 = state.pop_name();
            let b0 = state.pop_name();
            let a3 = state.pop_name();
            let a2 = state.pop_name();
            let a1 = state.pop_name();
            let a0 = state.pop_name();
            let var = state.new_var();
            state.push_name(var.clone());
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
                state.push_name(var.clone());
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
                state.push_name(var.clone());
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
    let src_name = state.dup_name(n);
    let var = state.new_var();
    state.push_name(var.clone());
    Some(format!("{} = {}", var, src_name))
}

fn dupw_pseudocode(state: &mut DecompilerState, word_idx: usize) -> Option<String> {
    use crate::analysis::stack_ops::StackLike;

    // DupW duplicates a word (4 elements) at word position word_idx
    // Word 0 = positions 0-3, Word 1 = positions 4-7, etc.
    let base = word_idx * 4;

    // Collect the source values BEFORE duplicating
    // These are the original variable names we're copying from
    let sources: Vec<String> = (0..4).map(|i| state.peek_name(base + i)).collect();

    // Use the StackLike trait's dupw implementation for correct stack manipulation
    // This pushes 4 new elements (copies of positions base, base+1, base+2, base+3)
    state.dupw(word_idx);

    // Generate new variable names for the duplicated word and rename them
    // After dupw, the top 4 elements are the duplicated ones
    let mut new_vars = Vec::new();
    let len = state.stack.len();
    for i in 0..4 {
        let var = state.new_var();
        // Rename element at position (len - 4 + i), i.e., the duplicated word
        state.stack[len - 4 + i].name = var.clone();
        new_vars.push(var);
    }

    // Format: (new_var_for_pos0, new_var_for_pos1, ...) = (source_0, source_1, ...)
    // new_vars[0] is at bottom of word (position len-4), new_vars[3] is on top (position len-1)
    // We want output to show top-to-bottom: (v_3, v_2, v_1, v_0) = (a_0, a_1, a_2, a_3)
    new_vars.reverse();
    Some(format!(
        "({}) = ({})",
        new_vars.join(", "),
        sources.join(", ")
    ))
}

pub fn binary_op_pseudocode(state: &mut DecompilerState, op: &str) -> Option<String> {
    let b = state.pop_name();
    let a = state.pop_name();
    let var = state.new_var();
    state.push_name(var.clone());
    Some(format!("{} = {} {} {}", var, a, op, b))
}

fn binary_imm_op_pseudocode(state: &mut DecompilerState, op: &str, imm: String) -> Option<String> {
    let a = state.pop_name();
    let var = state.new_var();
    state.push_name(var.clone());
    Some(format!("{} = {} {} {}", var, a, op, imm))
}

/// Like binary_op_pseudocode but wraps the expression in parentheses for boolean comparisons.
fn comparison_pseudocode(state: &mut DecompilerState, op: &str) -> Option<String> {
    let b = state.pop_name();
    let a = state.pop_name();
    let var = state.new_var();
    state.push_name(var.clone());
    Some(format!("{} = ({} {} {})", var, a, op, b))
}

/// Like binary_imm_op_pseudocode but wraps the expression in parentheses for boolean comparisons.
fn comparison_imm_pseudocode(state: &mut DecompilerState, op: &str, imm: String) -> Option<String> {
    let a = state.pop_name();
    let var = state.new_var();
    state.push_name(var.clone());
    Some(format!("{} = ({} {} {})", var, a, op, imm))
}

fn unary_op_pseudocode(state: &mut DecompilerState, op: &str) -> Option<String> {
    let a = state.pop_name();
    let var = state.new_var();
    state.push_name(var.clone());
    Some(format!("{} = {}{}", var, op, a))
}

fn unary_fn_pseudocode(state: &mut DecompilerState, fn_name: &str) -> Option<String> {
    let a = state.pop_name();
    let var = state.new_var();
    state.push_name(var.clone());
    Some(format!("{} = {}({})", var, fn_name, a))
}

// Extension field (ext2) operations - work on 2-element values
fn ext2_binary_op_pseudocode(state: &mut DecompilerState, op: &str) -> Option<String> {
    // Pop 4 elements (2 ext2 values), push 2 (1 ext2 result)
    let b1 = state.pop_name();
    let b0 = state.pop_name();
    let a1 = state.pop_name();
    let a0 = state.pop_name();
    let var0 = state.new_var();
    let var1 = state.new_var();
    state.push_name(var0.clone());
    state.push_name(var1.clone());
    Some(format!("({}, {}) = ({}, {}) {} ({}, {})", var0, var1, a0, a1, op, b0, b1))
}

fn ext2_unary_op_pseudocode(state: &mut DecompilerState, op: &str) -> Option<String> {
    // Pop 2, push 2
    let a1 = state.pop_name();
    let a0 = state.pop_name();
    let var0 = state.new_var();
    let var1 = state.new_var();
    state.push_name(var0.clone());
    state.push_name(var1.clone());
    Some(format!("({}, {}) = {}({}, {})", var0, var1, op, a0, a1))
}

fn ext2_unary_fn_pseudocode(state: &mut DecompilerState, fn_name: &str) -> Option<String> {
    // Pop 2, push 2
    let a1 = state.pop_name();
    let a0 = state.pop_name();
    let var0 = state.new_var();
    let var1 = state.new_var();
    state.push_name(var0.clone());
    state.push_name(var1.clone());
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

pub fn format_invocation_target(target: &InvocationTarget) -> String {
    match target {
        InvocationTarget::Symbol(ident) => ident.as_str().to_string(),
        InvocationTarget::Path(path) => path.inner().as_str().to_string(),
        InvocationTarget::MastRoot(root) => format!("0x{}", root.inner()),
    }
}

/// Format a procedure signature for the declaration hint.
///
/// Examples:
/// - `proc foo():` - no inputs, unknown outputs
/// - `pub proc foo(a_0, a_1):` - 2 inputs, unknown outputs
/// - `export.bar(a_0, a_1) -> r_0:` - 2 inputs, 1 output
/// - `proc foo(a_0) -> (r_0, r_1, r_2):` - 1 input, 3 outputs (parenthesized)
pub fn format_procedure_signature(
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
    // Multiple returns are parenthesized, single returns are not
    match outputs {
        Some(0) => format!("{}({}):", full_name, args_str),
        Some(1) => format!("{}({}) -> r_0:", full_name, args_str),
        Some(n) => {
            let returns: Vec<String> = (0..n).map(|i| format!("r_{}", i)).collect();
            format!("{}({}) -> ({}):", full_name, args_str, returns.join(", "))
        }
        None => {
            // Unknown outputs - just show inputs
            format!("{}({}):", full_name, args_str)
        }
    }
}

/// Extract the declaration prefix (proc, pub proc, export) from source text at a given line.
///
/// Returns the prefix string (e.g., "proc", "pub proc", "export") or empty string if not found.
pub fn extract_declaration_prefix(source_text: &str, line: u32) -> String {
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
pub fn rename_variable(text: &str, old_name: &str, new_name: &str) -> String {
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
///
/// FIX: Now uses exact path matching for Path targets instead of suffix matching.
fn lookup_contract_for_target<'a>(
    contracts: &'a ContractStore,
    target: &InvocationTarget,
) -> Option<&'a crate::analysis::ProcContract> {
    match target {
        InvocationTarget::Symbol(ident) => contracts.get_by_name(ident.as_str()),
        InvocationTarget::Path(path) => {
            // Use exact path matching to avoid ambiguity
            contracts.get_by_path(path.inner().as_str())
        }
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
///
/// FIX: Now uses proper word boundary detection to avoid false matches like `data_a_0`.
pub fn apply_input_indexing(text: &str, counter: &str, net_effect: i32) -> String {
    let mut result = String::new();
    let chars: Vec<char> = text.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        // Check for word boundary before 'a_'
        let before_ok = i == 0 || (!chars[i - 1].is_alphanumeric() && chars[i - 1] != '_');

        // Look for 'a_' followed by digits (input variable pattern: a_0, a_1, etc.)
        if before_ok && chars[i] == 'a' && i + 2 < chars.len() && chars[i + 1] == '_' && chars[i + 2].is_ascii_digit() {
            // Parse the number following 'a_'
            let num_start = i + 2;
            let mut num_end = num_start;
            while num_end < chars.len() && chars[num_end].is_ascii_digit() {
                num_end += 1;
            }

            // Check word boundary after the number (must not be followed by alphanumeric or underscore)
            let after_ok = num_end >= chars.len()
                || (!chars[num_end].is_alphanumeric() && chars[num_end] != '_');

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
        result.push(chars[i]);
        i += 1;
    }

    result
}

/// Apply counter indexing to output variables in a hint string.
///
/// For loops with non-zero net stack effect, output variable references like `v_3`, `v_4`
/// (where those variables were created during the loop body) need to be converted to
/// indexed form like `v_(3+i)`, `v_(4+i)` to show that different iterations produce
/// results at different stack positions.
///
/// # Arguments
/// * `text` - The hint string to transform
/// * `counter` - The loop counter variable name (e.g., "i", "j")
/// * `start_var_id` - The first variable ID created during the loop body
/// * `vars_per_iteration` - Number of variables created per iteration
///
/// # Formula
/// For `v_N` where `N >= start_var_id`:
/// - Transform to `v_(N + i*K)` where K = vars_per_iteration
/// - Simplifies `v_(0+i)` to `v_i`, `v_(N+i*1)` to `v_(N+i)`, etc.
pub fn apply_output_indexing(
    text: &str,
    counter: &str,
    start_var_id: usize,
    vars_per_iteration: usize,
) -> String {
    if vars_per_iteration == 0 {
        return text.to_string();
    }

    let mut result = String::new();
    let chars: Vec<char> = text.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        // Check for word boundary before 'v_'
        let before_ok = i == 0 || (!chars[i - 1].is_alphanumeric() && chars[i - 1] != '_');

        // Look for 'v_' followed by digits (output variable pattern: v_0, v_1, etc.)
        if before_ok && chars[i] == 'v' && i + 2 < chars.len() && chars[i + 1] == '_' && chars[i + 2].is_ascii_digit() {
            // Parse the number following 'v_'
            let num_start = i + 2;
            let mut num_end = num_start;
            while num_end < chars.len() && chars[num_end].is_ascii_digit() {
                num_end += 1;
            }

            // Check word boundary after the number
            let after_ok = num_end >= chars.len()
                || (!chars[num_end].is_alphanumeric() && chars[num_end] != '_');

            if after_ok {
                let num_str: String = chars[num_start..num_end].iter().collect();
                if let Ok(var_id) = num_str.parse::<usize>() {
                    // Only transform variables created during the loop (var_id >= start_var_id)
                    if var_id >= start_var_id {
                        // Generate indexed form: v_(N + i*K)
                        let indexed = format_indexed_var(var_id, counter, vars_per_iteration);
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

/// Format an indexed variable name with simplification.
///
/// Generates `v_(base + counter*stride)` with simplifications:
/// - `v_(0+i)` -> `v_i`
/// - `v_(N+i*1)` -> `v_(N+i)`
/// - `v_(i*1)` -> `v_i`
fn format_indexed_var(base: usize, counter: &str, stride: usize) -> String {
    if stride == 1 {
        // Simplify: v_(N+i*1) -> v_(N+i) or v_i
        if base == 0 {
            format!("v_{}", counter)
        } else {
            format!("v_({}+{})", base, counter)
        }
    } else {
        // Full form: v_(N+i*K)
        if base == 0 {
            format!("v_({}*{})", counter, stride)
        } else {
            format!("v_({}+{}*{})", base, counter, stride)
        }
    }
}

/// Legacy wrapper for apply_input_indexing (backwards compatibility).
pub fn apply_counter_indexing(text: &str, counter: &str, net_effect: i32) -> String {
    apply_input_indexing(text, counter, net_effect)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_procedure_signature_no_args() {
        assert_eq!(
            format_procedure_signature("proc", "foo", 0, Some(0)),
            "proc foo():"
        );
    }

    #[test]
    fn test_format_procedure_signature_with_args() {
        assert_eq!(
            format_procedure_signature("proc", "bar", 3, None),
            "proc bar(a_0, a_1, a_2):"
        );
    }

    #[test]
    fn test_format_procedure_signature_with_return() {
        assert_eq!(
            format_procedure_signature("pub proc", "baz", 2, Some(1)),
            "pub proc baz(a_0, a_1) -> r_0:"
        );
    }

    #[test]
    fn test_format_procedure_signature_multiple_returns() {
        assert_eq!(
            format_procedure_signature("proc", "maj", 3, Some(2)),
            "proc maj(a_0, a_1, a_2) -> (r_0, r_1):"
        );
    }

    #[test]
    fn test_format_procedure_signature_no_args_with_return() {
        assert_eq!(
            format_procedure_signature("export", "get_value", 0, Some(1)),
            "export get_value() -> r_0:"
        );
    }

    #[test]
    fn test_format_procedure_signature_no_prefix() {
        assert_eq!(
            format_procedure_signature("", "internal", 1, Some(1)),
            "internal(a_0) -> r_0:"
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
        // Should not match 'a_' in the middle of other patterns like 'data_a_0'
        let result = apply_counter_indexing("data = a_0", "i", -1);
        assert_eq!(result, "data = a_i");
    }

    #[test]
    fn test_apply_counter_indexing_nested_counter() {
        // With nested loops, use appropriate counter
        let result = apply_counter_indexing("v_0 = a_0 + a_1", "j", -1);
        assert_eq!(result, "v_0 = a_j + a_(1+j)");
    }

    // ─────────────────────────────────────────────────────────────────────────────
    // Tests for apply_output_indexing
    // ─────────────────────────────────────────────────────────────────────────────

    #[test]
    fn test_apply_output_indexing_single_var() {
        // Single variable created in loop, start_var_id = 0
        // v_0 -> v_i (simplified from v_(0+i))
        let result = apply_output_indexing("v_0 = a_0 + a_5", "i", 0, 1);
        assert_eq!(result, "v_i = a_0 + a_5");
    }

    #[test]
    fn test_apply_output_indexing_with_offset() {
        // Variable created in loop with prior variables existing
        // start_var_id = 3, so v_3 -> v_(3+i)
        let result = apply_output_indexing("v_3 = a_0 + a_5", "i", 3, 1);
        assert_eq!(result, "v_(3+i) = a_0 + a_5");
    }

    #[test]
    fn test_apply_output_indexing_preserves_prior_vars() {
        // Variables before the loop (v_0, v_1, v_2) should not be transformed
        // Only v_3 (>= start_var_id) should be transformed
        let result = apply_output_indexing("v_3 = v_0 + v_1", "i", 3, 1);
        assert_eq!(result, "v_(3+i) = v_0 + v_1");
    }

    #[test]
    fn test_apply_output_indexing_multiple_vars_per_iteration() {
        // Two variables created per iteration: v_0 and v_1
        // v_0 -> v_(i*2), v_1 -> v_(1+i*2)
        let result = apply_output_indexing("(v_0, v_1) = something", "i", 0, 2);
        assert_eq!(result, "(v_(i*2), v_(1+i*2)) = something");
    }

    #[test]
    fn test_apply_output_indexing_with_offset_and_stride() {
        // start_var_id = 2, vars_per_iteration = 2
        // v_2 -> v_(2+i*2), v_3 -> v_(3+i*2)
        let result = apply_output_indexing("(v_2, v_3) = something", "i", 2, 2);
        assert_eq!(result, "(v_(2+i*2), v_(3+i*2)) = something");
    }

    #[test]
    fn test_apply_output_indexing_different_counter() {
        // With nested loops, use the appropriate counter
        let result = apply_output_indexing("v_0 = a_0", "j", 0, 1);
        assert_eq!(result, "v_j = a_0");
    }

    #[test]
    fn test_apply_output_indexing_word_boundaries() {
        // Should not match 'v_' in the middle of other patterns
        let result = apply_output_indexing("prev_0 = v_0", "i", 0, 1);
        assert_eq!(result, "prev_0 = v_i");
    }

    #[test]
    fn test_apply_output_indexing_zero_vars() {
        // When vars_per_iteration is 0, should return unchanged
        let result = apply_output_indexing("v_0 = a_0", "i", 0, 0);
        assert_eq!(result, "v_0 = a_0");
    }

    #[test]
    fn test_combined_input_and_output_indexing() {
        // Test the full transformation for a consuming loop
        let text = "v_0 = a_0 + a_5";
        let with_input = apply_input_indexing(text, "i", -1);
        let with_output = apply_output_indexing(&with_input, "i", 0, 1);
        assert_eq!(with_output, "v_i = a_i + a_(5+i)");
    }

    #[test]
    fn test_combined_indexing_with_prior_vars() {
        // Prior variables (v_0, v_1) exist, loop creates v_2
        let text = "v_2 = v_0 + a_0 + a_5";
        let with_input = apply_input_indexing(text, "i", -1);
        let with_output = apply_output_indexing(&with_input, "i", 2, 1);
        assert_eq!(with_output, "v_(2+i) = v_0 + a_i + a_(5+i)");
    }
}
