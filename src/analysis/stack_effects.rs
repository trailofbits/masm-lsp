//! Stack effect handling for instructions.
//!
//! This module defines how each instruction affects the symbolic stack,
//! including pushing/popping values and applying validation.

use miden_assembly_syntax::ast::{Immediate, Instruction};
use miden_debug_types::SourceSpan;

use super::types::{AnalysisState, Bounds};
use super::utils::{felt_imm_to_u64, push_imm_to_u64};

/// Apply an instruction's effect on the analysis state.
///
/// This updates the symbolic stack to reflect what values would be
/// present after the instruction executes.
pub fn apply_effect(inst: &Instruction, state: &mut AnalysisState, span: SourceSpan) {
    match inst {
        // ─────────────────────────────────────────────────────────────────────
        // Advice operations (UNTRUSTED input)
        // ─────────────────────────────────────────────────────────────────────
        Instruction::AdvPush(n) => {
            let count = match n {
                Immediate::Value(v) => v.into_inner() as usize,
                _ => 1,
            };
            for _ in 0..count {
                let taint = state.make_advice(span);
                state.stack.push(taint);
            }
        }

        Instruction::AdvLoadW => {
            // Pop the address word first
            for _ in 0..4 {
                state.stack.pop();
            }
            // Push 4 untrusted values
            for _ in 0..4 {
                let taint = state.make_advice(span);
                state.stack.push(taint);
            }
        }

        Instruction::AdvPipe => {
            // Consumes 2 words, produces 2 words of advice
            for _ in 0..8 {
                state.stack.pop();
            }
            for _ in 0..8 {
                let taint = state.make_advice(span);
                state.stack.push(taint);
            }
        }

        // ─────────────────────────────────────────────────────────────────────
        // Merkle operations
        // ─────────────────────────────────────────────────────────────────────
        Instruction::MTreeGet => {
            // Pops: depth, index, root (4 elements for root word)
            // Pushes: value (4 elements)
            state.stack.pop(); // depth
            state.stack.pop(); // index
            // Keep root word, push result from Merkle store
            let value = state.make_merkle(span);
            state.stack.push(value);
        }

        Instruction::MTreeSet => {
            // Stack is modified but we'll simplify
            // In reality this is complex - for now just track that it uses Merkle store
        }

        // ─────────────────────────────────────────────────────────────────────
        // Push operations - trusted values
        // ─────────────────────────────────────────────────────────────────────
        Instruction::Push(imm) => {
            // Push instructions push trusted literal values - extract actual value
            let value = push_imm_to_u64(imm).unwrap_or(0);
            let taint = state.make_literal(value, Some(span));
            state.stack.push(taint);
        }

        Instruction::PushFeltList(values) => {
            for felt in values {
                let value = felt.as_int();
                let taint = state.make_literal(value, Some(span));
                state.stack.push(taint);
            }
        }

        Instruction::PushSlice(imm, range) => {
            // PushSlice pushes a subset of a word
            let count = range.len();
            for _ in 0..count {
                // We can't easily extract individual values from a word slice
                let taint = state.make_literal(0, Some(span));
                state.stack.push(taint);
            }
            let _ = imm; // Suppress unused warning
        }

        // ─────────────────────────────────────────────────────────────────────
        // Stack manipulation - Drop
        // ─────────────────────────────────────────────────────────────────────
        Instruction::Drop => {
            state.stack.pop();
        }

        Instruction::DropW => {
            for _ in 0..4 {
                state.stack.pop();
            }
        }

        // ─────────────────────────────────────────────────────────────────────
        // Stack manipulation - Dup
        // ─────────────────────────────────────────────────────────────────────
        Instruction::Dup0 => { state.stack.dup(0); }
        Instruction::Dup1 => { state.stack.dup(1); }
        Instruction::Dup2 => { state.stack.dup(2); }
        Instruction::Dup3 => { state.stack.dup(3); }
        Instruction::Dup4 => { state.stack.dup(4); }
        Instruction::Dup5 => { state.stack.dup(5); }
        Instruction::Dup6 => { state.stack.dup(6); }
        Instruction::Dup7 => { state.stack.dup(7); }
        Instruction::Dup8 => { state.stack.dup(8); }
        Instruction::Dup9 => { state.stack.dup(9); }
        Instruction::Dup10 => { state.stack.dup(10); }
        Instruction::Dup11 => { state.stack.dup(11); }
        Instruction::Dup12 => { state.stack.dup(12); }
        Instruction::Dup13 => { state.stack.dup(13); }
        Instruction::Dup14 => { state.stack.dup(14); }
        Instruction::Dup15 => { state.stack.dup(15); }

        Instruction::DupW0 => {
            // Duplicate word at positions 0-3 onto top
            // Each dup pushes and shifts indices, so we always dup position 3
            // This copies bottom-to-top: pos3, then pos2 (now at 3), then pos1, then pos0
            for _ in 0..4 {
                state.stack.dup(3);
            }
        }
        Instruction::DupW1 => {
            // Duplicate word at positions 4-7 onto top
            for _ in 0..4 {
                state.stack.dup(7);
            }
        }
        Instruction::DupW2 => {
            // Duplicate word at positions 8-11 onto top
            for _ in 0..4 {
                state.stack.dup(11);
            }
        }
        Instruction::DupW3 => {
            // Duplicate word at positions 12-15 onto top
            for _ in 0..4 {
                state.stack.dup(15);
            }
        }

        // ─────────────────────────────────────────────────────────────────────
        // Stack manipulation - Swap
        // ─────────────────────────────────────────────────────────────────────
        Instruction::Swap1 => { state.stack.swap(0, 1); }
        Instruction::Swap2 => { state.stack.swap(0, 2); }
        Instruction::Swap3 => { state.stack.swap(0, 3); }
        Instruction::Swap4 => { state.stack.swap(0, 4); }
        Instruction::Swap5 => { state.stack.swap(0, 5); }
        Instruction::Swap6 => { state.stack.swap(0, 6); }
        Instruction::Swap7 => { state.stack.swap(0, 7); }
        Instruction::Swap8 => { state.stack.swap(0, 8); }
        Instruction::Swap9 => { state.stack.swap(0, 9); }
        Instruction::Swap10 => { state.stack.swap(0, 10); }
        Instruction::Swap11 => { state.stack.swap(0, 11); }
        Instruction::Swap12 => { state.stack.swap(0, 12); }
        Instruction::Swap13 => { state.stack.swap(0, 13); }
        Instruction::Swap14 => { state.stack.swap(0, 14); }
        Instruction::Swap15 => { state.stack.swap(0, 15); }

        // ─────────────────────────────────────────────────────────────────────
        // Stack manipulation - Move
        // ─────────────────────────────────────────────────────────────────────
        Instruction::MovUp2 => { state.stack.movup(2); }
        Instruction::MovUp3 => { state.stack.movup(3); }
        Instruction::MovUp4 => { state.stack.movup(4); }
        Instruction::MovUp5 => { state.stack.movup(5); }
        Instruction::MovUp6 => { state.stack.movup(6); }
        Instruction::MovUp7 => { state.stack.movup(7); }
        Instruction::MovUp8 => { state.stack.movup(8); }
        Instruction::MovUp9 => { state.stack.movup(9); }
        Instruction::MovUp10 => { state.stack.movup(10); }
        Instruction::MovUp11 => { state.stack.movup(11); }
        Instruction::MovUp12 => { state.stack.movup(12); }
        Instruction::MovUp13 => { state.stack.movup(13); }
        Instruction::MovUp14 => { state.stack.movup(14); }
        Instruction::MovUp15 => { state.stack.movup(15); }

        Instruction::MovDn2 => { state.stack.movdn(2); }
        Instruction::MovDn3 => { state.stack.movdn(3); }
        Instruction::MovDn4 => { state.stack.movdn(4); }
        Instruction::MovDn5 => { state.stack.movdn(5); }
        Instruction::MovDn6 => { state.stack.movdn(6); }
        Instruction::MovDn7 => { state.stack.movdn(7); }
        Instruction::MovDn8 => { state.stack.movdn(8); }
        Instruction::MovDn9 => { state.stack.movdn(9); }
        Instruction::MovDn10 => { state.stack.movdn(10); }
        Instruction::MovDn11 => { state.stack.movdn(11); }
        Instruction::MovDn12 => { state.stack.movdn(12); }
        Instruction::MovDn13 => { state.stack.movdn(13); }
        Instruction::MovDn14 => { state.stack.movdn(14); }
        Instruction::MovDn15 => { state.stack.movdn(15); }

        // ─────────────────────────────────────────────────────────────────────
        // Stack manipulation - Pad
        // ─────────────────────────────────────────────────────────────────────
        Instruction::PadW => {
            for _ in 0..4 {
                let taint = state.make_literal(0, None);
                state.stack.push(taint);
            }
        }

        // ─────────────────────────────────────────────────────────────────────
        // Arithmetic - propagate bounds through operations
        // ─────────────────────────────────────────────────────────────────────
        Instruction::Add => {
            let b = state.stack.pop();
            let a = state.stack.pop();
            let new_bounds = match (a.as_ref(), b.as_ref()) {
                (Some(a), Some(b)) => a.bounds.add(&b.bounds),
                _ => Bounds::Field,
            };
            let result = state.make_derived(new_bounds);
            state.stack.push(result);
        }
        Instruction::AddImm(imm) => {
            let a = state.stack.pop();
            let imm_val = felt_imm_to_u64(imm).unwrap_or(0);
            let new_bounds = match a.as_ref() {
                Some(a) => a.bounds.add(&Bounds::Const(imm_val)),
                _ => Bounds::Field,
            };
            let result = state.make_derived(new_bounds);
            state.stack.push(result);
        }

        Instruction::Sub => {
            let b = state.stack.pop();
            let a = state.stack.pop();
            let new_bounds = match (a.as_ref(), b.as_ref()) {
                (Some(a), Some(b)) => a.bounds.sub(&b.bounds),
                _ => Bounds::Field,
            };
            let result = state.make_derived(new_bounds);
            state.stack.push(result);
        }
        Instruction::SubImm(imm) => {
            let a = state.stack.pop();
            let imm_val = felt_imm_to_u64(imm).unwrap_or(0);
            let new_bounds = match a.as_ref() {
                Some(a) => a.bounds.sub(&Bounds::Const(imm_val)),
                _ => Bounds::Field,
            };
            let result = state.make_derived(new_bounds);
            state.stack.push(result);
        }

        Instruction::Mul => {
            let b = state.stack.pop();
            let a = state.stack.pop();
            let new_bounds = match (a.as_ref(), b.as_ref()) {
                (Some(a), Some(b)) => a.bounds.mul(&b.bounds),
                _ => Bounds::Field,
            };
            let result = state.make_derived(new_bounds);
            state.stack.push(result);
        }
        Instruction::MulImm(imm) => {
            let a = state.stack.pop();
            let imm_val = felt_imm_to_u64(imm).unwrap_or(0);
            let new_bounds = match a.as_ref() {
                Some(a) => a.bounds.mul(&Bounds::Const(imm_val)),
                _ => Bounds::Field,
            };
            let result = state.make_derived(new_bounds);
            state.stack.push(result);
        }

        Instruction::Div => {
            let b = state.stack.pop();
            let a = state.stack.pop();
            let new_bounds = match (a.as_ref(), b.as_ref()) {
                (Some(a), Some(b)) => a.bounds.div(&b.bounds),
                _ => Bounds::Field,
            };
            let result = state.make_derived(new_bounds);
            state.stack.push(result);
        }
        Instruction::DivImm(imm) => {
            let a = state.stack.pop();
            let imm_val = felt_imm_to_u64(imm).unwrap_or(0);
            let new_bounds = match a.as_ref() {
                Some(a) if imm_val != 0 => a.bounds.div(&Bounds::Const(imm_val)),
                _ => Bounds::Field,
            };
            let result = state.make_derived(new_bounds);
            state.stack.push(result);
        }

        Instruction::Neg => {
            state.stack.pop();
            let result = state.make_derived(Bounds::Field);
            state.stack.push(result);
        }

        Instruction::Inv => {
            state.stack.pop();
            let result = state.make_derived(Bounds::Field);
            state.stack.push(result);
        }

        Instruction::Incr => {
            let a = state.stack.pop();
            let new_bounds = match a.as_ref() {
                Some(a) => a.bounds.incr(),
                _ => Bounds::Field,
            };
            let result = state.make_derived(new_bounds);
            state.stack.push(result);
        }

        // ─────────────────────────────────────────────────────────────────────
        // Comparison operations - propagate bounds through comparisons
        // ─────────────────────────────────────────────────────────────────────
        Instruction::Eq => {
            let b = state.stack.pop();
            let a = state.stack.pop();
            let new_bounds = match (a.as_ref(), b.as_ref()) {
                (Some(a), Some(b)) => a.bounds.eq(&b.bounds),
                _ => Bounds::Bool,
            };
            let result = state.make_derived(new_bounds);
            state.stack.push(result);
        }
        Instruction::EqImm(imm) => {
            let a = state.stack.pop();
            let imm_val = felt_imm_to_u64(imm).unwrap_or(0);
            let new_bounds = match a.as_ref() {
                Some(a) => a.bounds.eq(&Bounds::Const(imm_val)),
                _ => Bounds::Bool,
            };
            let result = state.make_derived(new_bounds);
            state.stack.push(result);
        }

        Instruction::Neq => {
            let b = state.stack.pop();
            let a = state.stack.pop();
            let new_bounds = match (a.as_ref(), b.as_ref()) {
                (Some(a), Some(b)) => a.bounds.neq(&b.bounds),
                _ => Bounds::Bool,
            };
            let result = state.make_derived(new_bounds);
            state.stack.push(result);
        }
        Instruction::NeqImm(imm) => {
            let a = state.stack.pop();
            let imm_val = felt_imm_to_u64(imm).unwrap_or(0);
            let new_bounds = match a.as_ref() {
                Some(a) => a.bounds.neq(&Bounds::Const(imm_val)),
                _ => Bounds::Bool,
            };
            let result = state.make_derived(new_bounds);
            state.stack.push(result);
        }

        Instruction::Lt => {
            let b = state.stack.pop();
            let a = state.stack.pop();
            let new_bounds = match (a.as_ref(), b.as_ref()) {
                (Some(a), Some(b)) => a.bounds.lt(&b.bounds),
                _ => Bounds::Bool,
            };
            let result = state.make_derived(new_bounds);
            state.stack.push(result);
        }

        Instruction::Lte => {
            let b = state.stack.pop();
            let a = state.stack.pop();
            let new_bounds = match (a.as_ref(), b.as_ref()) {
                (Some(a), Some(b)) => a.bounds.lte(&b.bounds),
                _ => Bounds::Bool,
            };
            let result = state.make_derived(new_bounds);
            state.stack.push(result);
        }

        Instruction::Gt => {
            let b = state.stack.pop();
            let a = state.stack.pop();
            let new_bounds = match (a.as_ref(), b.as_ref()) {
                (Some(a), Some(b)) => a.bounds.gt(&b.bounds),
                _ => Bounds::Bool,
            };
            let result = state.make_derived(new_bounds);
            state.stack.push(result);
        }

        Instruction::Gte => {
            let b = state.stack.pop();
            let a = state.stack.pop();
            let new_bounds = match (a.as_ref(), b.as_ref()) {
                (Some(a), Some(b)) => a.bounds.gte(&b.bounds),
                _ => Bounds::Bool,
            };
            let result = state.make_derived(new_bounds);
            state.stack.push(result);
        }

        Instruction::IsOdd => {
            state.stack.pop();
            let result = state.make_derived(Bounds::Bool);
            state.stack.push(result);
        }

        // ─────────────────────────────────────────────────────────────────────
        // Boolean operations
        // ─────────────────────────────────────────────────────────────────────
        Instruction::And => {
            state.stack.pop();
            state.stack.pop();
            let result = state.make_derived(Bounds::Bool);
            state.stack.push(result);
        }

        Instruction::Or => {
            state.stack.pop();
            state.stack.pop();
            let result = state.make_derived(Bounds::Bool);
            state.stack.push(result);
        }

        Instruction::Xor => {
            state.stack.pop();
            state.stack.pop();
            let result = state.make_derived(Bounds::Bool);
            state.stack.push(result);
        }

        Instruction::Not => {
            state.stack.pop();
            let result = state.make_derived(Bounds::Bool);
            state.stack.push(result);
        }

        // ─────────────────────────────────────────────────────────────────────
        // Memory operations - single element
        // ─────────────────────────────────────────────────────────────────────
        Instruction::MemLoad | Instruction::MemLoadImm(_) => {
            state.stack.pop(); // address
            let value = state.make_memory(span);
            state.stack.push(value);
        }

        Instruction::MemStore | Instruction::MemStoreImm(_) => {
            state.stack.pop(); // address
            state.stack.pop(); // value
        }

        // ─────────────────────────────────────────────────────────────────────
        // Memory operations - word level (4 elements)
        // ─────────────────────────────────────────────────────────────────────
        Instruction::MemLoadWBe | Instruction::MemLoadWLe => {
            // pop 1 address, push 4 values from memory
            state.stack.pop(); // address
            for _ in 0..4 {
                let value = state.make_memory(span);
                state.stack.push(value);
            }
        }
        Instruction::MemLoadWBeImm(_) | Instruction::MemLoadWLeImm(_) => {
            // push 4 values from memory (address from immediate)
            for _ in 0..4 {
                let value = state.make_memory(span);
                state.stack.push(value);
            }
        }

        Instruction::MemStoreWBe | Instruction::MemStoreWLe => {
            // pop 1 address + 4 values
            state.stack.pop(); // address
            for _ in 0..4 {
                state.stack.pop(); // value
            }
        }
        Instruction::MemStoreWBeImm(_) | Instruction::MemStoreWLeImm(_) => {
            // pop 4 values (address from immediate)
            for _ in 0..4 {
                state.stack.pop();
            }
        }

        // ─────────────────────────────────────────────────────────────────────
        // Local memory operations
        // ─────────────────────────────────────────────────────────────────────
        Instruction::LocLoad(_) => {
            // push 1 value from local memory
            let value = state.make_memory(span);
            state.stack.push(value);
        }
        Instruction::LocLoadWBe(_) | Instruction::LocLoadWLe(_) => {
            // push 4 values from local memory
            for _ in 0..4 {
                let value = state.make_memory(span);
                state.stack.push(value);
            }
        }

        Instruction::LocStore(_) => {
            // pop 1 value to store
            state.stack.pop();
        }
        Instruction::LocStoreWBe(_) | Instruction::LocStoreWLe(_) => {
            // pop 4 values to store
            for _ in 0..4 {
                state.stack.pop();
            }
        }

        // ─────────────────────────────────────────────────────────────────────
        // Memory streaming
        // ─────────────────────────────────────────────────────────────────────
        Instruction::MemStream => {
            // pops 12 elements, pushes 12 elements
            for _ in 0..12 {
                state.stack.pop();
            }
            for _ in 0..12 {
                let value = state.make_memory(span);
                state.stack.push(value);
            }
        }

        // ─────────────────────────────────────────────────────────────────────
        // Word-level stack manipulation
        // ─────────────────────────────────────────────────────────────────────

        // SwapW - swap words (groups of 4)
        Instruction::SwapW1 => {
            // Swap word 0-3 with word 4-7
            for i in 0..4 {
                state.stack.swap(i, i + 4);
            }
        }
        Instruction::SwapW2 => {
            // Swap word 0-3 with word 8-11
            for i in 0..4 {
                state.stack.swap(i, i + 8);
            }
        }
        Instruction::SwapW3 => {
            // Swap word 0-3 with word 12-15
            for i in 0..4 {
                state.stack.swap(i, i + 12);
            }
        }
        Instruction::SwapDw => {
            // Swap double word (0-7 with 8-15)
            for i in 0..8 {
                state.stack.swap(i, i + 8);
            }
        }

        // MovUpW - move word up to top
        Instruction::MovUpW2 => {
            // Move word at positions 8-11 to 0-3
            // This moves 4 elements: positions 8,9,10,11 become 0,1,2,3
            for _ in 0..4 {
                state.stack.movup(8);
            }
        }
        Instruction::MovUpW3 => {
            // Move word at positions 12-15 to 0-3
            for _ in 0..4 {
                state.stack.movup(12);
            }
        }

        // MovDnW - move word down from top
        Instruction::MovDnW2 => {
            // Move word at positions 0-3 to 8-11
            for _ in 0..4 {
                state.stack.movdn(8);
            }
        }
        Instruction::MovDnW3 => {
            // Move word at positions 0-3 to 12-15
            for _ in 0..4 {
                state.stack.movdn(12);
            }
        }

        // Reverse operations
        Instruction::Reversew => {
            // Reverse elements in top word (positions 0,1,2,3 -> 3,2,1,0)
            state.stack.swap(0, 3);
            state.stack.swap(1, 2);
        }
        Instruction::Reversedw => {
            // Reverse elements in top double word (positions 0-7)
            state.stack.swap(0, 7);
            state.stack.swap(1, 6);
            state.stack.swap(2, 5);
            state.stack.swap(3, 4);
        }

        // ─────────────────────────────────────────────────────────────────────
        // Procedure calls - conservatively clear stack tracking
        // ─────────────────────────────────────────────────────────────────────
        Instruction::Exec(_) | Instruction::Call(_) | Instruction::SysCall(_) => {
            // Procedure calls have complex stack effects that depend on the procedure
            // For now, we don't modify the stack - checkers handle the warnings
        }

        // ─────────────────────────────────────────────────────────────────────
        // u32 operations - explicit handling with correct stack effects
        // ─────────────────────────────────────────────────────────────────────

        // u32 assertions - no stack change, just validation
        Instruction::U32Assert | Instruction::U32AssertWithError(_) => {
            if let Some(t) = state.stack.peek_mut(0) {
                t.apply_validation();
            }
        }
        Instruction::U32Assert2 | Instruction::U32Assert2WithError(_) => {
            if let Some(t) = state.stack.peek_mut(0) {
                t.apply_validation();
            }
            if let Some(t) = state.stack.peek_mut(1) {
                t.apply_validation();
            }
        }
        Instruction::U32AssertW | Instruction::U32AssertWWithError(_) => {
            for i in 0..4 {
                if let Some(t) = state.stack.peek_mut(i) {
                    t.apply_validation();
                }
            }
        }

        // u32 test - pushes boolean result, doesn't pop
        Instruction::U32Test => {
            let result = state.make_derived(Bounds::Bool);
            state.stack.push(result);
        }
        Instruction::U32TestW => {
            let result = state.make_derived(Bounds::Bool);
            state.stack.push(result);
        }

        // u32 split/cast - special arity
        Instruction::U32Split => {
            // pop 1 field element, push 2 u32 values (hi, lo)
            state.stack.pop();
            let hi = state.make_derived(Bounds::u32());
            let lo = state.make_derived(Bounds::u32());
            state.stack.push(lo);
            state.stack.push(hi);
        }
        Instruction::U32Cast => {
            // pop 1, push 1 u32 (truncate to u32)
            state.stack.pop();
            let result = state.make_derived(Bounds::u32());
            state.stack.push(result);
        }

        // u32 binary wrapping arithmetic - pop 2, push 1
        Instruction::U32WrappingAdd => {
            state.stack.pop();
            state.stack.pop();
            let result = state.make_derived(Bounds::u32());
            state.stack.push(result);
        }
        Instruction::U32WrappingAddImm(imm) => {
            state.stack.pop();
            let result = state.make_derived(Bounds::u32());
            state.stack.push(result);
            let _ = imm;
        }
        Instruction::U32WrappingSub => {
            state.stack.pop();
            state.stack.pop();
            let result = state.make_derived(Bounds::u32());
            state.stack.push(result);
        }
        Instruction::U32WrappingSubImm(imm) => {
            state.stack.pop();
            let result = state.make_derived(Bounds::u32());
            state.stack.push(result);
            let _ = imm;
        }
        Instruction::U32WrappingMul => {
            state.stack.pop();
            state.stack.pop();
            let result = state.make_derived(Bounds::u32());
            state.stack.push(result);
        }
        Instruction::U32WrappingMulImm(imm) => {
            state.stack.pop();
            let result = state.make_derived(Bounds::u32());
            state.stack.push(result);
            let _ = imm;
        }

        // u32 binary overflowing arithmetic - pop 2, push 2 (result + overflow flag)
        Instruction::U32OverflowingAdd => {
            state.stack.pop();
            state.stack.pop();
            let overflow = state.make_derived(Bounds::Bool);
            let result = state.make_derived(Bounds::u32());
            state.stack.push(overflow);
            state.stack.push(result);
        }
        Instruction::U32OverflowingAddImm(imm) => {
            state.stack.pop();
            let overflow = state.make_derived(Bounds::Bool);
            let result = state.make_derived(Bounds::u32());
            state.stack.push(overflow);
            state.stack.push(result);
            let _ = imm;
        }
        Instruction::U32OverflowingSub => {
            state.stack.pop();
            state.stack.pop();
            let underflow = state.make_derived(Bounds::Bool);
            let result = state.make_derived(Bounds::u32());
            state.stack.push(underflow);
            state.stack.push(result);
        }
        Instruction::U32OverflowingSubImm(imm) => {
            state.stack.pop();
            let underflow = state.make_derived(Bounds::Bool);
            let result = state.make_derived(Bounds::u32());
            state.stack.push(underflow);
            state.stack.push(result);
            let _ = imm;
        }
        Instruction::U32OverflowingMul => {
            state.stack.pop();
            state.stack.pop();
            let overflow = state.make_derived(Bounds::Bool);
            let result = state.make_derived(Bounds::u32());
            state.stack.push(overflow);
            state.stack.push(result);
        }
        Instruction::U32OverflowingMulImm(imm) => {
            state.stack.pop();
            let overflow = state.make_derived(Bounds::Bool);
            let result = state.make_derived(Bounds::u32());
            state.stack.push(overflow);
            state.stack.push(result);
            let _ = imm;
        }

        // u32 ternary operations - pop 3
        Instruction::U32OverflowingAdd3 => {
            // pop 3, push 2 (result + carry)
            state.stack.pop();
            state.stack.pop();
            state.stack.pop();
            let carry = state.make_derived(Bounds::Bool);
            let result = state.make_derived(Bounds::u32());
            state.stack.push(carry);
            state.stack.push(result);
        }
        Instruction::U32WrappingAdd3 => {
            // pop 3, push 1
            state.stack.pop();
            state.stack.pop();
            state.stack.pop();
            let result = state.make_derived(Bounds::u32());
            state.stack.push(result);
        }
        Instruction::U32OverflowingMadd => {
            // pop 3 (a, b, c), push 2 (a*b + c with overflow)
            state.stack.pop();
            state.stack.pop();
            state.stack.pop();
            let overflow = state.make_derived(Bounds::u32()); // Can overflow beyond bool
            let result = state.make_derived(Bounds::u32());
            state.stack.push(overflow);
            state.stack.push(result);
        }
        Instruction::U32WrappingMadd => {
            // pop 3 (a, b, c), push 1 (a*b + c wrapped)
            state.stack.pop();
            state.stack.pop();
            state.stack.pop();
            let result = state.make_derived(Bounds::u32());
            state.stack.push(result);
        }

        // u32 division - pop 2, push 1
        Instruction::U32Div => {
            state.stack.pop();
            state.stack.pop();
            let result = state.make_derived(Bounds::u32());
            state.stack.push(result);
        }
        Instruction::U32DivImm(imm) => {
            state.stack.pop();
            let result = state.make_derived(Bounds::u32());
            state.stack.push(result);
            let _ = imm;
        }
        Instruction::U32Mod => {
            state.stack.pop();
            state.stack.pop();
            let result = state.make_derived(Bounds::u32());
            state.stack.push(result);
        }
        Instruction::U32ModImm(imm) => {
            state.stack.pop();
            let result = state.make_derived(Bounds::u32());
            state.stack.push(result);
            let _ = imm;
        }
        Instruction::U32DivMod => {
            // pop 2, push 2 (quotient, remainder)
            state.stack.pop();
            state.stack.pop();
            let remainder = state.make_derived(Bounds::u32());
            let quotient = state.make_derived(Bounds::u32());
            state.stack.push(remainder);
            state.stack.push(quotient);
        }
        Instruction::U32DivModImm(imm) => {
            // pop 1, push 2 (quotient, remainder)
            state.stack.pop();
            let remainder = state.make_derived(Bounds::u32());
            let quotient = state.make_derived(Bounds::u32());
            state.stack.push(remainder);
            state.stack.push(quotient);
            let _ = imm;
        }

        // u32 bitwise - pop 2, push 1
        Instruction::U32And | Instruction::U32Or | Instruction::U32Xor => {
            state.stack.pop();
            state.stack.pop();
            let result = state.make_derived(Bounds::u32());
            state.stack.push(result);
        }
        // u32 bitwise not - pop 1, push 1
        Instruction::U32Not => {
            state.stack.pop();
            let result = state.make_derived(Bounds::u32());
            state.stack.push(result);
        }

        // u32 shift/rotate - pop 2, push 1 (or pop 1 for Imm variants)
        Instruction::U32Shr | Instruction::U32Shl | Instruction::U32Rotr | Instruction::U32Rotl => {
            state.stack.pop(); // shift amount
            state.stack.pop(); // value
            let result = state.make_derived(Bounds::u32());
            state.stack.push(result);
        }
        Instruction::U32ShrImm(imm)
        | Instruction::U32ShlImm(imm)
        | Instruction::U32RotrImm(imm)
        | Instruction::U32RotlImm(imm) => {
            state.stack.pop(); // value only, shift amount from immediate
            let result = state.make_derived(Bounds::u32());
            state.stack.push(result);
            let _ = imm;
        }

        // u32 bit counting - pop 1, push 1
        Instruction::U32Popcnt
        | Instruction::U32Ctz
        | Instruction::U32Clz
        | Instruction::U32Clo
        | Instruction::U32Cto => {
            state.stack.pop();
            let result = state.make_derived(Bounds::u32());
            state.stack.push(result);
        }

        // u32 comparison - pop 2, push 1 bool
        Instruction::U32Lt | Instruction::U32Lte | Instruction::U32Gt | Instruction::U32Gte => {
            state.stack.pop();
            state.stack.pop();
            let result = state.make_derived(Bounds::Bool);
            state.stack.push(result);
        }

        // u32 min/max - pop 2, push 1
        Instruction::U32Min | Instruction::U32Max => {
            state.stack.pop();
            state.stack.pop();
            let result = state.make_derived(Bounds::u32());
            state.stack.push(result);
        }

        // ─────────────────────────────────────────────────────────────────────
        // ext2 operations - extension field arithmetic (2 field elements)
        // ─────────────────────────────────────────────────────────────────────
        Instruction::Ext2Add | Instruction::Ext2Sub | Instruction::Ext2Mul | Instruction::Ext2Div => {
            // Binary ops: pop 4 (two ext2 elements), push 2 (one ext2 element)
            for _ in 0..4 {
                state.stack.pop();
            }
            let result1 = state.make_derived(Bounds::Field);
            let result2 = state.make_derived(Bounds::Field);
            state.stack.push(result1);
            state.stack.push(result2);
        }
        Instruction::Ext2Neg | Instruction::Ext2Inv => {
            // Unary ops: pop 2, push 2
            state.stack.pop();
            state.stack.pop();
            let result1 = state.make_derived(Bounds::Field);
            let result2 = state.make_derived(Bounds::Field);
            state.stack.push(result1);
            state.stack.push(result2);
        }

        // ─────────────────────────────────────────────────────────────────────
        // Cryptographic operations
        // ─────────────────────────────────────────────────────────────────────
        Instruction::Hash => {
            // Hash: pop 4 (word), push 4 (hash result)
            for _ in 0..4 {
                state.stack.pop();
            }
            for _ in 0..4 {
                let result = state.make_derived(Bounds::Field);
                state.stack.push(result);
            }
        }
        Instruction::HMerge => {
            // HMerge: pop 8 (two words), push 4 (merged hash)
            for _ in 0..8 {
                state.stack.pop();
            }
            for _ in 0..4 {
                let result = state.make_derived(Bounds::Field);
                state.stack.push(result);
            }
        }
        Instruction::HPerm => {
            // HPerm: pop 12, push 12 (permutation - same count)
            for _ in 0..12 {
                state.stack.pop();
            }
            for _ in 0..12 {
                let result = state.make_derived(Bounds::Field);
                state.stack.push(result);
            }
        }

        // ─────────────────────────────────────────────────────────────────────
        // Conditional operations
        // ─────────────────────────────────────────────────────────────────────
        Instruction::CSwap => {
            // Pop condition, conditionally swap top two elements
            // Stack: [c, b, a, ...] -> [b, a, ...] or [a, b, ...] based on c
            state.stack.pop(); // condition
            // Since we don't know if swap happens, keep elements but mark as unknown bounds
            if let Some(t) = state.stack.peek_mut(0) {
                t.bounds = Bounds::Field;
            }
            if let Some(t) = state.stack.peek_mut(1) {
                t.bounds = Bounds::Field;
            }
        }
        Instruction::CSwapW => {
            // Pop condition, conditionally swap top two words
            state.stack.pop(); // condition
            // Mark affected positions as unknown
            for i in 0..8 {
                if let Some(t) = state.stack.peek_mut(i) {
                    t.bounds = Bounds::Field;
                }
            }
        }
        Instruction::CDrop => {
            // Pop condition, conditionally drop one of top two elements
            // Stack: [c, b, a, ...] -> [a] if c=1, [b] if c=0
            state.stack.pop(); // condition
            state.stack.pop(); // one of the values
            // Remaining value has unknown bounds
            if let Some(t) = state.stack.peek_mut(0) {
                t.bounds = Bounds::Field;
            }
        }
        Instruction::CDropW => {
            // Pop condition, conditionally drop one of top two words
            state.stack.pop(); // condition
            for _ in 0..4 {
                state.stack.pop(); // one word
            }
            // Remaining word has unknown bounds
            for i in 0..4 {
                if let Some(t) = state.stack.peek_mut(i) {
                    t.bounds = Bounds::Field;
                }
            }
        }

        // ─────────────────────────────────────────────────────────────────────
        // Assertion operations - no stack change (trap on failure)
        // ─────────────────────────────────────────────────────────────────────
        Instruction::Assert | Instruction::AssertWithError(_) => {
            // Pop condition, no push
            state.stack.pop();
        }
        Instruction::AssertEq | Instruction::AssertEqWithError(_) => {
            // Pop two values to compare
            state.stack.pop();
            state.stack.pop();
        }
        Instruction::AssertEqw | Instruction::AssertEqwWithError(_) => {
            // Pop two words to compare
            for _ in 0..8 {
                state.stack.pop();
            }
        }
        Instruction::Assertz | Instruction::AssertzWithError(_) => {
            // Pop one value to check if zero
            state.stack.pop();
        }
        Instruction::Eqw => {
            // Compare two words, pop 8, push 1 bool
            for _ in 0..8 {
                state.stack.pop();
            }
            let result = state.make_derived(Bounds::Bool);
            state.stack.push(result);
        }

        // ─────────────────────────────────────────────────────────────────────
        // Input/environment operations - push trusted values
        // ─────────────────────────────────────────────────────────────────────
        Instruction::Sdepth => {
            // Push current stack depth (trusted, from VM)
            let value = state.make_derived(Bounds::u32());
            state.stack.push(value);
        }
        Instruction::Caller => {
            // Push caller hash (4 elements, trusted from VM)
            for _ in 0..4 {
                let value = state.make_derived(Bounds::Field);
                state.stack.push(value);
            }
        }
        Instruction::Clk => {
            // Push current clock cycle (trusted, from VM)
            let value = state.make_derived(Bounds::Field);
            state.stack.push(value);
        }
        Instruction::Locaddr(_) => {
            // Push local memory address (trusted literal)
            let value = state.make_derived(Bounds::u32());
            state.stack.push(value);
        }

        // ─────────────────────────────────────────────────────────────────────
        // Other math operations
        // ─────────────────────────────────────────────────────────────────────
        Instruction::ILog2 => {
            // Pop 1, push 1 (integer log base 2)
            state.stack.pop();
            let result = state.make_derived(Bounds::u32()); // Result is in [0, 63]
            state.stack.push(result);
        }
        Instruction::Pow2 => {
            // Pop 1 (exponent), push 1 (2^exp)
            state.stack.pop();
            let result = state.make_derived(Bounds::Field);
            state.stack.push(result);
        }
        Instruction::Exp => {
            // Pop 2 (base, exp), push 1 (base^exp)
            state.stack.pop();
            state.stack.pop();
            let result = state.make_derived(Bounds::Field);
            state.stack.push(result);
        }
        Instruction::ExpImm(_) | Instruction::ExpBitLength(_) => {
            // Pop 1, push 1
            state.stack.pop();
            let result = state.make_derived(Bounds::Field);
            state.stack.push(result);
        }

        // ─────────────────────────────────────────────────────────────────────
        // Dynamic procedure calls
        // ─────────────────────────────────────────────────────────────────────
        Instruction::DynExec | Instruction::DynCall => {
            // Dynamic calls consume the target hash (4 elements) and have unknown effects
            for _ in 0..4 {
                state.stack.pop();
            }
            // Clear tracking since we don't know the procedure's stack effect
            state.stack.clear();
        }
        Instruction::ProcRef(_) => {
            // Push procedure hash (4 elements)
            for _ in 0..4 {
                let value = state.make_derived(Bounds::Field);
                state.stack.push(value);
            }
        }

        // ─────────────────────────────────────────────────────────────────────
        // Debug/trace operations - no stack change
        // ─────────────────────────────────────────────────────────────────────
        Instruction::Breakpoint | Instruction::Debug(_) | Instruction::Nop => {
            // No stack effect
        }
        Instruction::Emit | Instruction::EmitImm(_) | Instruction::Trace(_) => {
            // No stack effect (these are side effects only)
        }

        // ─────────────────────────────────────────────────────────────────────
        // STARK proof verification operations
        // ─────────────────────────────────────────────────────────────────────
        Instruction::FriExt2Fold4 => {
            // Complex STARK operation - clear tracking to be safe
            state.stack.clear();
        }
        Instruction::HornerBase | Instruction::HornerExt | Instruction::EvalCircuit | Instruction::LogPrecompile => {
            // Complex operations with variable stack effects
            state.stack.clear();
        }

        // ─────────────────────────────────────────────────────────────────────
        // System event operations
        // ─────────────────────────────────────────────────────────────────────
        Instruction::SysEvent(_) => {
            // System events may have complex stack effects
            state.stack.clear();
        }

        // ─────────────────────────────────────────────────────────────────────
        // Default - unhandled instructions
        // ─────────────────────────────────────────────────────────────────────
        #[allow(unreachable_patterns)]
        _ => {
            // Other instructions not explicitly handled - stack unchanged
            // This is conservative: we may lose precision but won't cause false results
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Tests
// ═══════════════════════════════════════════════════════════════════════════

#[cfg(test)]
mod tests {
    use super::*;
    use miden_assembly_syntax::ast::ModuleKind;
    use miden_assembly_syntax::{Parse, ParseOptions};
    use miden_debug_types::{DefaultSourceManager, SourceLanguage, SourceManager, Uri};

    /// Helper to parse a procedure and return the analysis state after running
    fn analyze_proc(source: &str) -> AnalysisState {
        let source_manager = DefaultSourceManager::default();
        let uri = Uri::from("test://test.masm");
        let full_source = format!("proc test_proc\n{}\nend", source);
        source_manager.load(SourceLanguage::Masm, uri.clone(), full_source);

        let source_file = source_manager.get_by_uri(&uri).expect("Failed to load source");
        let mut module_path = miden_assembly_syntax::ast::PathBuf::default();
        module_path.push("test");
        let opts = ParseOptions {
            kind: ModuleKind::Library,
            path: Some(module_path.into()),
            ..Default::default()
        };
        let module = source_file
            .parse_with_options(&source_manager, opts)
            .expect("Failed to parse MASM");

        let mut state = AnalysisState::new("test_proc".to_string());

        // Walk the procedure body and apply effects
        use miden_assembly_syntax::ast::visit::{self, Visit};
        use std::ops::ControlFlow;

        struct EffectApplier<'a> {
            state: &'a mut AnalysisState,
        }

        impl<'a> Visit for EffectApplier<'a> {
            fn visit_inst(
                &mut self,
                inst: &miden_debug_types::Span<Instruction>,
            ) -> ControlFlow<()> {
                apply_effect(inst.inner(), self.state, inst.span());
                ControlFlow::Continue(())
            }
        }

        let mut applier = EffectApplier { state: &mut state };
        let _ = visit::visit_module(&mut applier, &module);

        state
    }

    // ═══════════════════════════════════════════════════════════════════════
    // P0: Push value extraction tests
    // ═══════════════════════════════════════════════════════════════════════

    #[test]
    fn test_push_extracts_actual_value() {
        let state = analyze_proc("push.42");

        assert_eq!(state.stack.depth(), 1);
        let top = state.stack.peek(0).unwrap();
        assert_eq!(top.bounds, Bounds::Const(42));
    }

    #[test]
    fn test_push_large_value() {
        let state = analyze_proc("push.1000000");

        assert_eq!(state.stack.depth(), 1);
        let top = state.stack.peek(0).unwrap();
        assert_eq!(top.bounds, Bounds::Const(1000000));
    }

    #[test]
    fn test_push_multiple_values() {
        let state = analyze_proc("push.10 push.20 push.30");

        assert_eq!(state.stack.depth(), 3);
        // Stack order: 30 is on top (position 0), 20 at position 1, 10 at position 2
        assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(30));
        assert_eq!(state.stack.peek(1).unwrap().bounds, Bounds::Const(20));
        assert_eq!(state.stack.peek(2).unwrap().bounds, Bounds::Const(10));
    }

    #[test]
    fn test_push_zero() {
        let state = analyze_proc("push.0");

        assert_eq!(state.stack.depth(), 1);
        let top = state.stack.peek(0).unwrap();
        assert_eq!(top.bounds, Bounds::Const(0));
    }

    #[test]
    fn test_push_u32_max() {
        let state = analyze_proc("push.4294967295"); // u32::MAX

        assert_eq!(state.stack.depth(), 1);
        let top = state.stack.peek(0).unwrap();
        assert_eq!(top.bounds, Bounds::Const(4294967295));
    }

    // ═══════════════════════════════════════════════════════════════════════
    // Stack manipulation tests
    // ═══════════════════════════════════════════════════════════════════════

    #[test]
    fn test_dup_preserves_bounds() {
        let state = analyze_proc("push.42 dup.0");

        assert_eq!(state.stack.depth(), 2);
        assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(42));
        assert_eq!(state.stack.peek(1).unwrap().bounds, Bounds::Const(42));
    }

    #[test]
    fn test_swap_preserves_bounds() {
        let state = analyze_proc("push.10 push.20 swap");

        assert_eq!(state.stack.depth(), 2);
        assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(10));
        assert_eq!(state.stack.peek(1).unwrap().bounds, Bounds::Const(20));
    }

    #[test]
    fn test_drop_removes_top() {
        let state = analyze_proc("push.10 push.20 drop");

        assert_eq!(state.stack.depth(), 1);
        assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(10));
    }

    #[test]
    fn test_movup_preserves_bounds() {
        let state = analyze_proc("push.1 push.2 push.3 movup.2");

        // After movup.2: [1, 3, 2] -> [1] moves to top
        assert_eq!(state.stack.depth(), 3);
        assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(1));
        assert_eq!(state.stack.peek(1).unwrap().bounds, Bounds::Const(3));
        assert_eq!(state.stack.peek(2).unwrap().bounds, Bounds::Const(2));
    }

    // ═══════════════════════════════════════════════════════════════════════
    // Advice operation tests
    // ═══════════════════════════════════════════════════════════════════════

    #[test]
    fn test_adv_push_creates_untrusted() {
        let state = analyze_proc("adv_push.1");

        assert_eq!(state.stack.depth(), 1);
        let top = state.stack.peek(0).unwrap();
        assert!(top.source.is_untrusted());
        assert!(!top.is_validated());
    }

    #[test]
    fn test_adv_push_multiple() {
        let state = analyze_proc("adv_push.3");

        assert_eq!(state.stack.depth(), 3);
        for i in 0..3 {
            let elem = state.stack.peek(i).unwrap();
            assert!(elem.source.is_untrusted());
        }
    }

    // ═══════════════════════════════════════════════════════════════════════
    // Validation tests
    // ═══════════════════════════════════════════════════════════════════════

    #[test]
    fn test_u32assert_validates_top() {
        let state = analyze_proc("adv_push.1 u32assert");

        assert_eq!(state.stack.depth(), 1);
        let top = state.stack.peek(0).unwrap();
        assert!(top.is_validated());
        assert!(top.bounds.is_u32());
    }

    #[test]
    fn test_u32assert2_validates_top_two() {
        let state = analyze_proc("adv_push.2 u32assert2");

        assert_eq!(state.stack.depth(), 2);
        assert!(state.stack.peek(0).unwrap().is_validated());
        assert!(state.stack.peek(1).unwrap().is_validated());
    }

    // ═══════════════════════════════════════════════════════════════════════
    // P0: u32 operation stack effect tests
    // ═══════════════════════════════════════════════════════════════════════

    #[test]
    fn test_u32_test_pushes_bool() {
        // u32test doesn't pop, just pushes a bool
        let state = analyze_proc("push.42 u32test");

        assert_eq!(state.stack.depth(), 2); // original value + bool result
        assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Bool);
        assert_eq!(state.stack.peek(1).unwrap().bounds, Bounds::Const(42));
    }

    #[test]
    fn test_u32_split_pop1_push2() {
        // u32split pops 1, pushes 2 (hi, lo)
        let state = analyze_proc("push.1 u32split");

        assert_eq!(state.stack.depth(), 2);
        assert!(state.stack.peek(0).unwrap().bounds.is_u32()); // hi
        assert!(state.stack.peek(1).unwrap().bounds.is_u32()); // lo
    }

    #[test]
    fn test_u32_wrapping_add_pop2_push1() {
        let state = analyze_proc("push.10 push.20 u32wrapping_add");

        assert_eq!(state.stack.depth(), 1);
        assert!(state.stack.peek(0).unwrap().bounds.is_u32());
    }

    #[test]
    fn test_u32_wrapping_add_imm_pop1_push1() {
        let state = analyze_proc("push.10 u32wrapping_add.5");

        assert_eq!(state.stack.depth(), 1);
        assert!(state.stack.peek(0).unwrap().bounds.is_u32());
    }

    #[test]
    fn test_u32_overflowing_add_pop2_push2() {
        // u32overflowing_add pops 2, pushes 2 (result, overflow)
        let state = analyze_proc("push.10 push.20 u32overflowing_add");

        assert_eq!(state.stack.depth(), 2);
        // Result on top (position 0), overflow flag below (position 1)
        assert!(state.stack.peek(0).unwrap().bounds.is_u32()); // result
        assert_eq!(state.stack.peek(1).unwrap().bounds, Bounds::Bool); // overflow
    }

    #[test]
    fn test_u32_overflowing_add3_pop3_push2() {
        // u32overflowing_add3 pops 3, pushes 2 (result, carry)
        let state = analyze_proc("push.1 push.2 push.3 u32overflowing_add3");

        assert_eq!(state.stack.depth(), 2);
        assert!(state.stack.peek(0).unwrap().bounds.is_u32()); // result
        assert_eq!(state.stack.peek(1).unwrap().bounds, Bounds::Bool); // carry
    }

    #[test]
    fn test_u32_wrapping_add3_pop3_push1() {
        // u32wrapping_add3 pops 3, pushes 1
        let state = analyze_proc("push.1 push.2 push.3 u32wrapping_add3");

        assert_eq!(state.stack.depth(), 1);
        assert!(state.stack.peek(0).unwrap().bounds.is_u32());
    }

    #[test]
    fn test_u32_divmod_pop2_push2() {
        // u32divmod pops 2, pushes 2 (quotient, remainder)
        let state = analyze_proc("push.10 push.3 u32divmod");

        assert_eq!(state.stack.depth(), 2);
        assert!(state.stack.peek(0).unwrap().bounds.is_u32()); // quotient
        assert!(state.stack.peek(1).unwrap().bounds.is_u32()); // remainder
    }

    #[test]
    fn test_u32_divmod_imm_pop1_push2() {
        // u32divmod.N pops 1, pushes 2
        let state = analyze_proc("push.10 u32divmod.3");

        assert_eq!(state.stack.depth(), 2);
        assert!(state.stack.peek(0).unwrap().bounds.is_u32()); // quotient
        assert!(state.stack.peek(1).unwrap().bounds.is_u32()); // remainder
    }

    #[test]
    fn test_u32_not_pop1_push1() {
        let state = analyze_proc("push.255 u32not");

        assert_eq!(state.stack.depth(), 1);
        assert!(state.stack.peek(0).unwrap().bounds.is_u32());
    }

    #[test]
    fn test_u32_shl_pop2_push1() {
        let state = analyze_proc("push.1 push.4 u32shl");

        assert_eq!(state.stack.depth(), 1);
        assert!(state.stack.peek(0).unwrap().bounds.is_u32());
    }

    #[test]
    fn test_u32_shl_imm_pop1_push1() {
        let state = analyze_proc("push.1 u32shl.4");

        assert_eq!(state.stack.depth(), 1);
        assert!(state.stack.peek(0).unwrap().bounds.is_u32());
    }

    #[test]
    fn test_u32_popcnt_pop1_push1() {
        let state = analyze_proc("push.255 u32popcnt");

        assert_eq!(state.stack.depth(), 1);
        assert!(state.stack.peek(0).unwrap().bounds.is_u32());
    }

    #[test]
    fn test_u32_lt_pop2_push_bool() {
        let state = analyze_proc("push.10 push.20 u32lt");

        assert_eq!(state.stack.depth(), 1);
        assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Bool);
    }

    #[test]
    fn test_u32_min_pop2_push1() {
        let state = analyze_proc("push.10 push.20 u32min");

        assert_eq!(state.stack.depth(), 1);
        assert!(state.stack.peek(0).unwrap().bounds.is_u32());
    }

    #[test]
    fn test_u32_cast_pop1_push1() {
        let state = analyze_proc("push.1000000000000 u32cast");

        assert_eq!(state.stack.depth(), 1);
        assert!(state.stack.peek(0).unwrap().bounds.is_u32());
    }

    // ═══════════════════════════════════════════════════════════════════════
    // P1: Word operations stack effect tests
    // ═══════════════════════════════════════════════════════════════════════

    #[test]
    fn test_mem_loadw_be_pop1_push4() {
        let state = analyze_proc("push.1000 mem_loadw_be");

        assert_eq!(state.stack.depth(), 4);
        // All 4 values should be from memory
        for i in 0..4 {
            assert!(matches!(
                state.stack.peek(i).unwrap().source,
                crate::analysis::types::Source::Memory
            ));
        }
    }

    #[test]
    fn test_mem_storew_be_pop5() {
        let state = analyze_proc("push.1 push.2 push.3 push.4 push.1000 mem_storew_be");

        // 5 values pushed, 5 popped (addr + 4 values)
        assert_eq!(state.stack.depth(), 0);
    }

    #[test]
    fn test_loc_load_push1() {
        let state = analyze_proc("loc_load.0");

        assert_eq!(state.stack.depth(), 1);
    }

    #[test]
    fn test_loc_loadw_be_push4() {
        let state = analyze_proc("loc_loadw_be.0");

        assert_eq!(state.stack.depth(), 4);
    }

    #[test]
    fn test_loc_store_pop1() {
        let state = analyze_proc("push.42 loc_store.0");

        assert_eq!(state.stack.depth(), 0);
    }

    #[test]
    fn test_loc_storew_be_pop4() {
        let state = analyze_proc("push.1 push.2 push.3 push.4 loc_storew_be.0");

        assert_eq!(state.stack.depth(), 0);
    }

    #[test]
    fn test_swapw_preserves_bounds() {
        // Push 8 values: [1,2,3,4] and [5,6,7,8]
        let state = analyze_proc(
            "push.1 push.2 push.3 push.4 push.5 push.6 push.7 push.8 swapw",
        );

        assert_eq!(state.stack.depth(), 8);
        // After swapw.1: word at 0-3 swapped with word at 4-7
        // Original: top=[8,7,6,5,4,3,2,1] (8 on top)
        // After: top=[4,3,2,1,8,7,6,5]
        assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(4));
        assert_eq!(state.stack.peek(1).unwrap().bounds, Bounds::Const(3));
        assert_eq!(state.stack.peek(2).unwrap().bounds, Bounds::Const(2));
        assert_eq!(state.stack.peek(3).unwrap().bounds, Bounds::Const(1));
        assert_eq!(state.stack.peek(4).unwrap().bounds, Bounds::Const(8));
        assert_eq!(state.stack.peek(5).unwrap().bounds, Bounds::Const(7));
    }

    #[test]
    fn test_reversew_reverses_word() {
        let state = analyze_proc("push.1 push.2 push.3 push.4 reversew");

        assert_eq!(state.stack.depth(), 4);
        // Original: [4,3,2,1] (4 on top)
        // After reversew: [1,2,3,4]
        assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(1));
        assert_eq!(state.stack.peek(1).unwrap().bounds, Bounds::Const(2));
        assert_eq!(state.stack.peek(2).unwrap().bounds, Bounds::Const(3));
        assert_eq!(state.stack.peek(3).unwrap().bounds, Bounds::Const(4));
    }

    #[test]
    fn test_dupw_pushes_4() {
        let state = analyze_proc("push.1 push.2 push.3 push.4 dupw.0");

        // dupw.0 duplicates the top word (positions 0-3)
        // Initial stack: [4, 3, 2, 1] (4 on top)
        // After dupw.0: [4, 3, 2, 1, 4, 3, 2, 1]
        assert_eq!(state.stack.depth(), 8);
        // Top word (duplicated): positions 0-3
        assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(4));
        assert_eq!(state.stack.peek(1).unwrap().bounds, Bounds::Const(3));
        assert_eq!(state.stack.peek(2).unwrap().bounds, Bounds::Const(2));
        assert_eq!(state.stack.peek(3).unwrap().bounds, Bounds::Const(1));
        // Original word: positions 4-7
        assert_eq!(state.stack.peek(4).unwrap().bounds, Bounds::Const(4));
        assert_eq!(state.stack.peek(5).unwrap().bounds, Bounds::Const(3));
        assert_eq!(state.stack.peek(6).unwrap().bounds, Bounds::Const(2));
        assert_eq!(state.stack.peek(7).unwrap().bounds, Bounds::Const(1));
    }

    #[test]
    fn test_dropw_pops_4() {
        let state = analyze_proc("push.1 push.2 push.3 push.4 push.5 dropw");

        // 5 pushed, 4 dropped = 1 remaining
        assert_eq!(state.stack.depth(), 1);
        assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(1));
    }

    #[test]
    fn test_padw_pushes_4_zeros() {
        let state = analyze_proc("padw");

        assert_eq!(state.stack.depth(), 4);
        for i in 0..4 {
            assert_eq!(state.stack.peek(i).unwrap().bounds, Bounds::Const(0));
        }
    }

    // ═══════════════════════════════════════════════════════════════════════
    // P1: Bounds propagation through arithmetic tests
    // ═══════════════════════════════════════════════════════════════════════

    #[test]
    fn test_add_propagates_const_bounds() {
        let state = analyze_proc("push.10 push.5 add");

        assert_eq!(state.stack.depth(), 1);
        // 10 + 5 = 15
        assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(15));
    }

    #[test]
    fn test_sub_propagates_const_bounds() {
        let state = analyze_proc("push.10 push.3 sub");

        assert_eq!(state.stack.depth(), 1);
        // 10 - 3 = 7
        assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(7));
    }

    #[test]
    fn test_mul_propagates_const_bounds() {
        let state = analyze_proc("push.7 push.6 mul");

        assert_eq!(state.stack.depth(), 1);
        // 7 * 6 = 42
        assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(42));
    }

    #[test]
    fn test_div_propagates_const_bounds() {
        let state = analyze_proc("push.42 push.6 div");

        assert_eq!(state.stack.depth(), 1);
        // 42 / 6 = 7
        assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(7));
    }

    #[test]
    fn test_add_imm_propagates_bounds() {
        let state = analyze_proc("push.10 add.5");

        assert_eq!(state.stack.depth(), 1);
        // 10 + 5 = 15
        assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(15));
    }

    #[test]
    fn test_sub_imm_propagates_bounds() {
        let state = analyze_proc("push.10 sub.3");

        assert_eq!(state.stack.depth(), 1);
        // 10 - 3 = 7
        assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(7));
    }

    #[test]
    fn test_incr_via_add1_propagates_bounds() {
        // incr is add.1 in Miden syntax
        let state = analyze_proc("push.41 add.1");

        assert_eq!(state.stack.depth(), 1);
        // 41 + 1 = 42
        assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(42));
    }

    #[test]
    fn test_chained_arithmetic_propagates() {
        // Test: (5 + 3) * 2 = 16
        let state = analyze_proc("push.5 push.3 add push.2 mul");

        assert_eq!(state.stack.depth(), 1);
        assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(16));
    }

    #[test]
    fn test_eq_const_produces_bool() {
        let state = analyze_proc("push.5 push.5 eq");

        assert_eq!(state.stack.depth(), 1);
        // 5 == 5 → 1
        assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(1));
    }

    #[test]
    fn test_neq_const_produces_bool() {
        let state = analyze_proc("push.5 push.3 neq");

        assert_eq!(state.stack.depth(), 1);
        // 5 != 3 → 1
        assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(1));
    }

    #[test]
    fn test_lt_const_produces_bool() {
        let state = analyze_proc("push.3 push.5 lt");

        assert_eq!(state.stack.depth(), 1);
        // 3 < 5 → 1
        assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(1));
    }

    // ═══════════════════════════════════════════════════════════════════════
    // P2/P3: Additional instruction tests
    // ═══════════════════════════════════════════════════════════════════════

    #[test]
    fn test_hash_pop4_push4() {
        let state = analyze_proc("push.1 push.2 push.3 push.4 hash");

        assert_eq!(state.stack.depth(), 4);
    }

    #[test]
    fn test_hmerge_pop8_push4() {
        let state = analyze_proc(
            "push.1 push.2 push.3 push.4 push.5 push.6 push.7 push.8 hmerge",
        );

        assert_eq!(state.stack.depth(), 4);
    }

    #[test]
    fn test_sdepth_pushes_u32() {
        let state = analyze_proc("sdepth");

        assert_eq!(state.stack.depth(), 1);
        assert!(state.stack.peek(0).unwrap().bounds.is_u32());
    }

    #[test]
    fn test_assert_pops_condition() {
        let state = analyze_proc("push.1 assert");

        assert_eq!(state.stack.depth(), 0);
    }

    #[test]
    fn test_assertz_pops_value() {
        let state = analyze_proc("push.0 assertz");

        assert_eq!(state.stack.depth(), 0);
    }

    #[test]
    fn test_eqw_pop8_push_bool() {
        let state = analyze_proc(
            "push.1 push.2 push.3 push.4 push.1 push.2 push.3 push.4 eqw",
        );

        assert_eq!(state.stack.depth(), 1);
        assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Bool);
    }

    #[test]
    fn test_nop_no_effect() {
        let state = analyze_proc("push.42 nop");

        assert_eq!(state.stack.depth(), 1);
        assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(42));
    }

    #[test]
    fn test_pow2_pop1_push1() {
        let state = analyze_proc("push.5 pow2");

        assert_eq!(state.stack.depth(), 1);
        // pow2 result is in Field range
        assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Field);
    }

    #[test]
    fn test_ilog2_pop1_push_u32() {
        let state = analyze_proc("push.256 ilog2");

        assert_eq!(state.stack.depth(), 1);
        assert!(state.stack.peek(0).unwrap().bounds.is_u32());
    }
}
