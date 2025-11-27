//! Stack effect handling for instructions.
//!
//! This module defines how each instruction affects the symbolic stack,
//! including pushing/popping values and applying validation.

use miden_assembly_syntax::ast::{Immediate, Instruction};
use miden_debug_types::SourceSpan;

use super::checker::is_u32_op;
use super::types::{AnalysisState, Bounds};

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
        // Validation assertions
        // ─────────────────────────────────────────────────────────────────────
        Instruction::U32Assert => {
            if let Some(t) = state.stack.peek_mut(0) {
                t.apply_validation();
            }
        }

        Instruction::U32Assert2 => {
            if let Some(t) = state.stack.peek_mut(0) {
                t.apply_validation();
            }
            if let Some(t) = state.stack.peek_mut(1) {
                t.apply_validation();
            }
        }

        Instruction::U32AssertW => {
            for i in 0..4 {
                if let Some(t) = state.stack.peek_mut(i) {
                    t.apply_validation();
                }
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
        Instruction::Push(_) => {
            // Push instructions push trusted literal values
            let taint = state.make_literal(0, Some(span));
            state.stack.push(taint);
        }

        Instruction::PushFeltList(values) => {
            for _ in values {
                let taint = state.make_literal(0, Some(span));
                state.stack.push(taint);
            }
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
            for i in (0..4).rev() {
                state.stack.dup(i);
            }
        }
        Instruction::DupW1 => {
            for i in (4..8).rev() {
                state.stack.dup(i);
            }
        }
        Instruction::DupW2 => {
            for i in (8..12).rev() {
                state.stack.dup(i);
            }
        }
        Instruction::DupW3 => {
            for i in (12..16).rev() {
                state.stack.dup(i);
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
        // Arithmetic - results are derived values
        // ─────────────────────────────────────────────────────────────────────
        Instruction::Add | Instruction::AddImm(_) => {
            state.stack.pop();
            state.stack.pop();
            let result = state.make_derived(Bounds::Field);
            state.stack.push(result);
        }

        Instruction::Sub | Instruction::SubImm(_) => {
            state.stack.pop();
            state.stack.pop();
            let result = state.make_derived(Bounds::Field);
            state.stack.push(result);
        }

        Instruction::Mul | Instruction::MulImm(_) => {
            state.stack.pop();
            state.stack.pop();
            let result = state.make_derived(Bounds::Field);
            state.stack.push(result);
        }

        Instruction::Div | Instruction::DivImm(_) => {
            state.stack.pop();
            state.stack.pop();
            let result = state.make_derived(Bounds::Field);
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

        // ─────────────────────────────────────────────────────────────────────
        // Comparison operations - results are boolean
        // ─────────────────────────────────────────────────────────────────────
        Instruction::Eq | Instruction::EqImm(_) => {
            state.stack.pop();
            state.stack.pop();
            let result = state.make_derived(Bounds::Bool);
            state.stack.push(result);
        }

        Instruction::Neq | Instruction::NeqImm(_) => {
            state.stack.pop();
            state.stack.pop();
            let result = state.make_derived(Bounds::Bool);
            state.stack.push(result);
        }

        Instruction::Lt => {
            state.stack.pop();
            state.stack.pop();
            let result = state.make_derived(Bounds::Bool);
            state.stack.push(result);
        }

        Instruction::Lte => {
            state.stack.pop();
            state.stack.pop();
            let result = state.make_derived(Bounds::Bool);
            state.stack.push(result);
        }

        Instruction::Gt => {
            state.stack.pop();
            state.stack.pop();
            let result = state.make_derived(Bounds::Bool);
            state.stack.push(result);
        }

        Instruction::Gte => {
            state.stack.pop();
            state.stack.pop();
            let result = state.make_derived(Bounds::Bool);
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
        // Memory operations
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
        // Procedure calls - conservatively clear stack tracking
        // ─────────────────────────────────────────────────────────────────────
        Instruction::Exec(_) | Instruction::Call(_) | Instruction::SysCall(_) => {
            // Procedure calls have complex stack effects that depend on the procedure
            // For now, we don't modify the stack - checkers handle the warnings
        }

        // ─────────────────────────────────────────────────────────────────────
        // Default - handle u32 operations generically
        // ─────────────────────────────────────────────────────────────────────
        _ => {
            if is_u32_op(inst) {
                // Most u32 ops are binary: pop 2, push 1 u32 result
                state.stack.pop();
                state.stack.pop();
                let result = state.make_derived(Bounds::u32());
                state.stack.push(result);
            }
            // Other instructions not explicitly handled - stack unchanged
        }
    }
}
