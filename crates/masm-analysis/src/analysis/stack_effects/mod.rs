//! Stack effect handling for instructions.
//!
//! This module defines how each instruction affects the symbolic stack,
//! including pushing/popping values and applying validation.

use miden_assembly_syntax::ast::Instruction;
use miden_debug_types::SourceSpan;

use super::dispatcher::{actions_for, Action, SourceKind};
use super::types::{AnalysisState, Bounds, TrackedValue};
use super::utils::{felt_imm_to_u64, push_imm_to_u64};
use masm_instructions::{apply_stack_manipulation, StackLike, StackLikeResult};

fn pop_n(state: &mut AnalysisState, n: usize) {
    for _ in 0..n {
        state.stack.pop();
    }
}

fn collect_pops_into(target: &mut Vec<Option<TrackedValue>>, state: &mut AnalysisState, n: usize) {
    for _ in 0..n {
        target.push(state.stack.pop());
    }
}

fn apply_pop_push_actions<FPop, FPush>(
    state: &mut AnalysisState,
    actions: &[Action],
    mut on_pop: FPop,
    mut on_push: FPush,
) where
    FPop: FnMut(&mut AnalysisState, usize),
    FPush: FnMut(&mut AnalysisState, usize, SourceKind),
{
    for action in actions {
        match action {
            Action::Pop(n) => on_pop(state, *n),
            Action::Push { count, kind } => on_push(state, *count, kind.clone()),
            Action::Stack(_) | Action::Unknown => {}
        }
    }
}

fn push_n_with<F>(state: &mut AnalysisState, n: usize, mut f: F)
where
    F: FnMut(&mut AnalysisState) -> TrackedValue,
{
    for _ in 0..n {
        let value = f(state);
        state.stack.push(value);
    }
}

fn push_derived_with_bounds(state: &mut AnalysisState, n: usize, bounds: Bounds) {
    push_n_with(state, n, |state| state.make_derived(bounds.clone()));
}

fn collect_popped_values(
    state: &mut AnalysisState,
    actions: &[Action],
) -> Vec<Option<TrackedValue>> {
    let mut popped = Vec::new();
    for action in actions {
        if let Action::Pop(n) = action {
            collect_pops_into(&mut popped, state, *n);
        }
    }
    popped
}

fn push_from_actions<F>(state: &mut AnalysisState, actions: &[Action], mut f: F)
where
    F: FnMut(&mut AnalysisState, usize, SourceKind),
{
    for action in actions {
        if let Action::Push { count, kind } = action {
            f(state, *count, kind.clone());
        }
    }
}

fn apply_binary_bounds<F>(state: &mut AnalysisState, actions: &[Action], compute: F)
where
    F: Fn(Option<&TrackedValue>, Option<&TrackedValue>) -> Bounds,
{
    let popped = collect_popped_values(state, actions);
    let a = popped.get(1).and_then(|v| v.as_ref());
    let b = popped.get(0).and_then(|v| v.as_ref());
    let new_bounds = compute(a, b);
    push_from_actions(state, actions, |state, n, _| {
        push_derived_with_bounds(state, n, new_bounds.clone())
    });
}

fn apply_unary_bounds<F>(state: &mut AnalysisState, actions: &[Action], compute: F)
where
    F: Fn(Option<&TrackedValue>) -> Bounds,
{
    let popped = collect_popped_values(state, actions);
    let a = popped.get(0).and_then(|v| v.as_ref());
    let new_bounds = compute(a);
    push_from_actions(state, actions, |state, n, _| {
        push_derived_with_bounds(state, n, new_bounds.clone())
    });
}

/// Apply an instruction's effect on the analysis state.
///
/// This updates the symbolic stack to reflect what values would be
/// present after the instruction executes.
pub fn apply_effect(inst: &Instruction, state: &mut AnalysisState, span: SourceSpan) {
    if apply_stack_manipulation(&mut state.stack, inst) == StackLikeResult::Applied {
        return;
    }

    let actions = actions_for(inst);

    match inst {
        // ─────────────────────────────────────────────────────────────────────
        // Merkle operations
        // ─────────────────────────────────────────────────────────────────────
        Instruction::MTreeGet => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_n_with(state, n, |state| state.make_merkle(span))
            });
        }

        Instruction::MTreeSet => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_n_with(state, n, |state| state.make_derived(Bounds::Field))
            });
        }

        Instruction::MTreeMerge => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_n_with(state, n, |state| state.make_derived(Bounds::Field))
            });
        }

        Instruction::MTreeVerify | Instruction::MTreeVerifyWithError(_) => {
            // Verification only - no stack change (may trap on failure)
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
        // Stack manipulation - Pad
        // ─────────────────────────────────────────────────────────────────────
        Instruction::PadW => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_n_with(state, n, |state| state.make_literal(0, None))
            });
        }

        // ─────────────────────────────────────────────────────────────────────
        // Arithmetic - propagate bounds through operations
        // ─────────────────────────────────────────────────────────────────────
        Instruction::Add => apply_binary_bounds(state, &actions, |a, b| match (a, b) {
            (Some(a), Some(b)) => a.bounds.add(&b.bounds),
            _ => Bounds::Field,
        }),
        Instruction::AddImm(imm) => apply_unary_bounds(state, &actions, |a| match a {
            Some(a) => a
                .bounds
                .add(&Bounds::Const(felt_imm_to_u64(imm).unwrap_or(0))),
            _ => Bounds::Field,
        }),

        Instruction::Sub => apply_binary_bounds(state, &actions, |a, b| match (a, b) {
            (Some(a), Some(b)) => a.bounds.sub(&b.bounds),
            _ => Bounds::Field,
        }),
        Instruction::SubImm(imm) => apply_unary_bounds(state, &actions, |a| match a {
            Some(a) => a
                .bounds
                .sub(&Bounds::Const(felt_imm_to_u64(imm).unwrap_or(0))),
            _ => Bounds::Field,
        }),

        Instruction::Mul => apply_binary_bounds(state, &actions, |a, b| match (a, b) {
            (Some(a), Some(b)) => a.bounds.mul(&b.bounds),
            _ => Bounds::Field,
        }),
        Instruction::MulImm(imm) => apply_unary_bounds(state, &actions, |a| match a {
            Some(a) => a
                .bounds
                .mul(&Bounds::Const(felt_imm_to_u64(imm).unwrap_or(0))),
            _ => Bounds::Field,
        }),

        Instruction::Div => apply_binary_bounds(state, &actions, |a, b| match (a, b) {
            (Some(a), Some(b)) => a.bounds.div(&b.bounds),
            _ => Bounds::Field,
        }),
        Instruction::DivImm(imm) => apply_unary_bounds(state, &actions, |a| match a {
            Some(a) => {
                let imm_val = felt_imm_to_u64(imm).unwrap_or(0);
                if imm_val != 0 {
                    a.bounds.div(&Bounds::Const(imm_val))
                } else {
                    Bounds::Field
                }
            }
            _ => Bounds::Field,
        }),

        Instruction::Neg => apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
            push_derived_with_bounds(state, n, Bounds::Field)
        }),

        Instruction::Inv => apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
            push_derived_with_bounds(state, n, Bounds::Field)
        }),

        Instruction::Incr => apply_unary_bounds(state, &actions, |a| match a {
            Some(a) => a.bounds.incr(),
            _ => Bounds::Field,
        }),

        // ─────────────────────────────────────────────────────────────────────
        // Comparison operations - propagate bounds through comparisons
        // ─────────────────────────────────────────────────────────────────────
        Instruction::Eq | Instruction::Neq | Instruction::Lt | Instruction::Lte => {
            apply_binary_bounds(state, &actions, |a, b| match (a, b) {
                (Some(a), Some(b)) => match inst {
                    Instruction::Eq => a.bounds.eq(&b.bounds),
                    Instruction::Neq => a.bounds.neq(&b.bounds),
                    Instruction::Lt => a.bounds.lt(&b.bounds),
                    Instruction::Lte => a.bounds.lte(&b.bounds),
                    _ => Bounds::Bool,
                },
                _ => Bounds::Bool,
            });
        }
        Instruction::EqImm(imm) | Instruction::NeqImm(imm) => {
            let imm_val = felt_imm_to_u64(imm).unwrap_or(0);
            apply_unary_bounds(state, &actions, |a| match a {
                Some(a) => match inst {
                    Instruction::EqImm(_) => a.bounds.eq(&Bounds::Const(imm_val)),
                    Instruction::NeqImm(_) => a.bounds.neq(&Bounds::Const(imm_val)),
                    _ => Bounds::Bool,
                },
                _ => Bounds::Bool,
            });
        }

        Instruction::Gt => apply_binary_bounds(state, &actions, |a, b| match (a, b) {
            (Some(a), Some(b)) => a.bounds.gt(&b.bounds),
            _ => Bounds::Bool,
        }),

        Instruction::Gte => apply_binary_bounds(state, &actions, |a, b| match (a, b) {
            (Some(a), Some(b)) => a.bounds.gte(&b.bounds),
            _ => Bounds::Bool,
        }),

        Instruction::IsOdd => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_derived_with_bounds(state, n, Bounds::Bool)
            });
        }

        // ─────────────────────────────────────────────────────────────────────
        // Boolean operations
        // ─────────────────────────────────────────────────────────────────────
        Instruction::And | Instruction::Or | Instruction::Xor => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_derived_with_bounds(state, n, Bounds::Bool)
            });
        }

        Instruction::Not => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_derived_with_bounds(state, n, Bounds::Bool)
            });
        }

        // ─────────────────────────────────────────────────────────────────────
        // Memory operations - single element
        // ─────────────────────────────────────────────────────────────────────
        Instruction::MemLoad => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_n_with(state, n, |state| state.make_memory(span))
            });
        }
        Instruction::MemLoadImm(_) => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_n_with(state, n, |state| state.make_memory(span))
            });
        }

        Instruction::MemStore => {
            apply_pop_push_actions(state, &actions, pop_n, |_state, _, _| {});
        }
        Instruction::MemStoreImm(_) => {
            apply_pop_push_actions(state, &actions, pop_n, |_state, _, _| {});
        }

        // ─────────────────────────────────────────────────────────────────────
        // Memory operations - word level (4 elements)
        // ─────────────────────────────────────────────────────────────────────
        Instruction::MemLoadWBe | Instruction::MemLoadWLe => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_n_with(state, n, |state| state.make_memory(span))
            });
        }
        Instruction::MemLoadWBeImm(_) | Instruction::MemLoadWLeImm(_) => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_n_with(state, n, |state| state.make_memory(span))
            });
        }

        Instruction::MemStoreWBe | Instruction::MemStoreWLe => {
            apply_pop_push_actions(state, &actions, pop_n, |_state, _, _| {});
        }
        Instruction::MemStoreWBeImm(_) | Instruction::MemStoreWLeImm(_) => {
            apply_pop_push_actions(state, &actions, pop_n, |_state, _, _| {});
        }

        // ─────────────────────────────────────────────────────────────────────
        // Local memory operations
        // ─────────────────────────────────────────────────────────────────────
        Instruction::LocLoad(_) => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_n_with(state, n, |state| state.make_memory(span))
            });
        }
        Instruction::LocLoadWBe(_) | Instruction::LocLoadWLe(_) => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_n_with(state, n, |state| state.make_memory(span))
            });
        }

        Instruction::LocStore(_) => {
            apply_pop_push_actions(state, &actions, pop_n, |_state, _, _| {});
        }
        Instruction::LocStoreWBe(_) | Instruction::LocStoreWLe(_) => {
            apply_pop_push_actions(state, &actions, pop_n, |_state, _, _| {});
        }

        // ─────────────────────────────────────────────────────────────────────
        // Memory streaming
        // ─────────────────────────────────────────────────────────────────────
        Instruction::MemStream => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_n_with(state, n, |state| state.make_memory(span))
            });
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
            for i in 0..2 {
                if let Some(t) = state.stack.peek_mut(i) {
                    t.apply_validation();
                }
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
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                if n == 2 {
                    let lo = state.make_derived(Bounds::u32());
                    let hi = state.make_derived(Bounds::u32());
                    state.stack.push(lo);
                    state.stack.push(hi);
                } else {
                    push_n_with(state, n, |state| state.make_derived(Bounds::u32()));
                }
            });
        }
        Instruction::U32Cast => {
            // pop 1, push 1 u32 (truncate to u32)
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_n_with(state, n, |state| state.make_derived(Bounds::u32()))
            });
        }

        // u32 binary wrapping arithmetic - pop 2, push 1
        Instruction::U32WrappingAdd => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_derived_with_bounds(state, n, Bounds::u32())
            });
        }
        Instruction::U32WrappingAddImm(imm) => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_derived_with_bounds(state, n, Bounds::u32())
            });
            let _ = imm;
        }
        Instruction::U32WrappingSub => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_derived_with_bounds(state, n, Bounds::u32())
            });
        }
        Instruction::U32WrappingSubImm(imm) => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_derived_with_bounds(state, n, Bounds::u32())
            });
            let _ = imm;
        }
        Instruction::U32WrappingMul => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_derived_with_bounds(state, n, Bounds::u32())
            });
        }
        Instruction::U32WrappingMulImm(imm) => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_derived_with_bounds(state, n, Bounds::u32())
            });
            let _ = imm;
        }

        // u32 binary overflowing arithmetic - pop 2, push 2 (result + overflow flag)
        Instruction::U32OverflowingAdd => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                if n == 2 {
                    let result = state.make_derived(Bounds::u32());
                    state.stack.push(result);
                    let overflow = state.make_derived(Bounds::Bool);
                    state.stack.push(overflow);
                } else {
                    push_derived_with_bounds(state, n, Bounds::u32());
                }
            });
        }
        Instruction::U32OverflowingAddImm(imm) => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                if n == 2 {
                    let result = state.make_derived(Bounds::u32());
                    state.stack.push(result);
                    let overflow = state.make_derived(Bounds::Bool);
                    state.stack.push(overflow);
                } else {
                    push_derived_with_bounds(state, n, Bounds::u32());
                }
            });
            let _ = imm;
        }
        Instruction::U32OverflowingSub => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                if n == 2 {
                    let result = state.make_derived(Bounds::u32());
                    state.stack.push(result);
                    let underflow = state.make_derived(Bounds::Bool);
                    state.stack.push(underflow);
                } else {
                    push_derived_with_bounds(state, n, Bounds::u32());
                }
            });
        }
        Instruction::U32OverflowingSubImm(imm) => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                if n == 2 {
                    let result = state.make_derived(Bounds::u32());
                    state.stack.push(result);
                    let underflow = state.make_derived(Bounds::Bool);
                    state.stack.push(underflow);
                } else {
                    push_derived_with_bounds(state, n, Bounds::u32());
                }
            });
            let _ = imm;
        }
        Instruction::U32OverflowingMul => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                if n == 2 {
                    let result = state.make_derived(Bounds::u32());
                    state.stack.push(result);
                    let overflow = state.make_derived(Bounds::u32());
                    state.stack.push(overflow);
                } else {
                    push_derived_with_bounds(state, n, Bounds::u32());
                }
            });
        }
        Instruction::U32OverflowingMulImm(imm) => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                if n == 2 {
                    let result = state.make_derived(Bounds::u32());
                    state.stack.push(result);
                    let overflow = state.make_derived(Bounds::u32());
                    state.stack.push(overflow);
                } else {
                    push_derived_with_bounds(state, n, Bounds::u32());
                }
            });
            let _ = imm;
        }

        // u32 ternary operations - pop 3
        Instruction::U32OverflowingAdd3 => {
            // pop 3, push 2 (result + carry)
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                if n == 2 {
                    let result = state.make_derived(Bounds::u32());
                    state.stack.push(result);
                    let carry = state.make_derived(Bounds::u32());
                    state.stack.push(carry);
                } else {
                    push_derived_with_bounds(state, n, Bounds::u32());
                }
            });
        }
        Instruction::U32WrappingAdd3 => {
            // pop 3, push 1
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_derived_with_bounds(state, n, Bounds::u32())
            });
        }
        Instruction::U32OverflowingMadd => {
            // pop 3 (a, b, c), push 2 (a*b + c with overflow)
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                if n == 2 {
                    let result = state.make_derived(Bounds::u32());
                    state.stack.push(result);
                    let overflow = state.make_derived(Bounds::u32()); // Can overflow beyond bool
                    state.stack.push(overflow);
                } else {
                    push_derived_with_bounds(state, n, Bounds::u32());
                }
            });
        }
        Instruction::U32WrappingMadd => {
            // pop 3 (a, b, c), push 1 (a*b + c wrapped)
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_derived_with_bounds(state, n, Bounds::u32())
            });
        }

        // u32 division - pop 2, push 1
        Instruction::U32Div => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_n_with(state, n, |state| state.make_derived(Bounds::u32()))
            });
        }
        Instruction::U32DivImm(imm) => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_n_with(state, n, |state| state.make_derived(Bounds::u32()))
            });
            let _ = imm;
        }
        Instruction::U32Mod => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_n_with(state, n, |state| state.make_derived(Bounds::u32()))
            });
        }
        Instruction::U32ModImm(imm) => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_n_with(state, n, |state| state.make_derived(Bounds::u32()))
            });
            let _ = imm;
        }
        Instruction::U32DivMod => {
            // pop 2, push 2 (quotient, remainder)
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                if n == 2 {
                    let quotient = state.make_derived(Bounds::u32());
                    state.stack.push(quotient);
                    let remainder = state.make_derived(Bounds::u32());
                    state.stack.push(remainder);
                } else {
                    push_n_with(state, n, |state| state.make_derived(Bounds::u32()));
                }
            });
        }
        Instruction::U32DivModImm(imm) => {
            // pop 1, push 2 (quotient, remainder)
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                if n == 2 {
                    let quotient = state.make_derived(Bounds::u32());
                    state.stack.push(quotient);
                    let remainder = state.make_derived(Bounds::u32());
                    state.stack.push(remainder);
                } else {
                    push_n_with(state, n, |state| state.make_derived(Bounds::u32()));
                }
            });
            let _ = imm;
        }

        // u32 bitwise - pop 2, push 1
        Instruction::U32And | Instruction::U32Or | Instruction::U32Xor => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_derived_with_bounds(state, n, Bounds::u32())
            });
        }
        // u32 bitwise not - pop 1, push 1
        Instruction::U32Not => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_derived_with_bounds(state, n, Bounds::u32())
            });
        }

        // u32 shift/rotate - pop 2, push 1 (or pop 1 for Imm variants)
        Instruction::U32Shr | Instruction::U32Shl | Instruction::U32Rotr | Instruction::U32Rotl => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_derived_with_bounds(state, n, Bounds::u32())
            });
        }
        Instruction::U32ShrImm(imm)
        | Instruction::U32ShlImm(imm)
        | Instruction::U32RotrImm(imm)
        | Instruction::U32RotlImm(imm) => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_derived_with_bounds(state, n, Bounds::u32())
            });
            let _ = imm;
        }

        // u32 bit counting - pop 1, push 1
        Instruction::U32Popcnt
        | Instruction::U32Ctz
        | Instruction::U32Clz
        | Instruction::U32Clo
        | Instruction::U32Cto => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_derived_with_bounds(state, n, Bounds::u32())
            });
        }

        // u32 comparison - pop 2, push 1 bool
        Instruction::U32Lt | Instruction::U32Lte | Instruction::U32Gt | Instruction::U32Gte => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_derived_with_bounds(state, n, Bounds::Bool)
            });
        }

        // u32 min/max - pop 2, push 1
        Instruction::U32Min | Instruction::U32Max => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_derived_with_bounds(state, n, Bounds::u32())
            });
        }

        // ─────────────────────────────────────────────────────────────────────
        // ext2 operations - extension field arithmetic (2 field elements)
        // ─────────────────────────────────────────────────────────────────────
        Instruction::Ext2Add
        | Instruction::Ext2Sub
        | Instruction::Ext2Mul
        | Instruction::Ext2Div => {
            // Binary ops: pop 4 (two ext2 elements), push 2 (one ext2 element)
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_n_with(state, n, |state| state.make_derived(Bounds::Field))
            });
        }
        Instruction::Ext2Neg | Instruction::Ext2Inv => {
            // Unary ops: pop 2, push 2
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_n_with(state, n, |state| state.make_derived(Bounds::Field))
            });
        }

        // ─────────────────────────────────────────────────────────────────────
        // Cryptographic operations
        // ─────────────────────────────────────────────────────────────────────
        Instruction::Hash => {
            // Hash: pop 4 (word), push 4 (hash result)
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_n_with(state, n, |state| state.make_derived(Bounds::Field))
            });
        }
        Instruction::HMerge => {
            // HMerge: pop 8 (two words), push 4 (merged hash)
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_n_with(state, n, |state| state.make_derived(Bounds::Field))
            });
        }
        Instruction::HPerm => {
            // HPerm: pop 12, push 12 (permutation - same count)
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_n_with(state, n, |state| state.make_derived(Bounds::Field))
            });
        }

        // ─────────────────────────────────────────────────────────────────────
        // Conditional operations
        // ─────────────────────────────────────────────────────────────────────
        Instruction::CSwap => {
            // Conditionals can reorder or drop; conservatively reset bounds.
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_derived_with_bounds(state, n, Bounds::Field)
            });
        }
        Instruction::CSwapW => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_derived_with_bounds(state, n, Bounds::Field)
            });
        }
        Instruction::CDrop => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_derived_with_bounds(state, n, Bounds::Field)
            });
        }
        Instruction::CDropW => {
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_derived_with_bounds(state, n, Bounds::Field)
            });
        }

        // ─────────────────────────────────────────────────────────────────────
        // Assertion operations - no stack change (trap on failure)
        // ─────────────────────────────────────────────────────────────────────
        Instruction::Assert | Instruction::AssertWithError(_) => {
            // Pop condition, no push
            apply_pop_push_actions(state, &actions, pop_n, |_state, _, _| {});
        }
        Instruction::AssertEq | Instruction::AssertEqWithError(_) => {
            // Pop two values to compare
            apply_pop_push_actions(state, &actions, pop_n, |_state, _, _| {});
        }
        Instruction::AssertEqw | Instruction::AssertEqwWithError(_) => {
            // Pop two words to compare
            apply_pop_push_actions(state, &actions, pop_n, |_state, _, _| {});
        }
        Instruction::Assertz | Instruction::AssertzWithError(_) => {
            // Pop one value to check if zero
            apply_pop_push_actions(state, &actions, pop_n, |_state, _, _| {});
        }
        Instruction::Eqw => {
            // Compare two words, push equality flag without consuming operands
            state.stack.ensure_depth(8);
            let result = state.make_derived(Bounds::Bool);
            state.stack.push(result);
        }

        // ─────────────────────────────────────────────────────────────────────
        // Input/environment operations - push trusted values
        // ─────────────────────────────────────────────────────────────────────
        Instruction::Sdepth => {
            // Push current stack depth (trusted, from VM)
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_n_with(state, n, |state| state.make_derived(Bounds::u32()))
            });
        }
        Instruction::Caller => {
            // Overwrite top word with caller hash (trusted)
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_n_with(state, n, |state| state.make_derived(Bounds::Field))
            });
        }
        Instruction::Clk => {
            // Push current clock cycle (trusted, from VM)
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_n_with(state, n, |state| state.make_derived(Bounds::Field))
            });
        }
        Instruction::Locaddr(_) => {
            // Push local memory address (trusted literal)
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_n_with(state, n, |state| state.make_derived(Bounds::u32()))
            });
        }

        // ─────────────────────────────────────────────────────────────────────
        // Other math operations
        // ─────────────────────────────────────────────────────────────────────
        Instruction::ILog2 => {
            // Pop 1, push 1 (integer log base 2)
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_n_with(state, n, |state| state.make_derived(Bounds::u32()))
            });
        }
        Instruction::Pow2 => {
            // Pop 1 (exponent), push 1 (2^exp)
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_n_with(state, n, |state| state.make_derived(Bounds::Field))
            });
        }
        Instruction::Exp => {
            // Pop 2 (base, exp), push 1 (base^exp)
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_n_with(state, n, |state| state.make_derived(Bounds::Field))
            });
        }
        Instruction::ExpImm(_) | Instruction::ExpBitLength(_) => {
            // Pop 1, push 1
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_n_with(state, n, |state| state.make_derived(Bounds::Field))
            });
        }

        // ─────────────────────────────────────────────────────────────────────
        // Dynamic procedure calls
        // ─────────────────────────────────────────────────────────────────────
        Instruction::DynExec | Instruction::DynCall => {
            // Unknown effect; we don't assume any specific pop/push pattern.
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
            // Clear until we model this precisely.
            state.stack.clear();
        }
        Instruction::HornerBase | Instruction::HornerExt => {
            // Pop 16 inputs, push 16 derived outputs (accumulators updated)
            apply_pop_push_actions(state, &actions, pop_n, |state, n, _| {
                push_n_with(state, n, |state| state.make_derived(Bounds::Field))
            });
        }
        Instruction::EvalCircuit | Instruction::LogPrecompile => {
            // These are still treated conservatively.
            state.stack.clear();
        }

        // ─────────────────────────────────────────────────────────────────────
        // System event operations
        // ─────────────────────────────────────────────────────────────────────
        Instruction::SysEvent(_) => {
            // System events may have complex stack effects
            state.stack.clear();
        }
        _ => {
            // Default: apply dispatcher actions as generic pop/push with source-aware values.
            apply_pop_push_actions(state, &actions, pop_n, |state, n, kind| match kind {
                SourceKind::Advice => push_n_with(state, n, |state| state.make_advice(span)),
                SourceKind::Literal => push_n_with(state, n, |state| state.make_literal(0, None)),
                SourceKind::StackCopy | SourceKind::Derived => {
                    push_derived_with_bounds(state, n, Bounds::Field)
                }
                SourceKind::Memory => push_n_with(state, n, |state| state.make_memory(span)),
                SourceKind::Merkle => push_n_with(state, n, |state| state.make_merkle(span)),
            });
        }
    }
}

#[cfg(test)]
mod tests;
