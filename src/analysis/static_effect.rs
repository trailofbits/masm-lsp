//! Shared stack operations and instruction effect metadata.
//!
//! This module provides:
//! - A unified `StackLike` trait for stack manipulation operations
//! - Static stack effect tables for fast-path analysis

use miden_assembly_syntax::ast::{Immediate, Instruction};

// ═══════════════════════════════════════════════════════════════════════════════
// Static Stack Effects
// ═══════════════════════════════════════════════════════════════════════════════

// ═══════════════════════════════════════════════════════════════════════════════
// Stack Operation Types
// ═══════════════════════════════════════════════════════════════════════════════

/// Classification of pure stack manipulation operations.
///
/// These operations reorder or duplicate existing elements without changing
/// provenance. Conditional stack ops are intentionally excluded because they
/// consume conditions and have semantic effects beyond reordering.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StackOp {
    // ─────────────────────────────────────────────────────────────────────────
    // Single element operations
    // ─────────────────────────────────────────────────────────────────────────
    /// Swap top with element at position n: `swap.n`
    Swap(usize),
    /// Duplicate element at position n to top: `dup.n`
    Dup(usize),
    /// Move element at position n to top: `movup.n`
    MovUp(usize),
    /// Move top element to position n: `movdn.n`
    MovDn(usize),

    // ─────────────────────────────────────────────────────────────────────────
    // Word operations (4-element groups)
    // ─────────────────────────────────────────────────────────────────────────
    /// Swap word 0 with word at position n: `swapw.n`
    SwapW(usize),
    /// Duplicate word at position n to top: `dupw.n`
    DupW(usize),
    /// Move word at position n to top: `movupw.n`
    MovUpW(usize),
    /// Move top word to position n: `movdnw.n`
    MovDnW(usize),
    /// Swap double words (0..8 with 8..16): `swapdw`
    SwapDW,
}

impl StackOp {
    /// Returns the depth required for this operation (minimum stack elements needed).
    pub fn required_depth(&self) -> usize {
        match self {
            StackOp::Swap(n) => *n + 1,
            StackOp::Dup(n) => *n + 1,
            StackOp::MovUp(n) => *n + 1,
            StackOp::MovDn(n) => *n + 1,
            StackOp::SwapW(n) => (*n + 1) * 4,
            StackOp::DupW(n) => (*n + 1) * 4,
            StackOp::MovUpW(n) => (*n + 1) * 4,
            StackOp::MovDnW(n) => (*n + 1) * 4,
            StackOp::SwapDW => 16,
        }
    }

    /// Map an instruction to a pure stack manipulation op, if applicable.
    pub fn of(inst: &Instruction) -> Option<Self> {
        use Instruction::*;
        match inst {
            Swap1 => Some(StackOp::Swap(1)),
            Swap2 => Some(StackOp::Swap(2)),
            Swap3 => Some(StackOp::Swap(3)),
            Swap4 => Some(StackOp::Swap(4)),
            Swap5 => Some(StackOp::Swap(5)),
            Swap6 => Some(StackOp::Swap(6)),
            Swap7 => Some(StackOp::Swap(7)),
            Swap8 => Some(StackOp::Swap(8)),
            Swap9 => Some(StackOp::Swap(9)),
            Swap10 => Some(StackOp::Swap(10)),
            Swap11 => Some(StackOp::Swap(11)),
            Swap12 => Some(StackOp::Swap(12)),
            Swap13 => Some(StackOp::Swap(13)),
            Swap14 => Some(StackOp::Swap(14)),
            Swap15 => Some(StackOp::Swap(15)),

            Dup0 => Some(StackOp::Dup(0)),
            Dup1 => Some(StackOp::Dup(1)),
            Dup2 => Some(StackOp::Dup(2)),
            Dup3 => Some(StackOp::Dup(3)),
            Dup4 => Some(StackOp::Dup(4)),
            Dup5 => Some(StackOp::Dup(5)),
            Dup6 => Some(StackOp::Dup(6)),
            Dup7 => Some(StackOp::Dup(7)),
            Dup8 => Some(StackOp::Dup(8)),
            Dup9 => Some(StackOp::Dup(9)),
            Dup10 => Some(StackOp::Dup(10)),
            Dup11 => Some(StackOp::Dup(11)),
            Dup12 => Some(StackOp::Dup(12)),
            Dup13 => Some(StackOp::Dup(13)),
            Dup14 => Some(StackOp::Dup(14)),
            Dup15 => Some(StackOp::Dup(15)),

            MovUp2 => Some(StackOp::MovUp(2)),
            MovUp3 => Some(StackOp::MovUp(3)),
            MovUp4 => Some(StackOp::MovUp(4)),
            MovUp5 => Some(StackOp::MovUp(5)),
            MovUp6 => Some(StackOp::MovUp(6)),
            MovUp7 => Some(StackOp::MovUp(7)),
            MovUp8 => Some(StackOp::MovUp(8)),
            MovUp9 => Some(StackOp::MovUp(9)),
            MovUp10 => Some(StackOp::MovUp(10)),
            MovUp11 => Some(StackOp::MovUp(11)),
            MovUp12 => Some(StackOp::MovUp(12)),
            MovUp13 => Some(StackOp::MovUp(13)),
            MovUp14 => Some(StackOp::MovUp(14)),
            MovUp15 => Some(StackOp::MovUp(15)),

            MovDn2 => Some(StackOp::MovDn(2)),
            MovDn3 => Some(StackOp::MovDn(3)),
            MovDn4 => Some(StackOp::MovDn(4)),
            MovDn5 => Some(StackOp::MovDn(5)),
            MovDn6 => Some(StackOp::MovDn(6)),
            MovDn7 => Some(StackOp::MovDn(7)),
            MovDn8 => Some(StackOp::MovDn(8)),
            MovDn9 => Some(StackOp::MovDn(9)),
            MovDn10 => Some(StackOp::MovDn(10)),
            MovDn11 => Some(StackOp::MovDn(11)),
            MovDn12 => Some(StackOp::MovDn(12)),
            MovDn13 => Some(StackOp::MovDn(13)),
            MovDn14 => Some(StackOp::MovDn(14)),
            MovDn15 => Some(StackOp::MovDn(15)),

            SwapW1 => Some(StackOp::SwapW(1)),
            SwapW2 => Some(StackOp::SwapW(2)),
            SwapW3 => Some(StackOp::SwapW(3)),
            SwapDw => Some(StackOp::SwapDW),

            DupW0 => Some(StackOp::DupW(0)),
            DupW1 => Some(StackOp::DupW(1)),
            DupW2 => Some(StackOp::DupW(2)),
            DupW3 => Some(StackOp::DupW(3)),

            MovUpW2 => Some(StackOp::MovUpW(2)),
            MovUpW3 => Some(StackOp::MovUpW(3)),

            MovDnW2 => Some(StackOp::MovDnW(2)),
            MovDnW3 => Some(StackOp::MovDnW(3)),

            _ => None,
        }
    }
}

/// Static stack effect: (pops, pushes) for an instruction.
///
/// This represents the number of elements removed from and added to the stack
/// by a single instruction execution.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct StaticEffect {
    /// Number of elements popped from the stack
    pub pops: usize,
    /// Number of elements pushed to the stack
    pub pushes: usize,
}

impl StaticEffect {
    /// Create a new static effect.
    pub const fn new(pops: usize, pushes: usize) -> Self {
        Self { pops, pushes }
    }

    /// Net change in stack depth.
    pub const fn net(&self) -> i32 {
        self.pushes as i32 - self.pops as i32
    }

    /// Check if this is a stack-neutral operation.
    pub const fn is_neutral(&self) -> bool {
        self.pops == self.pushes
    }

    /// Get the static stack effect for an instruction.
    ///
    /// Returns `None` for instructions with dynamic effects (procedure calls,
    /// dynamic dispatch, etc.) that cannot be determined statically.
    pub fn of(inst: &Instruction) -> Option<StaticEffect> {
        use Instruction::*;

        Some(match inst {
            // ─────────────────────────────────────────────────────────────────────
            // Stack manipulation
            // ─────────────────────────────────────────────────────────────────────
            Drop => StaticEffect::new(1, 0),
            DropW => StaticEffect::new(4, 0),

            // Dup: pop 0, push 1 (copy from position n)
            Dup0 | Dup1 | Dup2 | Dup3 | Dup4 | Dup5 | Dup6 | Dup7 | Dup8 | Dup9 | Dup10 | Dup11
            | Dup12 | Dup13 | Dup14 | Dup15 => StaticEffect::new(0, 1),

            // DupW: pop 0, push 4 (copy word from position n)
            DupW0 | DupW1 | DupW2 | DupW3 => StaticEffect::new(0, 4),

            // Swap: pop 0, push 0 (reorder only)
            Swap1 | Swap2 | Swap3 | Swap4 | Swap5 | Swap6 | Swap7 | Swap8 | Swap9 | Swap10
            | Swap11 | Swap12 | Swap13 | Swap14 | Swap15 => StaticEffect::new(0, 0),

            // SwapW: swap words (reorder only)
            SwapW1 | SwapW2 | SwapW3 | SwapDw => StaticEffect::new(0, 0),

            // MovUp/MovDn: reorder only
            MovUp2 | MovUp3 | MovUp4 | MovUp5 | MovUp6 | MovUp7 | MovUp8 | MovUp9 | MovUp10
            | MovUp11 | MovUp12 | MovUp13 | MovUp14 | MovUp15 => StaticEffect::new(0, 0),

            MovDn2 | MovDn3 | MovDn4 | MovDn5 | MovDn6 | MovDn7 | MovDn8 | MovDn9 | MovDn10
            | MovDn11 | MovDn12 | MovDn13 | MovDn14 | MovDn15 => StaticEffect::new(0, 0),

            // MovUpW/MovDnW: reorder words
            MovUpW2 | MovUpW3 => StaticEffect::new(0, 0),
            MovDnW2 | MovDnW3 => StaticEffect::new(0, 0),

            // ─────────────────────────────────────────────────────────────────────
            // Arithmetic operations
            // ─────────────────────────────────────────────────────────────────────

            // Binary: pop 2, push 1
            Add | Sub | Mul | Div | And | Or | Xor => StaticEffect::new(2, 1),

            // Binary with immediate: pop 1, push 1
            AddImm(_) | SubImm(_) | MulImm(_) | DivImm(_) | ExpImm(_) => StaticEffect::new(1, 1),

            // Unary: pop 1, push 1
            Neg | Inv | Incr | Not | IsOdd | Pow2 | ILog2 => StaticEffect::new(1, 1),

            // Exp: pop 2, push 1
            Exp => StaticEffect::new(2, 1),
            ExpBitLength(_) => StaticEffect::new(2, 1),

            // ─────────────────────────────────────────────────────────────────────
            // Comparison operations
            // ─────────────────────────────────────────────────────────────────────

            // Binary comparisons: pop 2, push 1
            Eq | Neq | Lt | Lte | Gt | Gte => StaticEffect::new(2, 1),

            // Comparisons with immediate: pop 1, push 1
            EqImm(_) | NeqImm(_) => StaticEffect::new(1, 1),

            // Word equality: pop 8 (two words), push 1
            // Eqw compares two words: requires 8 elements, pushes flag (net +1)
            Eqw => StaticEffect::new(8, 9),

            // ─────────────────────────────────────────────────────────────────────
            // u32 operations
            // ─────────────────────────────────────────────────────────────────────

            // Assertions (no stack change, may trap)
            U32Assert
            | U32Assert2
            | U32AssertW
            | U32AssertWithError(_)
            | U32Assert2WithError(_)
            | U32AssertWWithError(_) => StaticEffect::new(0, 0),

            // Test: push bool without consuming value
            U32Test => StaticEffect::new(0, 1),
            U32TestW => StaticEffect::new(0, 1),

            // Wrapping arithmetic: pop 2, push 1
            U32WrappingAdd | U32WrappingSub | U32WrappingMul => StaticEffect::new(2, 1),
            U32WrappingAddImm(_) | U32WrappingSubImm(_) | U32WrappingMulImm(_) => {
                StaticEffect::new(1, 1)
            }

            // Overflowing arithmetic: pop 2, push 2 (result + overflow flag)
            U32OverflowingAdd | U32OverflowingSub | U32OverflowingMul => StaticEffect::new(2, 2),
            U32OverflowingAddImm(_) | U32OverflowingSubImm(_) | U32OverflowingMulImm(_) => {
                StaticEffect::new(1, 2)
            }

            // OverflowingAdd3/MAdd: pop 3, push 2
            U32OverflowingAdd3 | U32OverflowingMadd => StaticEffect::new(3, 2),

            // WrappingAdd3/MAdd: pop 3, push 1
            U32WrappingAdd3 | U32WrappingMadd => StaticEffect::new(3, 1),

            // Division: DivMod returns both quotient and remainder, Div/Mod return one value
            U32DivMod => StaticEffect::new(2, 2),
            U32Div | U32Mod => StaticEffect::new(2, 1),
            U32DivModImm(_) => StaticEffect::new(1, 2),
            U32DivImm(_) | U32ModImm(_) => StaticEffect::new(1, 1),

            // Split: pop 1, push 2 (high, low)
            U32Split => StaticEffect::new(1, 2),

            // Cast: pop 1, push 1
            U32Cast => StaticEffect::new(1, 1),

            // Bitwise: pop 2, push 1
            U32And | U32Or | U32Xor => StaticEffect::new(2, 1),

            // Bitwise unary: pop 1, push 1
            U32Not => StaticEffect::new(1, 1),

            // Shifts: pop 2, push 1
            U32Shl | U32Shr | U32Rotl | U32Rotr => StaticEffect::new(2, 1),
            U32ShlImm(_) | U32ShrImm(_) | U32RotlImm(_) | U32RotrImm(_) => StaticEffect::new(1, 1),

            // Popcount/leading/trailing: pop 1, push 1
            U32Popcnt | U32Clz | U32Ctz | U32Clo | U32Cto => StaticEffect::new(1, 1),

            // u32 comparisons: pop 2, push 1
            U32Lt | U32Lte | U32Gt | U32Gte | U32Min | U32Max => StaticEffect::new(2, 1),

            // ─────────────────────────────────────────────────────────────────────
            // Push operations
            // ─────────────────────────────────────────────────────────────────────
            Push(_) => StaticEffect::new(0, 1),
            PushFeltList(values) => StaticEffect::new(0, values.len()),
            PadW => StaticEffect::new(0, 4),
            PushSlice(_, range) => StaticEffect::new(0, range.len()),

            // ─────────────────────────────────────────────────────────────────────
            // Memory operations
            // ─────────────────────────────────────────────────────────────────────

            // Single element load: pop 1 (addr), push 1 (value)
            MemLoad => StaticEffect::new(1, 1),
            MemLoadImm(_) => StaticEffect::new(0, 1),

            // Single element store: pop 2 (addr, value), push 0
            MemStore => StaticEffect::new(2, 0),
            MemStoreImm(_) => StaticEffect::new(1, 0),

            // Word load: [A, a, ...] -> [V, ...] where A is word to overwrite, a is addr, V is loaded
            // Pop 5 (4 word elements + 1 addr), push 4 (loaded word)
            MemLoadWBe | MemLoadWLe => StaticEffect::new(5, 4),
            // Immediate: [A, ...] -> [V, ...] - pop 4 (word to overwrite), push 4 (loaded word)
            MemLoadWBeImm(_) | MemLoadWLeImm(_) => StaticEffect::new(4, 4),

            // Word store: pop 5 (addr + word), push 0
            MemStoreWBe | MemStoreWLe => StaticEffect::new(5, 0),
            MemStoreWBeImm(_) | MemStoreWLeImm(_) => StaticEffect::new(4, 0),

            // Memstream: pop 13 (addr + 12 state), push 13
            MemStream => StaticEffect::new(13, 13),

            // ─────────────────────────────────────────────────────────────────────
            // Local memory operations
            // ─────────────────────────────────────────────────────────────────────
            LocLoad(_) => StaticEffect::new(0, 1),
            LocStore(_) => StaticEffect::new(1, 0),
            LocLoadWBe(_) | LocLoadWLe(_) => StaticEffect::new(0, 4),
            LocStoreWBe(_) | LocStoreWLe(_) => StaticEffect::new(4, 0),
            Locaddr(_) => StaticEffect::new(0, 1),

            // ─────────────────────────────────────────────────────────────────────
            // Advice operations
            // ─────────────────────────────────────────────────────────────────────
            AdvPush(n) => {
                let count = match n {
                    Immediate::Value(v) => v.into_inner() as usize,
                    _ => return None, // Unknown count
                };
                StaticEffect::new(0, count)
            }
            AdvLoadW => StaticEffect::new(4, 4), // Pop address word, push value word
            AdvPipe => StaticEffect::new(8, 8),  // Transform hasher state

            // ─────────────────────────────────────────────────────────────────────
            // Cryptographic operations
            // ─────────────────────────────────────────────────────────────────────
            Hash => StaticEffect::new(4, 4),
            HMerge => StaticEffect::new(8, 4),
            HPerm => StaticEffect::new(12, 12),

            // ─────────────────────────────────────────────────────────────────────
            // Merkle tree operations
            // ─────────────────────────────────────────────────────────────────────
            MTreeGet => StaticEffect::new(6, 4), // Pop (depth, index, root), push value
            MTreeSet => StaticEffect::new(10, 4), // Pop (depth, index, old_root, value), push new_root
            MTreeMerge => StaticEffect::new(8, 4),
            MTreeVerify | MTreeVerifyWithError(_) => StaticEffect::new(0, 0),

            // ─────────────────────────────────────────────────────────────────────
            // Extension field operations (ext2)
            // ─────────────────────────────────────────────────────────────────────

            // Binary ext2: pop 4, push 2
            Ext2Add | Ext2Sub | Ext2Mul | Ext2Div => StaticEffect::new(4, 2),

            // Unary ext2: pop 2, push 2
            Ext2Neg | Ext2Inv => StaticEffect::new(2, 2),

            // ─────────────────────────────────────────────────────────────────────
            // Assertions
            // ─────────────────────────────────────────────────────────────────────
            Assert | AssertWithError(_) => StaticEffect::new(1, 0),
            AssertEq | AssertEqWithError(_) => StaticEffect::new(2, 0),
            Assertz | AssertzWithError(_) => StaticEffect::new(1, 0),
            AssertEqw | AssertEqwWithError(_) => StaticEffect::new(8, 0),

            // ─────────────────────────────────────────────────────────────────────
            // Conditional operations
            // ─────────────────────────────────────────────────────────────────────

            // CSwap: pop condition + 2 values, push 2 values (possibly swapped)
            CSwap => StaticEffect::new(3, 2),
            // CSwapW: pop condition + 2 words (8 elements), push 2 words (possibly swapped)
            CSwapW => StaticEffect::new(9, 8),
            // CDrop: pop condition + 2 values, push 1 (selected value)
            CDrop => StaticEffect::new(3, 1),
            // CDropW: pop condition + 2 words (8 elements), push 1 word (selected)
            CDropW => StaticEffect::new(9, 4),

            // ─────────────────────────────────────────────────────────────────────
            // Other operations
            // ─────────────────────────────────────────────────────────────────────
            Sdepth => StaticEffect::new(0, 1),
            Clk => StaticEffect::new(0, 1),
            // Caller overwrites top word with caller hash (net 0)
            Caller => StaticEffect::new(4, 4),
            ProcRef(_) => StaticEffect::new(0, 4), // Push procedure hash

            // Reverse operations (reorder only)
            Reversew => StaticEffect::new(0, 0),
            Reversedw => StaticEffect::new(0, 0),

            // No-ops
            Nop | Breakpoint | Debug(_) | Emit | EmitImm(_) | Trace(_) | SysEvent(_) => {
                StaticEffect::new(0, 0)
            }

            // ─────────────────────────────────────────────────────────────────────
            // Procedure calls - DYNAMIC (unknown effects)
            // ─────────────────────────────────────────────────────────────────────
            Exec(_) | Call(_) | SysCall(_) => return None,
            DynExec | DynCall => return None,

            // ─────────────────────────────────────────────────────────────────────
            // Complex STARK operations
            // ─────────────────────────────────────────────────────────────────────

            // horner_eval_base: reads c7..c0 (8), five garbage slots, alpha_addr, acc1, acc0
            // Updates acc in place. Stack depth unchanged but positions 14-15 modified.
            // For decompilation purposes, we model this as consuming and producing 16 elements.
            HornerBase => StaticEffect::new(16, 16),

            // horner_eval_ext: same pattern as horner_eval_base
            HornerExt => StaticEffect::new(16, 16),

            // fri_ext2fold4: [v7..v0, f_pos, d_seg, poe, e1, e0, a1, a0, layer_ptr, rem_ptr, ...]
            // Outputs 16 elements with stack shift left by 1 (pulls from overflow)
            FriExt2Fold4 => StaticEffect::new(17, 16),

            // eval_circuit: [ptr, n_read, n_eval, ...] -> [ptr, n_read, n_eval, ...]
            // Stack unchanged
            EvalCircuit => StaticEffect::new(3, 3),

            // log_precompile: [COMM (4), TAG (4), ...] -> [R1 (4), R0 (4), CAP_NEXT (4), ...]
            // Pops 8, pushes 12
            LogPrecompile => StaticEffect::new(8, 12),
        })
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// StackLike Trait
// ═══════════════════════════════════════════════════════════════════════════════

/// Trait for types that behave like a stack.
///
/// This provides a unified interface for stack manipulation operations,
/// allowing code reuse between different stack implementations (abstract
/// interpretation, decompiler state, etc.).
pub trait StackLike {
    /// The type of elements stored in the stack.
    type Element: Clone;

    /// Get the current stack depth.
    fn depth(&self) -> usize;

    /// Check if the stack is empty.
    fn is_empty(&self) -> bool {
        self.depth() == 0
    }

    /// Push an element onto the stack.
    fn push(&mut self, elem: Self::Element);

    /// Pop an element from the stack.
    ///
    /// If the stack is empty, implementations should either return a default
    /// value or "discover" a new input.
    fn pop(&mut self) -> Self::Element;

    /// Peek at the element at position n (0 = top).
    fn peek(&self, n: usize) -> Option<&Self::Element>;

    /// Ensure the stack has at least `needed` elements.
    ///
    /// Implementations should expand the stack with appropriate default/input
    /// values if necessary.
    fn ensure_depth(&mut self, needed: usize);

    /// Duplicate the element at position n onto the top.
    fn dup(&mut self, n: usize) {
        self.ensure_depth(n + 1);
        if let Some(elem) = self.peek(n).cloned() {
            self.push(elem);
        }
    }

    /// Swap elements at positions a and b (0 = top).
    fn swap(&mut self, a: usize, b: usize);

    /// Move the element at position n to the top.
    fn movup(&mut self, n: usize);

    /// Move the top element to position n.
    ///
    /// After `movdn(n)`: `[a, b, c, d, ...]` becomes `[b, c, d, ..., a, ...]`
    /// where `a` is now at position n.
    fn movdn(&mut self, n: usize);

    // ─────────────────────────────────────────────────────────────────────────
    // Word operations (4-element groups)
    // ─────────────────────────────────────────────────────────────────────────

    /// Swap two words (4-element groups) at word positions a and b.
    fn swapw(&mut self, word_a: usize, word_b: usize) {
        for i in 0..4 {
            self.swap(word_a * 4 + i, word_b * 4 + i);
        }
    }

    /// Move the word at position n to the top.
    fn movupw(&mut self, word_pos: usize) {
        for _ in 0..4 {
            self.movup(word_pos * 4);
        }
    }

    /// Move the top word to position n.
    fn movdnw(&mut self, word_pos: usize) {
        for _ in 0..4 {
            self.movdn(word_pos * 4);
        }
    }

    /// Duplicate a word (4 elements) at word position onto the top.
    fn dupw(&mut self, word_idx: usize) {
        let base = word_idx * 4;
        // Dup in reverse order to maintain correct stack order
        for i in (0..4).rev() {
            self.dup(base + i);
        }
    }

    /// Reverse the top 4 elements (word).
    fn reversew(&mut self) {
        self.swap(0, 3);
        self.swap(1, 2);
    }

    /// Reverse the top 8 elements (double word).
    fn reversedw(&mut self) {
        for i in 0..4 {
            self.swap(i, 7 - i);
        }
    }

    /// Push N copies of the given element onto the stack.
    fn push_n(&mut self, n: usize, elem: Self::Element)
    where
        Self::Element: Clone,
    {
        for _ in 0..n {
            self.push(elem.clone());
        }
    }

    /// Push N elements of the default value onto the stack.
    fn push_defaults(&mut self, n: usize)
    where
        Self::Element: Default,
    {
        for _ in 0..n {
            self.push(Self::Element::default());
        }
    }

    /// Pop N elements from the stack.
    fn pop_n(&mut self, n: usize) {
        for _ in 0..n {
            self.pop();
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// Stack Manipulation Helper
// ═══════════════════════════════════════════════════════════════════════════════

/// Result of applying a stack manipulation instruction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StackLikeResult {
    /// The instruction was a pure stack manipulation and was applied.
    Applied,
    /// The instruction is not a pure stack manipulation (needs special handling).
    NotApplied,
}

/// Apply pure stack manipulation instructions to a StackLike implementation.
///
/// This handles all dup, swap, movup, movdn, drop instructions and their word variants.
/// Returns `StackManipResult::Applied` if the instruction was handled, or
/// `StackManipResult::NotStackManip` if the instruction requires special handling.
///
/// This function is useful for stack tracking analysis where only the provenance
/// of values matters, not their contents.
pub fn apply_stack_manipulation<S: StackLike>(stack: &mut S, inst: &Instruction) -> StackLikeResult
where
    S::Element: Default,
{
    use Instruction::*;

    // Drop operations
    match inst {
        Drop => {
            stack.pop();
            return StackLikeResult::Applied;
        }
        DropW => {
            stack.pop_n(4);
            return StackLikeResult::Applied;
        }
        _ => {}
    }

    // Pure manipulation ops (reorders/duplicates only)
    if let Some(op) = StackOp::of(inst) {
        apply_stack_op(stack, op);
        StackLikeResult::Applied
    } else {
        StackLikeResult::NotApplied
    }
}

fn apply_stack_op<S: StackLike>(stack: &mut S, op: StackOp)
where
    S::Element: Default,
{
    match op {
        StackOp::Swap(n) => stack.swap(0, n),
        StackOp::Dup(n) => stack.dup(n),
        StackOp::MovUp(n) => stack.movup(n),
        StackOp::MovDn(n) => stack.movdn(n),
        StackOp::SwapW(n) => stack.swapw(0, n),
        StackOp::DupW(n) => stack.dupw(n),
        StackOp::MovUpW(n) => stack.movupw(n),
        StackOp::MovDnW(n) => stack.movdnw(n),
        StackOp::SwapDW => {
            for i in 0..8 {
                stack.swap(i, 8 + i);
            }
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// Tests
// ═══════════════════════════════════════════════════════════════════════════════

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_static_effect_arithmetic() {
        assert_eq!(
            StaticEffect::of(&Instruction::Add),
            Some(StaticEffect::new(2, 1))
        );
        assert_eq!(
            StaticEffect::of(&Instruction::Sub),
            Some(StaticEffect::new(2, 1))
        );
        assert_eq!(
            StaticEffect::of(&Instruction::Neg),
            Some(StaticEffect::new(1, 1))
        );
    }

    #[test]
    fn test_static_effect_stack_ops() {
        assert_eq!(
            StaticEffect::of(&Instruction::Drop),
            Some(StaticEffect::new(1, 0))
        );
        assert_eq!(
            StaticEffect::of(&Instruction::DropW),
            Some(StaticEffect::new(4, 0))
        );
        assert_eq!(
            StaticEffect::of(&Instruction::Dup0),
            Some(StaticEffect::new(0, 1))
        );
        assert_eq!(
            StaticEffect::of(&Instruction::Swap1),
            Some(StaticEffect::new(0, 0))
        );
    }

    #[test]
    fn test_static_effect_dynamic() {
        assert_eq!(StaticEffect::of(&Instruction::DynExec), None);
        assert_eq!(StaticEffect::of(&Instruction::DynCall), None);
    }

    #[test]
    fn test_net_effect() {
        assert_eq!(StaticEffect::new(2, 1).net(), -1);
        assert_eq!(StaticEffect::new(1, 2).net(), 1);
        assert_eq!(StaticEffect::new(2, 2).net(), 0);
    }
}
