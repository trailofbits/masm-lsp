//! Instruction semantics lookup.
//!
//! Semantics are defined directly over the `Instruction` enum with a single
//! `match` so there is no intermediate map to keep in sync.

use miden_assembly_syntax::ast::{Immediate, Instruction};

/// Unified instruction semantics.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InstructionEffect {
    /// Pure stack manipulation described by a `StackOp` plus its counts.
    StackOp {
        op: StackOp,
        pops: usize,
        pushes: usize,
    },
    /// Non-stack operations represented by pop/push counts.
    Counts { pops: usize, pushes: usize },
}

impl InstructionEffect {
    pub const fn counts(pops: usize, pushes: usize) -> Self {
        InstructionEffect::Counts { pops, pushes }
    }

    pub const fn with_stack_op(op: StackOp, pops: usize, pushes: usize) -> Self {
        InstructionEffect::StackOp { op, pops, pushes }
    }

    pub fn pops(&self) -> usize {
        match self {
            InstructionEffect::StackOp { pops, .. } => *pops,
            InstructionEffect::Counts { pops, .. } => *pops,
        }
    }

    pub fn pushes(&self) -> usize {
        match self {
            InstructionEffect::StackOp { pushes, .. } => *pushes,
            InstructionEffect::Counts { pushes, .. } => *pushes,
        }
    }

    pub fn net_effect(&self) -> i32 {
        self.pushes() as i32 - self.pops() as i32
    }

    pub fn is_neutral(&self) -> bool {
        self.pops() == self.pushes()
    }

    pub fn stack_op(&self) -> Option<StackOp> {
        match self {
            InstructionEffect::StackOp { op, .. } => Some(*op),
            InstructionEffect::Counts { .. } => None,
        }
    }
}

/// Classification of pure stack manipulation operations.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StackOp {
    // Single element operations
    Swap(usize),
    Dup(usize),
    MovUp(usize),
    MovDn(usize),
    Drop,
    DropW,
    // Word operations (4-element groups)
    SwapW(usize),
    DupW(usize),
    MovUpW(usize),
    MovDnW(usize),
    // Swap double word (8 elements)
    SwapDW,
}

impl StackOp {
    /// Map an instruction to a pure stack manipulation op, if applicable.
    pub fn of(inst: &Instruction) -> Option<Self> {
        use Instruction::*;
        match inst {
            Drop => Some(StackOp::Drop),
            DropW => Some(StackOp::DropW),
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

/// Stack operation helpers.
pub trait StackOpExt {
    fn is_pure_stack_op(&self) -> bool;
    fn dup_source_pos(&self) -> Option<usize>;
}

impl StackOpExt for Instruction {
    fn is_pure_stack_op(&self) -> bool {
        StackOp::of(self).is_some()
    }

    fn dup_source_pos(&self) -> Option<usize> {
        match StackOp::of(self) {
            Some(StackOp::Dup(n)) => Some(n),
            _ => None,
        }
    }
}

/// Trait for types that behave like a stack.
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
    fn pop(&mut self) -> Self::Element;

    /// Peek at the element at position n (0 = top).
    fn peek(&self, n: usize) -> Option<&Self::Element>;

    /// Ensure the stack has at least `needed` elements.
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
    fn movdn(&mut self, n: usize);

    // Word operations
    fn swapw(&mut self, word_a: usize, word_b: usize) {
        for i in 0..4 {
            self.swap(word_a * 4 + i, word_b * 4 + i);
        }
    }

    fn movupw(&mut self, word_pos: usize) {
        for _ in 0..4 {
            self.movup(word_pos * 4);
        }
    }

    fn movdnw(&mut self, word_pos: usize) {
        for _ in 0..4 {
            self.movdn(word_pos * 4);
        }
    }

    fn dupw(&mut self, word_idx: usize) {
        let base = word_idx * 4;
        for i in (0..4).rev() {
            self.dup(base + i);
        }
    }

    fn reversew(&mut self) {
        self.swap(0, 3);
        self.swap(1, 2);
    }

    fn reversedw(&mut self) {
        for i in 0..4 {
            self.swap(i, 7 - i);
        }
    }

    fn push_n(&mut self, n: usize, elem: Self::Element)
    where
        Self::Element: Clone,
    {
        for _ in 0..n {
            self.push(elem.clone());
        }
    }

    fn push_defaults(&mut self, n: usize)
    where
        Self::Element: Default,
    {
        for _ in 0..n {
            self.push(Self::Element::default());
        }
    }

    fn pop_n(&mut self, n: usize) {
        for _ in 0..n {
            self.pop();
        }
    }
}

/// Result of applying a stack manipulation instruction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StackLikeResult {
    /// The instruction was a pure stack manipulation and was applied.
    Applied,
    /// The instruction is not a pure stack manipulation (needs special handling).
    NotApplied,
}

/// Apply pure stack manipulation instructions to a StackLike implementation.
pub fn apply_stack_manipulation<S: StackLike>(stack: &mut S, inst: &Instruction) -> StackLikeResult
where
    S::Element: Default,
{
    use Instruction::*;

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

    if let Some(op) = StackOp::of(inst) {
        match op {
            StackOp::Swap(n) => stack.swap(0, n),
            StackOp::Dup(n) => stack.dup(n),
            StackOp::MovUp(n) => stack.movup(n),
            StackOp::MovDn(n) => stack.movdn(n),
            StackOp::Drop => {
                stack.pop();
                return StackLikeResult::Applied;
            }
            StackOp::DropW => {
                stack.pop_n(4);
                return StackLikeResult::Applied;
            }
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
        StackLikeResult::Applied
    } else {
        StackLikeResult::NotApplied
    }
}

/// Get semantics for an instruction.
///
/// Returns `None` for instructions whose effect depends on runtime targets
/// (calls/exec) or unresolved immediates.
pub fn semantics_of(inst: &Instruction) -> Option<InstructionEffect> {
    use Instruction::*;

    let sem = |pops, pushes| Some(InstructionEffect::counts(pops, pushes));
    let stack = |op: StackOp, pops: usize, pushes: usize| {
        Some(InstructionEffect::with_stack_op(op, pops, pushes))
    };

    match inst {
        // Dynamic/unknown
        DynExec | DynCall | Exec(_) | Call(_) | SysCall(_) => None,
        FriExt2Fold4 | EvalCircuit => None,

        // Trivial no-op
        Nop => sem(0, 0),

        // Stack manipulation (dup/swap/move/reverse)
        Swap1 => stack(StackOp::Swap(1), 0, 0),
        Swap2 => stack(StackOp::Swap(2), 0, 0),
        Swap3 => stack(StackOp::Swap(3), 0, 0),
        Swap4 => stack(StackOp::Swap(4), 0, 0),
        Swap5 => stack(StackOp::Swap(5), 0, 0),
        Swap6 => stack(StackOp::Swap(6), 0, 0),
        Swap7 => stack(StackOp::Swap(7), 0, 0),
        Swap8 => stack(StackOp::Swap(8), 0, 0),
        Swap9 => stack(StackOp::Swap(9), 0, 0),
        Swap10 => stack(StackOp::Swap(10), 0, 0),
        Swap11 => stack(StackOp::Swap(11), 0, 0),
        Swap12 => stack(StackOp::Swap(12), 0, 0),
        Swap13 => stack(StackOp::Swap(13), 0, 0),
        Swap14 => stack(StackOp::Swap(14), 0, 0),
        Swap15 => stack(StackOp::Swap(15), 0, 0),

        Dup0 => stack(StackOp::Dup(0), 0, 1),
        Dup1 => stack(StackOp::Dup(1), 0, 1),
        Dup2 => stack(StackOp::Dup(2), 0, 1),
        Dup3 => stack(StackOp::Dup(3), 0, 1),
        Dup4 => stack(StackOp::Dup(4), 0, 1),
        Dup5 => stack(StackOp::Dup(5), 0, 1),
        Dup6 => stack(StackOp::Dup(6), 0, 1),
        Dup7 => stack(StackOp::Dup(7), 0, 1),
        Dup8 => stack(StackOp::Dup(8), 0, 1),
        Dup9 => stack(StackOp::Dup(9), 0, 1),
        Dup10 => stack(StackOp::Dup(10), 0, 1),
        Dup11 => stack(StackOp::Dup(11), 0, 1),
        Dup12 => stack(StackOp::Dup(12), 0, 1),
        Dup13 => stack(StackOp::Dup(13), 0, 1),
        Dup14 => stack(StackOp::Dup(14), 0, 1),
        Dup15 => stack(StackOp::Dup(15), 0, 1),

        MovUp2 => stack(StackOp::MovUp(2), 0, 0),
        MovUp3 => stack(StackOp::MovUp(3), 0, 0),
        MovUp4 => stack(StackOp::MovUp(4), 0, 0),
        MovUp5 => stack(StackOp::MovUp(5), 0, 0),
        MovUp6 => stack(StackOp::MovUp(6), 0, 0),
        MovUp7 => stack(StackOp::MovUp(7), 0, 0),
        MovUp8 => stack(StackOp::MovUp(8), 0, 0),
        MovUp9 => stack(StackOp::MovUp(9), 0, 0),
        MovUp10 => stack(StackOp::MovUp(10), 0, 0),
        MovUp11 => stack(StackOp::MovUp(11), 0, 0),
        MovUp12 => stack(StackOp::MovUp(12), 0, 0),
        MovUp13 => stack(StackOp::MovUp(13), 0, 0),
        MovUp14 => stack(StackOp::MovUp(14), 0, 0),
        MovUp15 => stack(StackOp::MovUp(15), 0, 0),

        MovDn2 => stack(StackOp::MovDn(2), 0, 0),
        MovDn3 => stack(StackOp::MovDn(3), 0, 0),
        MovDn4 => stack(StackOp::MovDn(4), 0, 0),
        MovDn5 => stack(StackOp::MovDn(5), 0, 0),
        MovDn6 => stack(StackOp::MovDn(6), 0, 0),
        MovDn7 => stack(StackOp::MovDn(7), 0, 0),
        MovDn8 => stack(StackOp::MovDn(8), 0, 0),
        MovDn9 => stack(StackOp::MovDn(9), 0, 0),
        MovDn10 => stack(StackOp::MovDn(10), 0, 0),
        MovDn11 => stack(StackOp::MovDn(11), 0, 0),
        MovDn12 => stack(StackOp::MovDn(12), 0, 0),
        MovDn13 => stack(StackOp::MovDn(13), 0, 0),
        MovDn14 => stack(StackOp::MovDn(14), 0, 0),
        MovDn15 => stack(StackOp::MovDn(15), 0, 0),

        SwapW1 => stack(StackOp::SwapW(1), 0, 0),
        SwapW2 => stack(StackOp::SwapW(2), 0, 0),
        SwapW3 => stack(StackOp::SwapW(3), 0, 0),
        SwapDw => stack(StackOp::SwapDW, 0, 0),

        DupW0 => stack(StackOp::DupW(0), 0, 4),
        DupW1 => stack(StackOp::DupW(1), 0, 4),
        DupW2 => stack(StackOp::DupW(2), 0, 4),
        DupW3 => stack(StackOp::DupW(3), 0, 4),

        MovUpW2 => stack(StackOp::MovUpW(2), 0, 0),
        MovUpW3 => stack(StackOp::MovUpW(3), 0, 0),

        MovDnW2 => stack(StackOp::MovDnW(2), 0, 0),
        MovDnW3 => stack(StackOp::MovDnW(3), 0, 0),

        Reversew | Reversedw => sem(0, 0),

        // Assertions
        Assert | AssertWithError(_) => sem(1, 0),
        AssertEq | AssertEqWithError(_) => sem(2, 0),
        AssertEqw | AssertEqwWithError(_) => sem(8, 0),
        Assertz | AssertzWithError(_) => sem(1, 0),

        // Arithmetic
        Add | Sub | Mul | Div => sem(2, 1),
        AddImm(_) | SubImm(_) | MulImm(_) | DivImm(_) => sem(1, 1),
        Neg => sem(1, 1),
        ILog2 | Incr | Pow2 | IsOdd => sem(1, 1),
        Inv => sem(1, 1),
        Exp | ExpBitLength(_) => sem(2, 1),
        ExpImm(_) => sem(1, 1),
        Not => sem(1, 1),
        And | Or | Xor | Eq | Neq | Lt | Lte | Gt | Gte => sem(2, 1),
        EqImm(_) | NeqImm(_) => sem(1, 1),
        Eqw => sem(8, 9),

        // ext2 operations
        Ext2Add | Ext2Sub | Ext2Mul | Ext2Div => sem(4, 2),
        Ext2Neg | Ext2Inv => sem(2, 2),

        // u32 operations
        U32Test | U32TestW => sem(0, 1),
        U32Assert
        | U32AssertWithError(_)
        | U32Assert2
        | U32Assert2WithError(_)
        | U32AssertW
        | U32AssertWWithError(_) => sem(0, 0),
        U32Split => sem(1, 2),
        U32Cast => sem(1, 1),
        U32WrappingAdd | U32WrappingSub | U32WrappingMul => sem(2, 1),
        U32WrappingAddImm(_) | U32WrappingSubImm(_) | U32WrappingMulImm(_) | U32DivImm(_)
        | U32ModImm(_) | U32ShlImm(_) | U32ShrImm(_) | U32RotlImm(_) | U32RotrImm(_) | U32Not => {
            sem(1, 1)
        }
        U32OverflowingAdd | U32OverflowingSub | U32OverflowingMul => sem(2, 2),
        U32OverflowingAddImm(_) | U32OverflowingSubImm(_) | U32OverflowingMulImm(_) => sem(1, 2),
        U32OverflowingAdd3 => sem(3, 2),
        U32WrappingAdd3 => sem(3, 1),
        U32OverflowingMadd => sem(3, 2),
        U32WrappingMadd => sem(3, 1),
        U32Div | U32Mod => sem(2, 1),
        U32DivMod => sem(2, 2),
        U32DivModImm(_) => sem(1, 2),
        U32And | U32Or | U32Xor => sem(2, 1),
        U32Shr | U32Shl | U32Rotr | U32Rotl => sem(2, 1),
        U32Popcnt | U32Ctz | U32Clz | U32Clo | U32Cto => sem(1, 1),
        U32Lt | U32Lte | U32Gt | U32Gte | U32Min | U32Max => sem(2, 1),

        // Stack manipulation with data flow
        Drop => stack(StackOp::Drop, 1, 0),
        DropW => stack(StackOp::DropW, 4, 0),
        PadW => sem(0, 4),
        CSwap => sem(3, 2),
        CSwapW => sem(9, 8),
        CDrop => sem(3, 1),
        CDropW => sem(9, 4),

        // I/O and env
        Push(_) => sem(0, 1),
        PushFeltList(values) => sem(0, values.len()),
        PushSlice(_, range) => sem(0, range.len()),
        AdvPush(imm) => match imm {
            Immediate::Value(v) => sem(0, v.into_inner() as usize),
            _ => None,
        },
        Locaddr(_) => sem(0, 1),
        Sdepth => sem(0, 1),
        Caller => sem(4, 4),
        Clk => sem(0, 1),

        MemLoad => sem(1, 1),
        MemLoadImm(_) => sem(0, 1),
        MemLoadWBe | MemLoadWLe => sem(5, 4),
        MemLoadWBeImm(_) | MemLoadWLeImm(_) => sem(4, 4),
        LocLoad(_) => sem(0, 1),
        LocLoadWBe(_) | LocLoadWLe(_) => sem(0, 4),

        MemStore => sem(2, 0),
        MemStoreImm(_) => sem(1, 0),
        MemStoreWBe | MemStoreWLe => sem(5, 0),
        MemStoreWBeImm(_) | MemStoreWLeImm(_) => sem(4, 0),
        LocStore(_) => sem(1, 0),
        LocStoreWBe(_) | LocStoreWLe(_) => sem(4, 0),

        MemStream => sem(13, 13),
        AdvPipe => sem(8, 8),
        AdvLoadW => sem(4, 4),

        SysEvent(_) => sem(0, 0),

        // Crypto
        Hash => sem(4, 4),
        HMerge => sem(8, 4),
        HPerm => sem(12, 12),
        MTreeGet => sem(6, 4),
        MTreeSet => sem(10, 4),
        MTreeMerge => sem(8, 4),
        MTreeVerify | MTreeVerifyWithError(_) => sem(0, 0),

        // STARK proof verification
        HornerBase | HornerExt => sem(16, 16),
        LogPrecompile => sem(8, 12),

        ProcRef(_) => sem(0, 1),

        Breakpoint => sem(0, 0),
        Debug(_) => sem(0, 0),

        Emit | EmitImm(_) => sem(0, 0),
        Trace(_) => sem(0, 0),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use miden_assembly_syntax::ast::Instruction;

    #[test]
    fn semantics_for_basic_arithmetic() {
        let sem = semantics_of(&Instruction::Add).expect("add semantics");
        assert_eq!((sem.pops(), sem.pushes()), (2, 1));
        assert!(sem.stack_op().is_none());
    }

    #[test]
    fn semantics_for_stack_dups_and_swaps() {
        let dup = semantics_of(&Instruction::Dup3).expect("dup3");
        assert_eq!(
            (dup.pops(), dup.pushes(), dup.stack_op()),
            (0, 1, StackOp::of(&Instruction::Dup3))
        );

        let swap = semantics_of(&Instruction::Swap7).expect("swap7");
        assert_eq!(
            (swap.pops(), swap.pushes(), swap.stack_op()),
            (0, 0, StackOp::of(&Instruction::Swap7))
        );

        let drop = semantics_of(&Instruction::Drop).expect("drop");
        assert_eq!(
            (drop.pops(), drop.pushes(), drop.stack_op()),
            (1, 0, StackOp::of(&Instruction::Drop))
        );
    }

    #[test]
    fn semantics_for_memory_ops() {
        let load = semantics_of(&Instruction::MemLoad).expect("mem_load");
        assert_eq!((load.pops(), load.pushes()), (1, 1));

        let loadw = semantics_of(&Instruction::MemLoadWBe).expect("mem_loadw_be");
        assert_eq!((loadw.pops(), loadw.pushes()), (5, 4));

        let store = semantics_of(&Instruction::MemStoreImm(0u32.into())).expect("store imm");
        assert_eq!((store.pops(), store.pushes()), (1, 0));
    }

    #[test]
    fn semantics_for_dynamic_call_is_none() {
        assert!(semantics_of(&Instruction::DynExec).is_none());
        assert!(semantics_of(&Instruction::DynCall).is_none());
    }
}
