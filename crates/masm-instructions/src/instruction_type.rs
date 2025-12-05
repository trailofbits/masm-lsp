use miden_assembly_syntax::ast::Instruction;

use crate::StackOp;

/// High-level classification of instructions for quick filtering.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InstructionType {
    /// Pure stack manipulation (reorder/dup/move/drop).
    Stack(StackOp),
    /// Memory load/store operations.
    Memory(MemoryOp),
    /// Local variable access.
    Local(LocalOp),
    /// Advice provider access.
    Advice,
    /// Merkle store operations.
    Merkle,
    /// Procedure/syscall invocation.
    Call { dynamic: bool },
    /// Literal pushes/padding.
    Push,
    /// Cryptographic/stark helpers.
    Crypto,
    /// Everything else.
    Other,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemoryOp {
    Load,
    Store,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LocalOp {
    Load,
    Store,
    Addr,
}

impl InstructionType {
    pub fn is_memory(&self) -> bool {
        matches!(self, InstructionType::Memory(_))
    }

    pub fn is_local(&self) -> bool {
        matches!(self, InstructionType::Local(_))
    }

    pub fn is_pure_stack(&self) -> bool {
        matches!(self, InstructionType::Stack(_))
    }
}

/// Classify an instruction into a coarse-grained type.
pub fn type_of(inst: &Instruction) -> InstructionType {
    use Instruction::*;

    if let Some(op) = StackOp::of(inst) {
        return InstructionType::Stack(op);
    }

    match inst {
        // Memory
        MemLoad | MemLoadImm(_) | MemLoadWBe | MemLoadWLe | MemLoadWBeImm(_) | MemLoadWLeImm(_) => {
            InstructionType::Memory(MemoryOp::Load)
        }
        MemStore | MemStoreImm(_) | MemStoreWBe | MemStoreWLe | MemStoreWBeImm(_)
        | MemStoreWLeImm(_) => InstructionType::Memory(MemoryOp::Store),

        // Locals
        LocLoad(_) | LocLoadWBe(_) | LocLoadWLe(_) => InstructionType::Local(LocalOp::Load),
        LocStore(_) | LocStoreWBe(_) | LocStoreWLe(_) => InstructionType::Local(LocalOp::Store),
        Locaddr(_) => InstructionType::Local(LocalOp::Addr),

        // Advice
        AdvPush(_) | AdvLoadW | AdvPipe => InstructionType::Advice,

        // Merkle
        MTreeGet | MTreeSet | MTreeMerge | MTreeVerify | MTreeVerifyWithError(_) => {
            InstructionType::Merkle
        }

        // Calls
        Exec(_) | Call(_) | SysCall(_) => InstructionType::Call { dynamic: false },
        DynExec | DynCall => InstructionType::Call { dynamic: true },

        // Pushes/padding
        Push(_) | PushFeltList(_) | PushSlice(_, _) | PadW => InstructionType::Push,

        // Crypto / STARK helpers
        Hash | HMerge | HPerm | FriExt2Fold4 | HornerBase | HornerExt | EvalCircuit
        | LogPrecompile => InstructionType::Crypto,

        // Default
        _ => InstructionType::Other,
    }
}
