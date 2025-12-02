//! Shared instruction effects registry.
//!
//! This module provides a centralized source of truth for instruction behavior,
//! including stack effects and manipulation types. This eliminates duplication
//! across different analyzers and ensures consistency.
//!
//! # Usage
//!
//! The main entry point is [`InstructionEffect`] which describes how an instruction
//! affects the stack:
//!
//! ```ignore
//! let effect = InstructionEffect::of(instruction);
//! match effect {
//!     InstructionEffect::Static { pops, pushes } => { /* simple pop/push */ }
//!     InstructionEffect::Manipulation(StackOp::Dup(n)) => { /* dup.n */ }
//!     InstructionEffect::Unknown => { /* unknown effect */ }
//! }
//! ```

use miden_assembly_syntax::ast::Instruction;

use super::stack_ops::{static_effect, StaticEffect};

// ═══════════════════════════════════════════════════════════════════════════
// Stack Operation Types
// ═══════════════════════════════════════════════════════════════════════════

/// Classification of stack manipulation operations.
///
/// These operations preserve provenance and move/copy elements without
/// creating new values. Each variant includes the position parameter(s).
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

    // ─────────────────────────────────────────────────────────────────────────
    // Conditional/Other
    // ─────────────────────────────────────────────────────────────────────────
    /// Conditional swap based on condition: `cswap`
    CSwap,
    /// Conditional swap of words: `cswapw`
    CSwapW,
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
            StackOp::CSwap => 3,  // condition + 2 elements
            StackOp::CSwapW => 9, // condition + 2 words
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Memory Operation Types
// ═══════════════════════════════════════════════════════════════════════════

/// Classification of memory operations.
///
/// These operations interact with memory and may have special handling
/// for address tracking (input vs output addresses).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MemoryOp {
    /// Load single element from stack address: `mem_load`
    LoadStack,
    /// Load single element from immediate address: `mem_load.n`
    LoadImm,
    /// Store single element to stack address: `mem_store`
    StoreStack,
    /// Store single element to immediate address: `mem_store.n`
    StoreImm,
    /// Load word from stack address: `mem_loadw`
    LoadWStack,
    /// Load word from immediate address: `mem_loadw.n`
    LoadWImm,
    /// Store word to stack address: `mem_storew`
    StoreWStack,
    /// Store word to immediate address: `mem_storew.n`
    StoreWImm,
    /// Local load: `loc_load.n`
    LocLoad,
    /// Local store: `loc_store.n`
    LocStore,
    /// Local word load: `loc_loadw.n`
    LocLoadW,
    /// Local word store: `loc_storew.n`
    LocStoreW,
    /// Get local address: `locaddr.n`
    LocAddr,
}

impl MemoryOp {
    /// Returns true if this is a write operation.
    pub fn is_write(&self) -> bool {
        matches!(
            self,
            MemoryOp::StoreStack
                | MemoryOp::StoreImm
                | MemoryOp::StoreWStack
                | MemoryOp::StoreWImm
                | MemoryOp::LocStore
                | MemoryOp::LocStoreW
        )
    }

    /// Returns true if this is a read operation.
    pub fn is_read(&self) -> bool {
        matches!(
            self,
            MemoryOp::LoadStack
                | MemoryOp::LoadImm
                | MemoryOp::LoadWStack
                | MemoryOp::LoadWImm
                | MemoryOp::LocLoad
                | MemoryOp::LocLoadW
        )
    }

    /// Returns true if address comes from the stack (not immediate).
    pub fn has_stack_address(&self) -> bool {
        matches!(
            self,
            MemoryOp::LoadStack
                | MemoryOp::StoreStack
                | MemoryOp::LoadWStack
                | MemoryOp::StoreWStack
        )
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Instruction Effect
// ═══════════════════════════════════════════════════════════════════════════

/// Complete description of an instruction's effect.
///
/// This enum categorizes instructions by how they affect the stack:
/// - `Static`: Simple pop/push operations (arithmetic, comparison, etc.)
/// - `Manipulation`: Stack reordering that preserves provenance (swap, dup, mov)
/// - `Memory`: Memory operations with potential address tracking
/// - `Push`: Pushes a new value (literal, advice, etc.)
/// - `Unknown`: Effect cannot be statically determined
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum InstructionEffect {
    /// Simple static effect: pops N elements, pushes M elements
    Static(StaticEffect),
    /// Stack manipulation operation (preserves provenance)
    Manipulation(StackOp),
    /// Memory operation with special handling
    Memory {
        /// The type of memory operation
        op: MemoryOp,
        /// Static stack effect
        effect: StaticEffect,
    },
    /// Push operation (creates new value on stack)
    Push {
        /// Number of elements pushed
        count: usize,
    },
    /// Drop operation
    Drop {
        /// Number of elements dropped
        count: usize,
    },
    /// Unknown or complex effect (control flow, dynamic calls, etc.)
    Unknown,
}

impl InstructionEffect {
    /// Get the instruction effect for a given instruction.
    ///
    /// This is the main entry point for the effects registry.
    pub fn of(inst: &Instruction) -> Self {
        use Instruction::*;

        match inst {
            // ─────────────────────────────────────────────────────────────────
            // Stack manipulation (provenance-preserving)
            // ─────────────────────────────────────────────────────────────────
            Swap1 => InstructionEffect::Manipulation(StackOp::Swap(1)),
            Swap2 => InstructionEffect::Manipulation(StackOp::Swap(2)),
            Swap3 => InstructionEffect::Manipulation(StackOp::Swap(3)),
            Swap4 => InstructionEffect::Manipulation(StackOp::Swap(4)),
            Swap5 => InstructionEffect::Manipulation(StackOp::Swap(5)),
            Swap6 => InstructionEffect::Manipulation(StackOp::Swap(6)),
            Swap7 => InstructionEffect::Manipulation(StackOp::Swap(7)),
            Swap8 => InstructionEffect::Manipulation(StackOp::Swap(8)),
            Swap9 => InstructionEffect::Manipulation(StackOp::Swap(9)),
            Swap10 => InstructionEffect::Manipulation(StackOp::Swap(10)),
            Swap11 => InstructionEffect::Manipulation(StackOp::Swap(11)),
            Swap12 => InstructionEffect::Manipulation(StackOp::Swap(12)),
            Swap13 => InstructionEffect::Manipulation(StackOp::Swap(13)),
            Swap14 => InstructionEffect::Manipulation(StackOp::Swap(14)),
            Swap15 => InstructionEffect::Manipulation(StackOp::Swap(15)),

            Dup0 => InstructionEffect::Manipulation(StackOp::Dup(0)),
            Dup1 => InstructionEffect::Manipulation(StackOp::Dup(1)),
            Dup2 => InstructionEffect::Manipulation(StackOp::Dup(2)),
            Dup3 => InstructionEffect::Manipulation(StackOp::Dup(3)),
            Dup4 => InstructionEffect::Manipulation(StackOp::Dup(4)),
            Dup5 => InstructionEffect::Manipulation(StackOp::Dup(5)),
            Dup6 => InstructionEffect::Manipulation(StackOp::Dup(6)),
            Dup7 => InstructionEffect::Manipulation(StackOp::Dup(7)),
            Dup8 => InstructionEffect::Manipulation(StackOp::Dup(8)),
            Dup9 => InstructionEffect::Manipulation(StackOp::Dup(9)),
            Dup10 => InstructionEffect::Manipulation(StackOp::Dup(10)),
            Dup11 => InstructionEffect::Manipulation(StackOp::Dup(11)),
            Dup12 => InstructionEffect::Manipulation(StackOp::Dup(12)),
            Dup13 => InstructionEffect::Manipulation(StackOp::Dup(13)),
            Dup14 => InstructionEffect::Manipulation(StackOp::Dup(14)),
            Dup15 => InstructionEffect::Manipulation(StackOp::Dup(15)),

            MovUp2 => InstructionEffect::Manipulation(StackOp::MovUp(2)),
            MovUp3 => InstructionEffect::Manipulation(StackOp::MovUp(3)),
            MovUp4 => InstructionEffect::Manipulation(StackOp::MovUp(4)),
            MovUp5 => InstructionEffect::Manipulation(StackOp::MovUp(5)),
            MovUp6 => InstructionEffect::Manipulation(StackOp::MovUp(6)),
            MovUp7 => InstructionEffect::Manipulation(StackOp::MovUp(7)),
            MovUp8 => InstructionEffect::Manipulation(StackOp::MovUp(8)),
            MovUp9 => InstructionEffect::Manipulation(StackOp::MovUp(9)),
            MovUp10 => InstructionEffect::Manipulation(StackOp::MovUp(10)),
            MovUp11 => InstructionEffect::Manipulation(StackOp::MovUp(11)),
            MovUp12 => InstructionEffect::Manipulation(StackOp::MovUp(12)),
            MovUp13 => InstructionEffect::Manipulation(StackOp::MovUp(13)),
            MovUp14 => InstructionEffect::Manipulation(StackOp::MovUp(14)),
            MovUp15 => InstructionEffect::Manipulation(StackOp::MovUp(15)),

            MovDn2 => InstructionEffect::Manipulation(StackOp::MovDn(2)),
            MovDn3 => InstructionEffect::Manipulation(StackOp::MovDn(3)),
            MovDn4 => InstructionEffect::Manipulation(StackOp::MovDn(4)),
            MovDn5 => InstructionEffect::Manipulation(StackOp::MovDn(5)),
            MovDn6 => InstructionEffect::Manipulation(StackOp::MovDn(6)),
            MovDn7 => InstructionEffect::Manipulation(StackOp::MovDn(7)),
            MovDn8 => InstructionEffect::Manipulation(StackOp::MovDn(8)),
            MovDn9 => InstructionEffect::Manipulation(StackOp::MovDn(9)),
            MovDn10 => InstructionEffect::Manipulation(StackOp::MovDn(10)),
            MovDn11 => InstructionEffect::Manipulation(StackOp::MovDn(11)),
            MovDn12 => InstructionEffect::Manipulation(StackOp::MovDn(12)),
            MovDn13 => InstructionEffect::Manipulation(StackOp::MovDn(13)),
            MovDn14 => InstructionEffect::Manipulation(StackOp::MovDn(14)),
            MovDn15 => InstructionEffect::Manipulation(StackOp::MovDn(15)),

            // Word operations
            SwapW1 => InstructionEffect::Manipulation(StackOp::SwapW(1)),
            SwapW2 => InstructionEffect::Manipulation(StackOp::SwapW(2)),
            SwapW3 => InstructionEffect::Manipulation(StackOp::SwapW(3)),
            SwapDw => InstructionEffect::Manipulation(StackOp::SwapDW),

            DupW0 => InstructionEffect::Manipulation(StackOp::DupW(0)),
            DupW1 => InstructionEffect::Manipulation(StackOp::DupW(1)),
            DupW2 => InstructionEffect::Manipulation(StackOp::DupW(2)),
            DupW3 => InstructionEffect::Manipulation(StackOp::DupW(3)),

            MovUpW2 => InstructionEffect::Manipulation(StackOp::MovUpW(2)),
            MovUpW3 => InstructionEffect::Manipulation(StackOp::MovUpW(3)),

            MovDnW2 => InstructionEffect::Manipulation(StackOp::MovDnW(2)),
            MovDnW3 => InstructionEffect::Manipulation(StackOp::MovDnW(3)),

            CSwap => InstructionEffect::Manipulation(StackOp::CSwap),
            CSwapW => InstructionEffect::Manipulation(StackOp::CSwapW),

            // ─────────────────────────────────────────────────────────────────
            // Push operations (create new values)
            // ─────────────────────────────────────────────────────────────────
            Push(_) => InstructionEffect::Push { count: 1 },
            PushFeltList(list) => InstructionEffect::Push { count: list.len() },
            AdvPush(n) => {
                use miden_assembly_syntax::ast::Immediate;
                let count = match n {
                    Immediate::Value(v) => v.into_inner() as usize,
                    _ => 1,
                };
                InstructionEffect::Push { count }
            }

            // ─────────────────────────────────────────────────────────────────
            // Drop operations
            // ─────────────────────────────────────────────────────────────────
            Drop => InstructionEffect::Drop { count: 1 },
            DropW => InstructionEffect::Drop { count: 4 },

            // ─────────────────────────────────────────────────────────────────
            // Memory operations
            // ─────────────────────────────────────────────────────────────────
            MemLoad => InstructionEffect::Memory {
                op: MemoryOp::LoadStack,
                effect: StaticEffect::new(1, 1),
            },
            MemLoadImm(_) => InstructionEffect::Memory {
                op: MemoryOp::LoadImm,
                effect: StaticEffect::new(0, 1),
            },
            MemStore => InstructionEffect::Memory {
                op: MemoryOp::StoreStack,
                effect: StaticEffect::new(2, 0),
            },
            MemStoreImm(_) => InstructionEffect::Memory {
                op: MemoryOp::StoreImm,
                effect: StaticEffect::new(1, 0),
            },
            MemLoadWBe | MemLoadWLe => InstructionEffect::Memory {
                op: MemoryOp::LoadWStack,
                effect: StaticEffect::new(5, 4),
            },
            MemLoadWBeImm(_) | MemLoadWLeImm(_) => InstructionEffect::Memory {
                op: MemoryOp::LoadWImm,
                effect: StaticEffect::new(4, 4),
            },
            MemStoreWBe | MemStoreWLe => InstructionEffect::Memory {
                op: MemoryOp::StoreWStack,
                effect: StaticEffect::new(5, 0),
            },
            MemStoreWBeImm(_) | MemStoreWLeImm(_) => InstructionEffect::Memory {
                op: MemoryOp::StoreWImm,
                effect: StaticEffect::new(4, 0),
            },
            LocLoad(_) => InstructionEffect::Memory {
                op: MemoryOp::LocLoad,
                effect: StaticEffect::new(0, 1),
            },
            LocStore(_) => InstructionEffect::Memory {
                op: MemoryOp::LocStore,
                effect: StaticEffect::new(1, 0),
            },
            LocLoadWBe(_) | LocLoadWLe(_) => InstructionEffect::Memory {
                op: MemoryOp::LocLoadW,
                effect: StaticEffect::new(0, 4),
            },
            LocStoreWBe(_) | LocStoreWLe(_) => InstructionEffect::Memory {
                op: MemoryOp::LocStoreW,
                effect: StaticEffect::new(4, 0),
            },
            Locaddr(_) => InstructionEffect::Memory {
                op: MemoryOp::LocAddr,
                effect: StaticEffect::new(0, 1),
            },

            // ─────────────────────────────────────────────────────────────────
            // Unknown/dynamic effects (procedure calls)
            // ─────────────────────────────────────────────────────────────────
            Exec(_) | Call(_) | SysCall(_) | DynExec | DynCall => InstructionEffect::Unknown,
            SysEvent(_) => InstructionEffect::Unknown,

            // ─────────────────────────────────────────────────────────────────
            // Complex STARK operations - use static effects from stack_ops
            // ─────────────────────────────────────────────────────────────────
            FriExt2Fold4 | HornerBase | HornerExt | EvalCircuit | LogPrecompile => {
                if let Some(effect) = static_effect(inst) {
                    InstructionEffect::Static(effect)
                } else {
                    InstructionEffect::Unknown
                }
            }

            // ─────────────────────────────────────────────────────────────────
            // All other instructions - use static_effect
            // ─────────────────────────────────────────────────────────────────
            _ => {
                if let Some(effect) = static_effect(inst) {
                    InstructionEffect::Static(effect)
                } else {
                    InstructionEffect::Unknown
                }
            }
        }
    }

    /// Get the static stack effect (pops, pushes) regardless of effect type.
    ///
    /// Returns `None` for `Unknown` effects.
    pub fn stack_effect(&self) -> Option<StaticEffect> {
        match self {
            InstructionEffect::Static(e) => Some(*e),
            InstructionEffect::Manipulation(op) => match op {
                StackOp::Dup(_) => Some(StaticEffect::new(0, 1)),
                StackOp::DupW(_) => Some(StaticEffect::new(0, 4)),
                // Other manipulations have net-zero effect
                _ => Some(StaticEffect::new(0, 0)),
            },
            InstructionEffect::Memory { effect, .. } => Some(*effect),
            InstructionEffect::Push { count } => Some(StaticEffect::new(0, *count)),
            InstructionEffect::Drop { count } => Some(StaticEffect::new(*count, 0)),
            InstructionEffect::Unknown => None,
        }
    }

    /// Returns true if this is a stack manipulation operation.
    pub fn is_manipulation(&self) -> bool {
        matches!(self, InstructionEffect::Manipulation(_))
    }

    /// Returns true if this is a memory operation.
    pub fn is_memory(&self) -> bool {
        matches!(self, InstructionEffect::Memory { .. })
    }

    /// Returns true if this is a memory write operation.
    pub fn is_memory_write(&self) -> bool {
        matches!(self, InstructionEffect::Memory { op, .. } if op.is_write())
    }

    /// Returns true if this is a memory read operation.
    pub fn is_memory_read(&self) -> bool {
        matches!(self, InstructionEffect::Memory { op, .. } if op.is_read())
    }

    /// Returns true if this operation has unknown effect.
    pub fn is_unknown(&self) -> bool {
        matches!(self, InstructionEffect::Unknown)
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Tests
// ═══════════════════════════════════════════════════════════════════════════

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_swap_effects() {
        use miden_assembly_syntax::ast::Instruction;

        let effect = InstructionEffect::of(&Instruction::Swap1);
        assert!(matches!(
            effect,
            InstructionEffect::Manipulation(StackOp::Swap(1))
        ));
        assert_eq!(effect.stack_effect(), Some(StaticEffect::new(0, 0)));

        let effect = InstructionEffect::of(&Instruction::Swap15);
        assert!(matches!(
            effect,
            InstructionEffect::Manipulation(StackOp::Swap(15))
        ));
    }

    #[test]
    fn test_dup_effects() {
        use miden_assembly_syntax::ast::Instruction;

        let effect = InstructionEffect::of(&Instruction::Dup0);
        assert!(matches!(
            effect,
            InstructionEffect::Manipulation(StackOp::Dup(0))
        ));
        assert_eq!(effect.stack_effect(), Some(StaticEffect::new(0, 1)));
    }

    #[test]
    fn test_mov_effects() {
        use miden_assembly_syntax::ast::Instruction;

        let effect = InstructionEffect::of(&Instruction::MovUp5);
        assert!(matches!(
            effect,
            InstructionEffect::Manipulation(StackOp::MovUp(5))
        ));
        assert_eq!(effect.stack_effect(), Some(StaticEffect::new(0, 0)));

        let effect = InstructionEffect::of(&Instruction::MovDn8);
        assert!(matches!(
            effect,
            InstructionEffect::Manipulation(StackOp::MovDn(8))
        ));
    }

    #[test]
    fn test_memory_effects() {
        use miden_assembly_syntax::ast::Instruction;

        let effect = InstructionEffect::of(&Instruction::MemLoad);
        assert!(matches!(
            effect,
            InstructionEffect::Memory {
                op: MemoryOp::LoadStack,
                ..
            }
        ));
        assert!(effect.is_memory_read());
        assert!(!effect.is_memory_write());

        let effect = InstructionEffect::of(&Instruction::MemStore);
        assert!(matches!(
            effect,
            InstructionEffect::Memory {
                op: MemoryOp::StoreStack,
                ..
            }
        ));
        assert!(!effect.is_memory_read());
        assert!(effect.is_memory_write());
    }

    #[test]
    fn test_mem_loadw_effect() {
        use miden_assembly_syntax::ast::Instruction;

        let effect = InstructionEffect::of(&Instruction::MemLoadWBe);
        if let InstructionEffect::Memory { effect, .. } = effect {
            // mem_loadw: pops 5 (4 word + addr), pushes 4 (loaded word)
            assert_eq!(effect.pops, 5);
            assert_eq!(effect.pushes, 4);
        } else {
            panic!("Expected Memory effect");
        }
    }

    #[test]
    fn test_arithmetic_uses_static() {
        use miden_assembly_syntax::ast::Instruction;

        let effect = InstructionEffect::of(&Instruction::Add);
        assert!(matches!(effect, InstructionEffect::Static(_)));
        assert_eq!(effect.stack_effect(), Some(StaticEffect::new(2, 1)));
    }

    #[test]
    fn test_required_depth() {
        assert_eq!(StackOp::Swap(1).required_depth(), 2);
        assert_eq!(StackOp::Swap(15).required_depth(), 16);
        assert_eq!(StackOp::Dup(0).required_depth(), 1);
        assert_eq!(StackOp::MovUp(10).required_depth(), 11);
        assert_eq!(StackOp::SwapW(1).required_depth(), 8);
        assert_eq!(StackOp::SwapDW.required_depth(), 16);
    }
}
