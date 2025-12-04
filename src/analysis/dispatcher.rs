//! Instruction action dispatcher built on semantics.
//!
//! This provides a small set of actions (pop/push/reorder/unknown) derived from
//! `semantics_of`/`StackOp`. Consumers can map these actions to their own state
//! mutations without duplicating giant match statements.

use miden_assembly_syntax::ast::Instruction;

use super::semantics::semantics_of;
use super::static_effect::StackOp;

/// Minimal actions representing an instruction's effect.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Action {
    /// Pop N elements.
    Pop(usize),
    /// Push N elements with a source kind.
    Push { count: usize, kind: SourceKind },
    /// Reorder/dup/drop via a `StackOp`.
    Stack(StackOp),
    /// Effect is dynamic/unknown.
    Unknown,
}

/// Origin of a pushed value to help callers choose taint/bounds.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SourceKind {
    /// Advice source (untrusted).
    Advice,
    /// Literal constant (value may be unknown to the dispatcher).
    Literal,
    /// Copy/duplication of an existing stack value.
    StackCopy,
    /// Derived/computed value with no special taint.
    Derived,
    /// Loaded from memory (address provenance known to caller).
    Memory,
    /// Loaded from a Merkle store.
    Merkle,
}

/// Dispatch instruction into a sequence of actions.
///
/// For pure stack ops we emit the `StackOp` and, if counts are known, matching
/// pop/push actions. For static effects we emit pop then push. Dynamic effects
/// return a single `Unknown` action.
pub fn actions_for(inst: &Instruction) -> Vec<Action> {
    use SourceKind::*;

    // Stack ops: preserve reorder semantics plus counts if known.
    if let Some(op) = StackOp::of(inst) {
        let mut actions = vec![Action::Stack(op)];
        if let Some(sems) = semantics_of(inst) {
            if sems.pops > 0 {
                actions.push(Action::Pop(sems.pops));
            }
            if sems.pushes > 0 {
                actions.push(Action::Push {
                    count: sems.pushes,
                    kind: StackCopy,
                });
            }
        }
        return actions;
    }

    // Advice operations: explicitly mark advice origin.
    match inst {
        Instruction::AdvPush(imm) => {
            if let miden_assembly_syntax::ast::Immediate::Value(v) = imm {
                return vec![Action::Push {
                    count: v.into_inner() as usize,
                    kind: Advice,
                }];
            } else {
                return vec![Action::Unknown];
            }
        }
        Instruction::AdvLoadW => {
            return vec![
                Action::Pop(4),
                Action::Push {
                    count: 4,
                    kind: Advice,
                },
            ];
        }
        Instruction::AdvPipe => {
            return vec![
                Action::Pop(8),
                Action::Push {
                    count: 8,
                    kind: Advice,
                },
            ];
        }
        _ => {}
    }

    // Memory loads/stores
    match inst {
        Instruction::MemLoad | Instruction::MemLoadImm(_) => {
            return vec![
                Action::Pop(1),
                Action::Push {
                    count: 1,
                    kind: Memory,
                },
            ];
        }
        Instruction::MemLoadWBe
        | Instruction::MemLoadWLe
        | Instruction::MemLoadWBeImm(_)
        | Instruction::MemLoadWLeImm(_) => {
            return vec![
                Action::Pop(5.min(semantics_of(inst).map(|s| s.pops).unwrap_or(5))), // includes addr
                Action::Push {
                    count: 4,
                    kind: Memory,
                },
            ];
        }
        Instruction::MemStore | Instruction::MemStoreImm(_) => {
            return vec![Action::Pop(semantics_of(inst).map(|s| s.pops).unwrap_or(2))];
        }
        Instruction::MemStoreWBe
        | Instruction::MemStoreWLe
        | Instruction::MemStoreWBeImm(_)
        | Instruction::MemStoreWLeImm(_) => {
            return vec![Action::Pop(semantics_of(inst).map(|s| s.pops).unwrap_or(5))];
        }
        _ => {}
    }

    // Merkle ops
    match inst {
        Instruction::MTreeGet => {
            return vec![
                Action::Pop(semantics_of(inst).map(|s| s.pops).unwrap_or(6)),
                Action::Push {
                    count: 4,
                    kind: Merkle,
                },
            ];
        }
        Instruction::MTreeSet | Instruction::MTreeMerge => {
            return vec![
                Action::Pop(semantics_of(inst).map(|s| s.pops).unwrap_or(8)),
                Action::Push {
                    count: 4,
                    kind: Merkle,
                },
            ];
        }
        Instruction::MTreeVerify | Instruction::MTreeVerifyWithError(_) => {
            return vec![Action::Pop(semantics_of(inst).map(|s| s.pops).unwrap_or(0))];
        }
        _ => {}
    }

    // Literal pushes
    match inst {
        Instruction::Push(_) => {
            return vec![Action::Push {
                count: 1,
                kind: Literal,
            }];
        }
        Instruction::PushFeltList(values) => {
            return vec![Action::Push {
                count: values.len(),
                kind: Literal,
            }];
        }
        Instruction::PadW => {
            return vec![Action::Push {
                count: 4,
                kind: Literal,
            }];
        }
        Instruction::PushSlice(_, range) => {
            return vec![Action::Push {
                count: range.len(),
                kind: Literal,
            }];
        }
        _ => {}
    }

    match semantics_of(inst) {
        Some(sems) => {
            let mut actions = Vec::new();
            if sems.pops > 0 {
                actions.push(Action::Pop(sems.pops));
            }
            if sems.pushes > 0 {
                actions.push(Action::Push {
                    count: sems.pushes,
                    kind: Derived,
                });
            }
            actions
        }
        None => vec![Action::Unknown],
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use miden_assembly_syntax::ast::Instruction;

    #[test]
    fn actions_for_add() {
        let acts = actions_for(&Instruction::Add);
        assert_eq!(
            acts,
            vec![
                Action::Pop(2),
                Action::Push {
                    count: 1,
                    kind: SourceKind::Derived
                }
            ]
        );
    }

    #[test]
    fn actions_for_dup() {
        let acts = actions_for(&Instruction::Dup2);
        assert_eq!(
            acts,
            vec![
                Action::Stack(StackOp::Dup(2)),
                Action::Pop(0), // not emitted
                Action::Push {
                    count: 1,
                    kind: SourceKind::StackCopy
                }
            ]
            .into_iter()
            .filter(|a| !matches!(a, Action::Pop(0)))
            .collect::<Vec<_>>()
        );
    }

    #[test]
    fn actions_for_dynamic_call() {
        let acts = actions_for(&Instruction::DynExec);
        assert_eq!(acts, vec![Action::Unknown]);
    }

    #[test]
    fn actions_for_memory_load() {
        let acts = actions_for(&Instruction::MemLoad);
        assert_eq!(
            acts,
            vec![
                Action::Pop(1),
                Action::Push {
                    count: 1,
                    kind: SourceKind::Memory
                }
            ]
        );
    }

    #[test]
    fn actions_for_merkle_get() {
        let acts = actions_for(&Instruction::MTreeGet);
        assert_eq!(
            acts,
            vec![
                Action::Pop(6),
                Action::Push {
                    count: 4,
                    kind: SourceKind::Merkle
                }
            ]
        );
    }
}
