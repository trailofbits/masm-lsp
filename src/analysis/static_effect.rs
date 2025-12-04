//! Shared stack operations and instruction effect metadata.
//!
//! This module provides:
//! - A unified `StackLike` trait for stack manipulation operations
//! - Static stack effect facade built on generated semantics

use miden_assembly_syntax::ast::Instruction;

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
        crate::analysis::semantics::semantics_of(inst).map(|s| StaticEffect::new(s.pops, s.pushes))
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
    use crate::instruction_docs::get_instruction_info;

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

    #[test]
    fn docs_stack_shapes_match_effect_for_samples() {
        let samples = [
            Instruction::Add,
            Instruction::Drop,
            Instruction::Dup1,
            Instruction::MemLoad,
            Instruction::MemStore,
        ];

        for inst in samples {
            let effect = StaticEffect::of(&inst).expect("static effect present");
            let rendered = inst.to_string();
            let info = get_instruction_info(&rendered).expect("docs entry");
            let input_len = info
                .stack_input
                .trim_matches(['[', ']'])
                .split(',')
                .filter(|s| {
                    let t = s.trim();
                    !t.is_empty() && t != "..." && t.to_ascii_lowercase() != "stack"
                })
                .count();
            let output_len = info
                .stack_output
                .trim_matches(['[', ']'])
                .split(',')
                .filter(|s| {
                    let t = s.trim();
                    !t.is_empty() && t != "..." && t.to_ascii_lowercase() != "stack"
                })
                .count();
            assert!(
                input_len >= effect.pops,
                "doc stack_input len mismatch for {} (docs: {}, effect: {})",
                rendered,
                input_len,
                effect.pops
            );
            assert!(
                output_len >= effect.pushes,
                "doc stack_output len mismatch for {} (docs: {}, effect: {})",
                rendered,
                output_len,
                effect.pushes
            );
        }
    }
}
