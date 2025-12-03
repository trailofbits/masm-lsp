//! Symbolic stack for value tracking.
//!
//! This module provides a symbolic stack implementation that tracks
//! value metadata through stack operations.

use crate::analysis::static_effect::StackLike;

use super::taint::TrackedValue;

// ═══════════════════════════════════════════════════════════════════════════
// Symbolic Stack
// ═══════════════════════════════════════════════════════════════════════════

/// A symbolic stack that tracks value metadata.
///
/// The stack grows upward: the last element is the top of the stack (position 0).
#[derive(Clone, Debug, Default)]
pub struct SymbolicStack {
    elements: Vec<TrackedValue>,
}

impl SymbolicStack {
    pub fn new() -> Self {
        Self::default()
    }

    /// Push a value onto the stack.
    pub fn push(&mut self, value: TrackedValue) {
        self.elements.push(value);
    }

    /// Pop a value from the stack.
    pub fn pop(&mut self) -> Option<TrackedValue> {
        self.elements.pop()
    }

    /// Peek at a value on the stack (0 = top).
    pub fn peek(&self, n: usize) -> Option<&TrackedValue> {
        if n < self.elements.len() {
            Some(&self.elements[self.elements.len() - 1 - n])
        } else {
            None
        }
    }

    /// Peek at a value on the stack mutably (0 = top).
    pub fn peek_mut(&mut self, n: usize) -> Option<&mut TrackedValue> {
        let len = self.elements.len();
        if n < len {
            Some(&mut self.elements[len - 1 - n])
        } else {
            None
        }
    }

    /// Get the current stack depth.
    pub fn depth(&self) -> usize {
        self.elements.len()
    }

    /// Duplicate the value at position n onto the top.
    pub fn dup(&mut self, n: usize) -> bool {
        if n < self.elements.len() {
            let idx = self.elements.len() - 1 - n;
            let elem = self.elements[idx].clone();
            self.elements.push(elem);
            true
        } else {
            false
        }
    }

    /// Swap values at positions a and b (0 = top).
    pub fn swap(&mut self, a: usize, b: usize) -> bool {
        let len = self.elements.len();
        if a < len && b < len {
            let idx_a = len - 1 - a;
            let idx_b = len - 1 - b;
            self.elements.swap(idx_a, idx_b);
            true
        } else {
            false
        }
    }

    /// Move the element at position n to the top.
    pub fn movup(&mut self, n: usize) -> bool {
        let len = self.elements.len();
        if n < len && n > 0 {
            let idx = len - 1 - n;
            let elem = self.elements.remove(idx);
            self.elements.push(elem);
            true
        } else {
            false
        }
    }

    /// Move the top element to position n.
    pub fn movdn(&mut self, n: usize) -> bool {
        let len = self.elements.len();
        if n < len && n > 0 {
            let elem = self.elements.pop().unwrap();
            let idx = len - n;
            self.elements.insert(idx, elem);
            true
        } else {
            false
        }
    }

    /// Clear all tracked values from the stack.
    ///
    /// This is used when we encounter a procedure call with unknown stack effects.
    /// We clear the tracking to avoid false positives, since we can't know what
    /// values remain on the stack after the call.
    pub fn clear(&mut self) {
        self.elements.clear();
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// StackLike Implementation
// ═══════════════════════════════════════════════════════════════════════════

impl StackLike for SymbolicStack {
    type Element = TrackedValue;

    fn depth(&self) -> usize {
        self.elements.len()
    }

    fn push(&mut self, elem: TrackedValue) {
        self.elements.push(elem);
    }

    fn pop(&mut self) -> TrackedValue {
        // Return default (untracked) value if stack is empty
        self.elements.pop().unwrap_or_default()
    }

    fn peek(&self, n: usize) -> Option<&TrackedValue> {
        if n < self.elements.len() {
            Some(&self.elements[self.elements.len() - 1 - n])
        } else {
            None
        }
    }

    fn ensure_depth(&mut self, needed: usize) {
        // For value tracking, we pad with default (untracked) values
        while self.elements.len() < needed {
            self.elements.insert(0, TrackedValue::default());
        }
    }

    fn swap(&mut self, a: usize, b: usize) {
        self.ensure_depth(a.max(b) + 1);
        let len = self.elements.len();
        let idx_a = len - 1 - a;
        let idx_b = len - 1 - b;
        self.elements.swap(idx_a, idx_b);
    }

    fn movup(&mut self, n: usize) {
        if n == 0 {
            return;
        }
        self.ensure_depth(n + 1);
        let len = self.elements.len();
        let idx = len - 1 - n;
        let elem = self.elements.remove(idx);
        self.elements.push(elem);
    }

    fn movdn(&mut self, n: usize) {
        if n == 0 {
            return;
        }
        self.ensure_depth(n + 1);
        let elem = self.elements.pop().unwrap();
        let len = self.elements.len();
        let idx = len + 1 - n;
        self.elements.insert(idx, elem);
    }

    fn dupw(&mut self, word_idx: usize) {
        // Duplicate word while preserving element order by always copying the last element
        // of the source word; indices shift as elements are pushed.
        let pos = word_idx * 4 + 3;
        self.ensure_depth(pos + 1);
        for _ in 0..4 {
            self.dup(pos);
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Tests
// ═══════════════════════════════════════════════════════════════════════════

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::types::bounds::Bounds;

    fn make_tracked_value(value: u64) -> TrackedValue {
        TrackedValue {
            id: value,
            bounds: Bounds::Const(value),
            ..Default::default()
        }
    }

    #[test]
    fn test_symbolic_stack_basic() {
        let mut stack = SymbolicStack::new();
        assert_eq!(stack.depth(), 0);

        stack.push(make_tracked_value(1));
        stack.push(make_tracked_value(2));

        assert_eq!(stack.depth(), 2);
        assert!(matches!(stack.peek(0).unwrap().bounds, Bounds::Const(2)));
        assert!(matches!(stack.peek(1).unwrap().bounds, Bounds::Const(1)));
    }

    #[test]
    fn test_symbolic_stack_dup() {
        let mut stack = SymbolicStack::new();

        stack.push(make_tracked_value(1));
        stack.push(make_tracked_value(2));

        assert!(stack.dup(1)); // Duplicate element at position 1 (value 1)
        assert_eq!(stack.depth(), 3);
        assert!(matches!(stack.peek(0).unwrap().bounds, Bounds::Const(1)));
    }

    #[test]
    fn test_symbolic_stack_swap() {
        let mut stack = SymbolicStack::new();

        stack.push(make_tracked_value(1));
        stack.push(make_tracked_value(2));

        assert!(stack.swap(0, 1));
        assert!(matches!(stack.peek(0).unwrap().bounds, Bounds::Const(1)));
        assert!(matches!(stack.peek(1).unwrap().bounds, Bounds::Const(2)));
    }

    #[test]
    fn test_symbolic_stack_movup() {
        let mut stack = SymbolicStack::new();

        stack.push(make_tracked_value(1));
        stack.push(make_tracked_value(2));
        stack.push(make_tracked_value(3));

        assert!(stack.movup(2)); // Move element at position 2 to top
        assert!(matches!(stack.peek(0).unwrap().bounds, Bounds::Const(1)));
        assert!(matches!(stack.peek(1).unwrap().bounds, Bounds::Const(3)));
        assert!(matches!(stack.peek(2).unwrap().bounds, Bounds::Const(2)));
    }

    #[test]
    fn test_symbolic_stack_clear() {
        let mut stack = SymbolicStack::new();
        stack.push(make_tracked_value(1));
        stack.push(make_tracked_value(2));

        stack.clear();
        assert_eq!(stack.depth(), 0);
    }
}
