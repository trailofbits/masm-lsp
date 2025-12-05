use super::ids::SsaId;

/// A symbolic stack that tracks SSA IDs instead of string names.
///
/// This is used during Pass 1 to build the SSA representation.
#[derive(Debug, Clone)]
pub struct SsaStack {
    /// Stack of SSA IDs (top is last element).
    stack: Vec<SsaId>,
}

impl SsaStack {
    /// Create a new empty SSA stack.
    pub fn new() -> Self {
        Self { stack: Vec::new() }
    }

    /// Create a stack initialized with argument SSA IDs.
    ///
    /// Arguments are arranged so that `args[0]` (a_0) is on top of the stack.
    /// In the internal vec, the top is the last element, so we reverse the input.
    pub fn with_arguments(args: &[SsaId]) -> Self {
        // args = [a_0, a_1, a_2] means a_0 is on top
        // Internal stack vec has top at end, so we need [a_2, a_1, a_0]
        let stack: Vec<SsaId> = args.iter().rev().copied().collect();
        Self { stack }
    }

    /// Push an SSA ID onto the stack.
    pub fn push(&mut self, id: SsaId) {
        self.stack.push(id);
    }

    /// Insert an SSA ID at the bottom of the stack.
    pub fn insert_bottom(&mut self, id: SsaId) {
        self.stack.insert(0, id);
    }

    /// Pop an SSA ID from the stack.
    pub fn pop(&mut self) -> Option<SsaId> {
        self.stack.pop()
    }

    /// Peek at the SSA ID at position n (0 = top).
    pub fn peek(&self, n: usize) -> Option<SsaId> {
        if n < self.stack.len() {
            Some(self.stack[self.stack.len() - 1 - n])
        } else {
            None
        }
    }

    /// Get the current stack depth.
    pub fn depth(&self) -> usize {
        self.stack.len()
    }

    /// Duplicate the value at position n to the top.
    pub fn dup(&mut self, n: usize) {
        if let Some(id) = self.peek(n) {
            self.stack.push(id);
        }
    }

    /// Swap positions a and b.
    pub fn swap(&mut self, a: usize, b: usize) {
        let len = self.stack.len();
        if a < len && b < len {
            let idx_a = len - 1 - a;
            let idx_b = len - 1 - b;
            self.stack.swap(idx_a, idx_b);
        }
    }

    /// Move position n to the top.
    pub fn movup(&mut self, n: usize) {
        if n < self.stack.len() {
            let idx = self.stack.len() - 1 - n;
            let id = self.stack.remove(idx);
            self.stack.push(id);
        }
    }

    /// Move top to position n.
    pub fn movdn(&mut self, n: usize) {
        let len = self.stack.len();
        if n < len {
            let idx = len - 1 - n;
            let id = self.stack.pop().unwrap();
            self.stack.insert(idx, id);
        }
    }

    /// Get a snapshot of the stack (bottom to top).
    pub fn snapshot(&self) -> Vec<SsaId> {
        self.stack.clone()
    }

    /// Restore from a snapshot.
    pub fn restore(&mut self, snapshot: &[SsaId]) {
        self.stack = snapshot.to_vec();
    }

    /// Get the stack contents (bottom to top).
    pub fn contents(&self) -> &[SsaId] {
        &self.stack
    }
}

impl Default for SsaStack {
    fn default() -> Self {
        Self::new()
    }
}
