use std::fmt;

use super::{
    context::SsaContext,
    ids::{SsaId, VarKind},
    stack::SsaStack,
};

/// Loop counter names in order of nesting depth.
pub const LOOP_COUNTER_NAMES: &[&str] = &["i", "j", "k", "l", "m", "n", "o", "p", "q"];

/// Error when attempting to create loop phi nodes.
#[derive(Debug)]
pub enum LoopPhiError {
    DepthMismatch { entry: usize, exit: usize },
    PermutedStack,
}

impl fmt::Display for LoopPhiError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LoopPhiError::DepthMismatch { entry, exit } => {
                write!(
                    f,
                    "loop entry/exit stack depths differ (entry={entry}, exit={exit})"
                )
            }
            LoopPhiError::PermutedStack => {
                write!(
                    f,
                    "loop permutes stack values; variable mapping is ambiguous"
                )
            }
        }
    }
}

/// Decompiler state that uses SSA for tracking variable identity.
///
/// This is used during Pass 1 to build SSA relationships. After the AST walk,
/// call `resolve_names()` on the SSA context to compute final display names.
#[derive(Debug)]
pub struct DecompilerState {
    /// SSA context for tracking values and phi relationships.
    pub ctx: SsaContext,
    /// Stack of SSA IDs.
    stack: SsaStack,
    /// Whether tracking has failed.
    pub tracking_failed: bool,
}

impl DecompilerState {
    /// Next argument index based on currently known arguments.
    fn next_argument_index(&self) -> usize {
        self.ctx
            .values
            .values()
            .filter(|v| matches!(v.kind, VarKind::Argument(_)))
            .count()
    }

    /// Create a new state with procedure arguments.
    pub fn new(input_count: usize) -> Self {
        let mut ctx = SsaContext::new();
        let mut args = Vec::new();
        for i in 0..input_count {
            args.push(ctx.new_argument(i));
        }
        let stack = SsaStack::with_arguments(&args);
        Self {
            ctx,
            stack,
            tracking_failed: false,
        }
    }

    /// Create a new local variable and return its SSA ID.
    pub fn new_local(&mut self) -> SsaId {
        self.ctx.new_local()
    }

    /// Push an SSA ID onto the stack.
    pub fn push(&mut self, id: SsaId) {
        self.stack.push(id);
    }

    /// Pop an SSA ID from the stack.
    /// If stack is empty, creates a new argument (dynamic discovery).
    pub fn pop(&mut self) -> SsaId {
        self.stack.pop().unwrap_or_else(|| {
            // Discover a new argument
            let next_arg = self.next_argument_index();
            self.ctx.new_argument(next_arg)
        })
    }

    /// Peek at the SSA ID at position n (0 = top), discovering inputs if needed.
    pub fn peek(&mut self, n: usize) -> Option<SsaId> {
        if self.depth() <= n {
            self.ensure_depth(n + 1);
        }
        self.stack.peek(n)
    }

    /// Get the current stack depth.
    pub fn depth(&self) -> usize {
        self.stack.depth()
    }

    /// Duplicate the value at position n to the top.
    pub fn dup(&mut self, n: usize) {
        self.ensure_depth(n + 1);
        self.stack.dup(n);
    }

    /// Swap positions a and b.
    pub fn swap(&mut self, a: usize, b: usize) {
        let max_pos = a.max(b);
        self.ensure_depth(max_pos + 1);
        self.stack.swap(a, b);
    }

    /// Move position n to the top.
    pub fn movup(&mut self, n: usize) {
        self.ensure_depth(n + 1);
        self.stack.movup(n);
    }

    /// Move top to position n.
    pub fn movdn(&mut self, n: usize) {
        self.ensure_depth(n + 1);
        self.stack.movdn(n);
    }

    /// Apply a net stack effect to the current stack.
    ///
    /// Positive values push new locals, negative values pop existing values.
    /// Returns Err if a negative effect would underflow the current stack.
    pub fn apply_net_effect(&mut self, net_effect: i32) -> Result<(), ()> {
        if net_effect > 0 {
            for _ in 0..net_effect {
                let v = self.new_local();
                self.push(v);
            }
            Ok(())
        } else if net_effect < 0 {
            let needed = (-net_effect) as usize;
            if self.depth() < needed {
                return Err(());
            }
            for _ in 0..needed {
                // For net-effect adjustment we should not discover new args; guard above
                self.stack.pop();
            }
            Ok(())
        } else {
            Ok(())
        }
    }

    /// Save current stack state.
    pub fn save_stack(&self) -> Vec<SsaId> {
        self.stack.snapshot()
    }

    /// Restore stack state.
    pub fn restore_stack(&mut self, snapshot: &[SsaId]) {
        self.stack.restore(snapshot);
    }

    /// Ensure the stack has at least `needed` elements by creating new arguments at the bottom.
    fn ensure_depth(&mut self, needed: usize) {
        let current = self.depth();
        if current >= needed {
            return;
        }
        let missing = needed - current;
        for _ in 0..missing {
            let idx = self.next_argument_index();
            let arg = self.ctx.new_argument(idx);
            self.stack.insert_bottom(arg);
        }
    }

    /// Mark the top `count` values on the stack as returns (r_0 is the topmost).
    pub fn mark_returns(&mut self, count: usize) {
        if count == 0 {
            return;
        }
        let snapshot = self.save_stack();
        for (idx, id) in snapshot.iter().rev().take(count).enumerate() {
            self.ctx.set_return(*id, idx);
        }
    }

    /// Create phi nodes for a loop.
    ///
    /// This should be called at loop exit when the loop has zero net effect.
    /// It creates phi nodes that merge the entry and exit values at each position.
    pub fn create_loop_phis(&mut self, entry_stack: &[SsaId]) -> Result<(), LoopPhiError> {
        let exit_stack = self.stack.snapshot();

        if entry_stack.len() != exit_stack.len() {
            return Err(LoopPhiError::DepthMismatch {
                entry: entry_stack.len(),
                exit: exit_stack.len(),
            });
        }

        // If the loop only permuted values (no new defs), accept without creating phis.
        // This avoids false failures for stack-neutral permutations.
        let mut entry_sorted = entry_stack.to_vec();
        let mut exit_sorted = exit_stack.to_vec();
        entry_sorted.sort_by_key(|id| id.0);
        exit_sorted.sort_by_key(|id| id.0);
        if entry_sorted == exit_sorted {
            self.stack.restore(&exit_stack);
            return Ok(());
        }

        for i in 0..entry_stack.len() {
            let entry_id = entry_stack[i];
            let exit_id = exit_stack[i];

            if entry_id == exit_id {
                continue;
            }

            // Create a phi that merges entry and exit values
            self.ctx.add_phi(exit_id, vec![entry_id, exit_id]);
        }

        Ok(())
    }

    /// Create phi nodes for an if-else merge.
    ///
    /// Creates phi nodes for positions where then-branch and else-branch
    /// have different values.
    pub fn create_if_else_phis(&mut self, then_stack: &[SsaId], else_stack: &[SsaId]) {
        let min_len = then_stack.len().min(else_stack.len());

        for i in 0..min_len {
            let then_id = then_stack[i];
            let else_id = else_stack[i];
            if then_id != else_id {
                // Create a phi that merges both branches
                self.ctx.add_phi(else_id, vec![then_id, else_id]);
            }
        }
    }

    /// Get the display name for an SSA ID.
    ///
    /// Call `ctx.resolve_names()` first for accurate results.
    pub fn display_name(&self, id: SsaId) -> &str {
        self.ctx.get_display_name(id)
    }

    /// Mark tracking as failed.
    pub fn fail(&mut self) {
        self.tracking_failed = true;
        self.stack = SsaStack::new();
    }
}
