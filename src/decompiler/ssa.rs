//! SSA (Static Single Assignment) representation for decompilation.
//!
//! This module provides an internal SSA representation that tracks how values
//! flow through the program. It identifies which values are "the same logical
//! variable" through phi node analysis, enabling clean pseudocode generation
//! where updates show as `a_1 = a_1 + 1` rather than `v_5 = a_1 + 1`.
//!
//! # Design
//!
//! The SSA representation is used in a two-pass approach:
//! 1. **Pass 1**: Generate SSA operations with unique value IDs
//! 2. **Pass 2**: Resolve phi relationships and emit pseudocode with proper names
//!
//! Phi nodes are created at:
//! - Loop entry points (for loop-variant variables)
//! - If-else merge points (for conditionally modified variables)
//!
//! # Pseudocode Generation Helpers
//!
//! Helper functions build SSA-aware pseudocode templates directly from
//! `DecompilerState`.

use std::collections::HashMap;
use std::fmt;

use crate::analysis::contracts::types::ProcSignature;

/// Extract the whitespace prefix for a given line (0-based) from source text.
pub fn extract_declaration_prefix(source_text: &str, line: u32) -> String {
    source_text
        .lines()
        .nth(line as usize)
        .map(|l| l.chars().take_while(|c| c.is_whitespace()).collect())
        .unwrap_or_default()
}

/// Format a procedure signature string for inlay hints.
pub fn format_procedure_signature(
    decl_prefix: &str,
    proc_name: &str,
    input_count: usize,
    output_count: Option<usize>,
    contract_signature: Option<&ProcSignature>,
) -> String {
    if let Some(sig) = contract_signature {
        return sig.format_for_display(&format!("{decl_prefix}proc {proc_name}"));
    }

    let inputs = (0..input_count)
        .map(|i| format!("a_{i}: felt"))
        .collect::<Vec<_>>()
        .join(", ");
    let outputs = output_count
        .map(|o| {
            (0..o)
                .map(|i| format!("r_{i}: felt"))
                .collect::<Vec<_>>()
                .join(", ")
        })
        .unwrap_or_else(|| "?".to_string());

    format!("{decl_prefix}proc {proc_name}({inputs}) -> ({outputs})")
}

// ═══════════════════════════════════════════════════════════════════════════
// SSA Value Identification
// ═══════════════════════════════════════════════════════════════════════════

/// Unique identifier for an SSA value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SsaId(pub usize);

impl SsaId {
    /// Create a new SSA ID.
    pub fn new(id: usize) -> Self {
        Self(id)
    }
}

impl From<&SsaId> for SsaId {
    fn from(id: &SsaId) -> Self {
        *id
    }
}

/// The kind of variable an SSA value represents.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VarKind {
    /// Procedure argument at fixed position (a_0, a_1, ...)
    Argument(usize),
    /// Local/intermediate variable (v_0, v_1, ...)
    Local(usize),
    /// Return value (r_0, r_1, ...)
    Return(usize),
}

impl VarKind {
    /// Get the display name for this variable kind.
    pub fn display_name(&self) -> String {
        match self {
            VarKind::Argument(i) => format!("a_{}", i),
            VarKind::Local(i) => format!("v_{}", i),
            VarKind::Return(i) => format!("r_{}", i),
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// SSA Values and Operations
// ═══════════════════════════════════════════════════════════════════════════

/// An SSA value with its definition.
#[derive(Debug, Clone)]
pub struct SsaValue {
    /// Unique identifier for this value.
    pub id: SsaId,
    /// The kind of variable this value represents.
    pub kind: VarKind,
    /// The canonical name to use when displaying this value.
    /// Initially set based on `kind`, but may be updated through phi resolution.
    pub display_name: String,
}

impl SsaValue {
    /// Create a new argument value.
    pub fn argument(id: SsaId, index: usize) -> Self {
        let kind = VarKind::Argument(index);
        Self {
            id,
            display_name: kind.display_name(),
            kind,
        }
    }

    /// Create a new local value.
    pub fn local(id: SsaId, index: usize) -> Self {
        let kind = VarKind::Local(index);
        Self {
            id,
            display_name: kind.display_name(),
            kind,
        }
    }

    /// Create a new return value.
    pub fn return_val(id: SsaId, index: usize) -> Self {
        let kind = VarKind::Return(index);
        Self {
            id,
            display_name: kind.display_name(),
            kind,
        }
    }
}

/// A phi node that merges multiple SSA values at a control flow merge point.
///
/// At display time, all values in a phi node use the same display name.
#[derive(Debug, Clone)]
pub struct PhiNode {
    /// The SSA ID that represents the merged value.
    pub result: SsaId,
    /// The SSA IDs of the values being merged.
    pub operands: Vec<SsaId>,
}

impl PhiNode {
    /// Create a new phi node.
    pub fn new(result: SsaId, operands: Vec<SsaId>) -> Self {
        Self { result, operands }
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// SSA Context
// ═══════════════════════════════════════════════════════════════════════════

/// Context for SSA-based decompilation.
///
/// Tracks all SSA values and phi nodes, and provides methods to resolve
/// which values should share the same display name.
#[derive(Debug)]
pub struct SsaContext {
    /// All SSA values created during analysis.
    pub values: HashMap<SsaId, SsaValue>,
    /// All phi nodes created at merge points.
    pub phi_nodes: Vec<PhiNode>,
    /// Counter for generating unique SSA IDs.
    pub next_id: usize,
    /// Counter for generating local variable indices.
    next_local_index: usize,
    /// Maps SSA IDs to their canonical display name (computed during resolution).
    resolved_names: HashMap<SsaId, String>,
}

impl SsaContext {
    /// Create a new SSA context.
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            phi_nodes: Vec::new(),
            next_id: 0,
            next_local_index: 0,
            resolved_names: HashMap::new(),
        }
    }

    /// Create a new SSA value for a procedure argument.
    pub fn new_argument(&mut self, index: usize) -> SsaId {
        let id = SsaId::new(self.next_id);
        self.next_id += 1;
        let value = SsaValue::argument(id, index);
        self.values.insert(id, value);
        id
    }

    /// Create a new SSA value for a local/intermediate variable.
    pub fn new_local(&mut self) -> SsaId {
        let id = SsaId::new(self.next_id);
        self.next_id += 1;
        let index = self.next_local_index;
        self.next_local_index += 1;
        let value = SsaValue::local(id, index);
        self.values.insert(id, value);
        id
    }

    /// Create a new SSA value for a return value.
    pub fn new_return(&mut self, index: usize) -> SsaId {
        let id = SsaId::new(self.next_id);
        self.next_id += 1;
        let value = SsaValue::return_val(id, index);
        self.values.insert(id, value);
        id
    }

    /// Add a phi node that merges multiple values.
    ///
    /// The `result` is the SSA ID for the merged value (typically a new local).
    /// The `operands` are the SSA IDs being merged.
    pub fn add_phi(&mut self, result: SsaId, operands: Vec<SsaId>) {
        self.phi_nodes.push(PhiNode::new(result, operands));
    }

    /// Get the SSA value for an ID.
    pub fn get_value(&self, id: SsaId) -> Option<&SsaValue> {
        self.values.get(&id)
    }

    /// Resolve phi relationships and compute canonical display names.
    ///
    /// After calling this, `get_display_name` will return consistent names
    /// for all values that are phi-related.
    pub fn resolve_names(&mut self) {
        // Build equivalence classes using union-find
        let mut parent: HashMap<SsaId, SsaId> = HashMap::new();

        // Initialize each value as its own parent
        for &id in self.values.keys() {
            parent.insert(id, id);
        }

        // Find with path compression
        fn find(parent: &mut HashMap<SsaId, SsaId>, id: SsaId) -> SsaId {
            let p = *parent.get(&id).unwrap_or(&id);
            if p != id {
                let root = find(parent, p);
                parent.insert(id, root);
                root
            } else {
                id
            }
        }

        // Union two sets
        fn union(parent: &mut HashMap<SsaId, SsaId>, a: SsaId, b: SsaId) {
            let root_a = find(parent, a);
            let root_b = find(parent, b);
            if root_a != root_b {
                // Prefer the smaller ID as the root (earlier values are usually arguments)
                if root_a.0 < root_b.0 {
                    parent.insert(root_b, root_a);
                } else {
                    parent.insert(root_a, root_b);
                }
            }
        }

        // Process phi nodes to build equivalence classes
        for phi in &self.phi_nodes {
            // The result and all operands are in the same equivalence class
            for &operand in &phi.operands {
                union(&mut parent, phi.result, operand);
            }
        }

        // Resolve names: each equivalence class gets the name of its canonical member
        // The canonical member is the one with the smallest ID (typically an argument or
        // the first definition)
        for &id in self.values.keys() {
            let root = find(&mut parent, id);
            let canonical_name = self
                .values
                .get(&root)
                .map(|v| v.display_name.clone())
                .unwrap_or_else(|| format!("?_{}", id.0));
            self.resolved_names.insert(id, canonical_name);
        }
    }

    /// Get the display name for an SSA value.
    ///
    /// Call `resolve_names()` first to ensure phi relationships are resolved.
    pub fn get_display_name(&self, id: SsaId) -> &str {
        self.resolved_names
            .get(&id)
            .map(|s| s.as_str())
            .or_else(|| self.values.get(&id).map(|v| v.display_name.as_str()))
            .unwrap_or("?")
    }

    /// Get the number of values created (for estimating next argument index).
    pub fn value_count(&self) -> usize {
        self.values.len()
    }

    /// Count how many arguments have been discovered.
    pub fn argument_count(&self) -> usize {
        self.values
            .values()
            .filter(|v| matches!(v.kind, VarKind::Argument(_)))
            .count()
    }

    /// Find the smallest argument index in the phi-equivalence class of `id`.
    pub fn argument_index_via_phi(&self, id: SsaId) -> Option<usize> {
        let mut parent: HashMap<SsaId, SsaId> = HashMap::new();
        for &val in self.values.keys() {
            parent.insert(val, val);
        }

        fn find(parent: &mut HashMap<SsaId, SsaId>, id: SsaId) -> SsaId {
            let p = *parent.get(&id).unwrap_or(&id);
            if p != id {
                let root = find(parent, p);
                parent.insert(id, root);
                root
            } else {
                id
            }
        }

        fn union(parent: &mut HashMap<SsaId, SsaId>, a: SsaId, b: SsaId) {
            let root_a = find(parent, a);
            let root_b = find(parent, b);
            if root_a != root_b {
                if root_a.0 < root_b.0 {
                    parent.insert(root_b, root_a);
                } else {
                    parent.insert(root_a, root_b);
                }
            }
        }

        for phi in &self.phi_nodes {
            for &op in &phi.operands {
                union(&mut parent, phi.result, op);
            }
        }

        let mut root_arg: HashMap<SsaId, usize> = HashMap::new();
        let mut parent_copy = parent.clone();
        for (&sid, val) in self.values.iter() {
            if let VarKind::Argument(idx) = val.kind {
                let root = find(&mut parent_copy, sid);
                root_arg
                    .entry(root)
                    .and_modify(|e| *e = (*e).min(idx))
                    .or_insert(idx);
            }
        }

        let root = {
            let mut tmp = parent.clone();
            find(&mut tmp, id)
        };
        root_arg.get(&root).copied()
    }

    /// Check if two SSA values should have the same display name.
    pub fn same_variable(&self, a: SsaId, b: SsaId) -> bool {
        self.get_display_name(a) == self.get_display_name(b)
    }
}

impl Default for SsaContext {
    fn default() -> Self {
        Self::new()
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// SSA Stack for Decompilation
// ═══════════════════════════════════════════════════════════════════════════

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
        if n > 0 && n < self.stack.len() {
            let len = self.stack.len();
            let idx = len - 1 - n;
            let elem = self.stack.remove(idx);
            self.stack.push(elem);
        }
    }

    /// Move top to position n.
    pub fn movdn(&mut self, n: usize) {
        if n > 0 && !self.stack.is_empty() {
            let elem = self.stack.pop().unwrap();
            let len = self.stack.len();
            let idx = len.saturating_sub(n);
            self.stack.insert(idx, elem);
        }
    }

    /// Get a snapshot of the current stack state.
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

// ═══════════════════════════════════════════════════════════════════════════
// Pseudocode Templates
// ═══════════════════════════════════════════════════════════════════════════

/// A segment of pseudocode that may contain SSA ID references.
#[derive(Debug, Clone)]
pub enum PseudocodeSegment {
    /// Literal text (e.g., " = ", " + ", "mem[")
    Literal(String),
    /// Reference to an SSA value that will be resolved to a display name
    SsaRef(SsaId),
}

/// A pseudocode template that can be resolved to a final string.
#[derive(Debug, Clone)]
pub struct PseudocodeTemplate {
    segments: Vec<PseudocodeSegment>,
}

impl PseudocodeTemplate {
    /// Create a new empty template.
    pub fn new() -> Self {
        Self {
            segments: Vec::new(),
        }
    }

    /// Add a literal string segment.
    pub fn literal(mut self, s: &str) -> Self {
        self.segments
            .push(PseudocodeSegment::Literal(s.to_string()));
        self
    }

    /// Add an SSA reference segment.
    pub fn ssa_ref(mut self, id: SsaId) -> Self {
        self.segments.push(PseudocodeSegment::SsaRef(id));
        self
    }

    /// Resolve the template to a final string using the SSA context.
    pub fn resolve(&self, ctx: &SsaContext) -> String {
        self.segments
            .iter()
            .map(|seg| match seg {
                PseudocodeSegment::Literal(s) => s.clone(),
                PseudocodeSegment::SsaRef(id) => ctx.get_display_name(*id).to_string(),
            })
            .collect()
    }

    /// Resolve the template with an override function for specific SSA IDs.
    ///
    /// The override function is called for each SSA reference. If it returns
    /// `Some(name)`, that name is used instead of the default display name.
    /// This is used for parametric variable naming at resolution time.
    pub fn resolve_with_overrides<F>(&self, ctx: &SsaContext, override_fn: F) -> String
    where
        F: Fn(SsaId) -> Option<String>,
    {
        self.segments
            .iter()
            .map(|seg| match seg {
                PseudocodeSegment::Literal(s) => s.clone(),
                PseudocodeSegment::SsaRef(id) => {
                    // First check for override, then fall back to context
                    override_fn(*id).unwrap_or_else(|| ctx.get_display_name(*id).to_string())
                }
            })
            .collect()
    }

    /// Check if the template is empty.
    pub fn is_empty(&self) -> bool {
        self.segments.is_empty()
    }

    /// Get all SSA IDs referenced in this template.
    pub fn ssa_ids(&self) -> impl Iterator<Item = SsaId> + '_ {
        self.segments.iter().filter_map(|seg| match seg {
            PseudocodeSegment::SsaRef(id) => Some(*id),
            PseudocodeSegment::Literal(_) => None,
        })
    }
}

impl Default for PseudocodeTemplate {
    fn default() -> Self {
        Self::new()
    }
}

/// Builder for creating pseudocode templates with a fluent API.
///
/// Example:
/// ```ignore
/// let template = PseudocodeBuilder::new()
///     .var(result_id)
///     .text(" = ")
///     .var(left_id)
///     .text(" + ")
///     .var(right_id)
///     .build();
/// ```
#[derive(Debug, Default)]
pub struct PseudocodeBuilder {
    template: PseudocodeTemplate,
}

impl PseudocodeBuilder {
    /// Create a new builder.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add literal text.
    pub fn text(&mut self, s: &str) -> &mut Self {
        self.template
            .segments
            .push(PseudocodeSegment::Literal(s.to_string()));
        self
    }

    /// Add an SSA variable reference.
    pub fn var<V: Into<SsaId>>(&mut self, id: V) -> &mut Self {
        self.template
            .segments
            .push(PseudocodeSegment::SsaRef(id.into()));
        self
    }

    /// Build the template.
    pub fn build(self) -> PseudocodeTemplate {
        self.template
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// SSA-aware Decompiler State
// ═══════════════════════════════════════════════════════════════════════════

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
                write!(f, "loop entry/exit stack depths differ (entry={entry}, exit={exit})")
            }
            LoopPhiError::PermutedStack => {
                write!(f, "loop permutes stack values; variable mapping is ambiguous")
            }
        }
    }
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

// ═══════════════════════════════════════════════════════════════════════════
// Generic Pseudocode Generation Helpers
// ═══════════════════════════════════════════════════════════════════════════

/// Generate pseudocode for a binary operation (e.g., add, sub, mul).
pub fn binary_op(state: &mut DecompilerState, op: &str) -> PseudocodeTemplate {
    let b = state.pop();
    let a = state.pop();
    let result = state.new_local();
    state.push(result);

    let mut output = PseudocodeBuilder::new();
    output.var(result).text(" = ").var(a).text(" ").text(op).text(" ").var(b);
    output.build()
}

/// Generate pseudocode for a binary operation with an immediate operand.
pub fn binary_imm_op(
    state: &mut DecompilerState,
    op: &str,
    imm: &str,
) -> PseudocodeTemplate {
    let a = state.pop();
    let result = state.new_local();
    state.push(result);

    let mut output = PseudocodeBuilder::new();
    output.var(result).text(" = ").var(a).text(" ").text(op).text(" ").text(imm);
    output.build()
}

/// Generate pseudocode for a comparison operation.
pub fn comparison(state: &mut DecompilerState, op: &str) -> PseudocodeTemplate {
    let b = state.pop();
    let a = state.pop();
    let result = state.new_local();
    state.push(result);

    let mut output = PseudocodeBuilder::new();
    output
        .var(result)
        .text(" = (")
        .var(a)
        .text(" ")
        .text(op)
        .text(" ")
        .var(b)
        .text(")");
    output.build()
}

/// Generate pseudocode for a comparison operation with an immediate operand.
pub fn comparison_imm(
    state: &mut DecompilerState,
    op: &str,
    imm: &str,
) -> PseudocodeTemplate {
    let a = state.pop();
    let result = state.new_local();
    state.push(result);

    let mut output = PseudocodeBuilder::new();
    output
        .var(result)
        .text(" = (")
        .var(a)
        .text(" ")
        .text(op)
        .text(" ")
        .text(imm)
        .text(")");
    output.build()
}

/// Generate pseudocode for a unary prefix operation (e.g., negation, not).
pub fn unary_op(state: &mut DecompilerState, op: &str) -> PseudocodeTemplate {
    let a = state.pop();
    let result = state.new_local();
    state.push(result);

    let mut output = PseudocodeBuilder::new();
    output.var(result).text(" = ").text(op).var(a);
    output.build()
}

/// Generate pseudocode for a unary function call (e.g., `clz`, `popcnt`).
pub fn unary_fn(state: &mut DecompilerState, fn_name: &str) -> PseudocodeTemplate {
    let a = state.pop();
    let result = state.new_local();
    state.push(result);

    let mut output = PseudocodeBuilder::new();
    output
        .var(result)
        .text(" = ")
        .text(fn_name)
        .text("(")
        .var(a)
        .text(")");
    output.build()
}

/// Generate pseudocode for a dup operation.
pub fn dup(state: &mut DecompilerState, n: usize) -> PseudocodeTemplate {
    let src = state.peek(n).unwrap();
    state.dup(n);
    let result = state.new_local();
    state.pop();
    state.push(result);

    let mut output = PseudocodeBuilder::new();
    output.var(result).text(" = ").var(src);
    output.build()
}

/// Generate pseudocode for an ext2 (quadratic extension) binary operation.
pub fn ext2_binary_op(state: &mut DecompilerState, op: &str) -> PseudocodeTemplate {
    let b1 = state.pop();
    let b0 = state.pop();
    let a1 = state.pop();
    let a0 = state.pop();
    let r0 = state.new_local();
    let r1 = state.new_local();
    state.push(r0);
    state.push(r1);

    let mut output = PseudocodeBuilder::new();
    output
        .text("(")
        .var(r0)
        .text(", ")
        .var(r1)
        .text(") = (")
        .var(a0)
        .text(", ")
        .var(a1)
        .text(") ")
        .text(op)
        .text(" (")
        .var(b0)
        .text(", ")
        .var(b1)
        .text(")");
    output.build()
}

/// Generate pseudocode for an ext2 unary prefix operation.
pub fn ext2_unary_op(state: &mut DecompilerState, op: &str) -> PseudocodeTemplate {
    let a1 = state.pop();
    let a0 = state.pop();
    let r0 = state.new_local();
    let r1 = state.new_local();
    state.push(r0);
    state.push(r1);

    let mut output = PseudocodeBuilder::new();
    output
        .text("(")
        .var(r0)
        .text(", ")
        .var(r1)
        .text(") = ")
        .text(op)
        .text("(")
        .var(a0)
        .text(", ")
        .var(a1)
        .text(")");
    output.build()
}

/// Generate pseudocode for an ext2 unary function call.
pub fn ext2_unary_fn(state: &mut DecompilerState, fn_name: &str) -> PseudocodeTemplate {
    let a1 = state.pop();
    let a0 = state.pop();
    let r0 = state.new_local();
    let r1 = state.new_local();
    state.push(r0);
    state.push(r1);

    let mut output = PseudocodeBuilder::new();
    output
        .text("(")
        .var(r0)
        .text(", ")
        .var(r1)
        .text(") = ")
        .text(fn_name)
        .text("((")
        .var(a0)
        .text(", ")
        .var(a1)
        .text("))");
    output.build()
}

// ═══════════════════════════════════════════════════════════════════════════
// Tests
// ═══════════════════════════════════════════════════════════════════════════

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ssa_context_basic() {
        let mut ctx = SsaContext::new();

        let a0 = ctx.new_argument(0);
        let a1 = ctx.new_argument(1);
        let v0 = ctx.new_local();

        assert_eq!(ctx.get_value(a0).unwrap().display_name, "a_0");
        assert_eq!(ctx.get_value(a1).unwrap().display_name, "a_1");
        assert_eq!(ctx.get_value(v0).unwrap().display_name, "v_0");
    }

    #[test]
    fn test_phi_resolution_simple() {
        let mut ctx = SsaContext::new();

        // Simulate: a_0 is updated in a loop
        // a_0 -> v_0 (updated value) -> phi merges them
        let a0 = ctx.new_argument(0);
        let v0 = ctx.new_local();

        // Create phi: the updated value (v0) should be treated as a_0
        ctx.add_phi(v0, vec![a0, v0]);
        ctx.resolve_names();

        // Both should now display as "a_0"
        assert_eq!(ctx.get_display_name(a0), "a_0");
        assert_eq!(ctx.get_display_name(v0), "a_0");
        assert!(ctx.same_variable(a0, v0));
    }

    #[test]
    fn test_phi_resolution_chain() {
        let mut ctx = SsaContext::new();

        // Simulate multiple updates: a_0 -> v_0 -> v_1 -> ...
        let a0 = ctx.new_argument(0);
        let v0 = ctx.new_local();
        let v1 = ctx.new_local();

        // v0 is first update of a0
        ctx.add_phi(v0, vec![a0]);
        // v1 is update of v0
        ctx.add_phi(v1, vec![v0]);

        ctx.resolve_names();

        // All should display as "a_0"
        assert_eq!(ctx.get_display_name(a0), "a_0");
        assert_eq!(ctx.get_display_name(v0), "a_0");
        assert_eq!(ctx.get_display_name(v1), "a_0");
    }

    #[test]
    fn test_phi_resolution_if_else() {
        let mut ctx = SsaContext::new();

        // Simulate if-else: a_0 might become v_0 or v_1 depending on branch
        let a0 = ctx.new_argument(0);
        let v0 = ctx.new_local(); // then-branch result
        let v1 = ctx.new_local(); // else-branch result
        let v2 = ctx.new_local(); // merge result

        // At merge point, v2 = phi(v0, v1), and both v0, v1 are updates of a0
        ctx.add_phi(v0, vec![a0]);
        ctx.add_phi(v1, vec![a0]);
        ctx.add_phi(v2, vec![v0, v1]);

        ctx.resolve_names();

        // All should display as "a_0"
        assert_eq!(ctx.get_display_name(a0), "a_0");
        assert_eq!(ctx.get_display_name(v0), "a_0");
        assert_eq!(ctx.get_display_name(v1), "a_0");
        assert_eq!(ctx.get_display_name(v2), "a_0");
    }

    #[test]
    fn test_independent_variables() {
        let mut ctx = SsaContext::new();

        // Two independent variables
        let a0 = ctx.new_argument(0);
        let a1 = ctx.new_argument(1);
        let v0 = ctx.new_local(); // derived from a0
        let v1 = ctx.new_local(); // derived from a1

        ctx.add_phi(v0, vec![a0]);
        ctx.add_phi(v1, vec![a1]);

        ctx.resolve_names();

        assert_eq!(ctx.get_display_name(a0), "a_0");
        assert_eq!(ctx.get_display_name(v0), "a_0");
        assert_eq!(ctx.get_display_name(a1), "a_1");
        assert_eq!(ctx.get_display_name(v1), "a_1");

        assert!(ctx.same_variable(a0, v0));
        assert!(ctx.same_variable(a1, v1));
        assert!(!ctx.same_variable(a0, a1));
        assert!(!ctx.same_variable(v0, v1));
    }

    #[test]
    fn test_ssa_stack_basic() {
        let mut ctx = SsaContext::new();
        let a0 = ctx.new_argument(0);
        let a1 = ctx.new_argument(1);

        let stack = SsaStack::with_arguments(&[a0, a1]);

        // a_0 should be on top
        assert_eq!(stack.peek(0), Some(a0));
        assert_eq!(stack.peek(1), Some(a1));
        assert_eq!(stack.depth(), 2);
    }

    #[test]
    fn test_ssa_stack_operations() {
        let mut ctx = SsaContext::new();
        let a0 = ctx.new_argument(0);
        let a1 = ctx.new_argument(1);
        let v0 = ctx.new_local();

        let mut stack = SsaStack::with_arguments(&[a0, a1]);

        // Push v0
        stack.push(v0);
        assert_eq!(stack.peek(0), Some(v0));
        assert_eq!(stack.depth(), 3);

        // Pop v0
        assert_eq!(stack.pop(), Some(v0));
        assert_eq!(stack.depth(), 2);

        // Dup position 1
        stack.dup(1);
        assert_eq!(stack.peek(0), Some(a1));
        assert_eq!(stack.depth(), 3);
    }

    #[test]
    fn test_pseudocode_template() {
        let mut ctx = SsaContext::new();
        let a0 = ctx.new_argument(0);
        let a1 = ctx.new_argument(1);
        let v0 = ctx.new_local();

        // Create template: "v_0 = a_0 + a_1"
        let mut builder = PseudocodeBuilder::new();
        builder.var(v0);
        builder.text(" = ");
        builder.var(a0);
        builder.text(" + ");
        builder.var(a1);
        let template = builder.build();

        let result = template.resolve(&ctx);
        assert_eq!(result, "v_0 = a_0 + a_1");
    }

    #[test]
    fn test_pseudocode_template_with_phi() {
        let mut ctx = SsaContext::new();
        let a0 = ctx.new_argument(0);
        let v0 = ctx.new_local();

        // v0 is an update of a0 (via phi)
        ctx.add_phi(v0, vec![a0]);
        ctx.resolve_names();

        // Create template: "v_0 = a_0 + 1" (but v_0 should resolve to a_0)
        let mut builder = PseudocodeBuilder::new();
        builder.var(v0);
        builder.text(" = ");
        builder.var(a0);
        builder.text(" + 1");
        let template = builder.build();

        let result = template.resolve(&ctx);
        // After phi resolution, v0 displays as a_0
        assert_eq!(result, "a_0 = a_0 + 1");
    }

    #[test]
    fn test_apply_net_effect() {
        let mut state = DecompilerState::new(0);
        assert_eq!(state.depth(), 0);

        // Positive effect pushes new locals
        state.apply_net_effect(3).unwrap();
        assert_eq!(state.depth(), 3);

        // Negative effect pops existing values
        state.apply_net_effect(-2).unwrap();
        assert_eq!(state.depth(), 1);

        // Underflow should error and leave depth unchanged
        assert!(state.apply_net_effect(-5).is_err());
        assert_eq!(state.depth(), 1);
    }

    #[test]
    fn test_ssa_decompiler_state_basic() {
        let mut state = DecompilerState::new(3);

        // Stack should have a_0, a_1, a_2 with a_0 on top
        assert_eq!(state.depth(), 3);

        let top = state.peek(0).unwrap();
        assert_eq!(state.display_name(top), "a_0");

        let second = state.peek(1).unwrap();
        assert_eq!(state.display_name(second), "a_1");
    }

    #[test]
    fn test_ssa_decompiler_state_loop_phis() {
        let mut state = DecompilerState::new(2);

        // Simulate a loop: entry stack is [a_0, a_1]
        let entry_stack = state.save_stack();

        // Loop body: pop a_0, push v_0 (updated value)
        state.pop();
        let v0 = state.new_local();
        state.push(v0);

        // Create phi nodes at loop exit
        state.create_loop_phis(&entry_stack).unwrap();

        // Resolve names
        state.ctx.resolve_names();

        // v_0 should now display as a_0
        let top = state.peek(0).unwrap();
        assert_eq!(state.display_name(top), "a_0");
    }

    #[test]
    fn test_ssa_decompiler_state_if_else_phis() {
        let mut state = DecompilerState::new(1);

        // Pop condition (simulating if.true consuming it)
        let _cond = state.pop();

        // Save entry state
        let entry_stack = state.save_stack();

        // Then branch: push v_0
        let v0 = state.new_local();
        state.push(v0);
        let then_stack = state.save_stack();

        // Restore for else branch
        state.restore_stack(&entry_stack);

        // Else branch: push v_1
        let v1 = state.new_local();
        state.push(v1);
        let else_stack = state.save_stack();

        // Create phi nodes at merge
        state.create_if_else_phis(&then_stack, &else_stack);

        // Resolve names
        state.ctx.resolve_names();

        // v_1 should now display as v_0 (same as then-branch)
        assert_eq!(state.ctx.get_display_name(v1), "v_0");
    }

    #[test]
    fn test_memcopy_elements_simulation() {
        // Simulate the memcopy_elements procedure to verify SSA handling
        let mut state = DecompilerState::new(3); // a_0=n, a_1=read_ptr, a_2=write_ptr

        // neg: v_0 = -a_0
        let _a0 = state.pop();
        let v0 = state.new_local();
        state.push(v0);

        // dup neq.0: v_1 = (v_0 != 0)
        state.dup(0);
        let _dup_v0 = state.pop();
        let v1 = state.new_local();
        state.push(v1);

        // Save loop entry state (with condition on top)
        let loop_entry_stack = state.save_stack();

        // Pop condition for while.true
        state.pop();

        // Loop body simulation (simplified):
        // - read from a_1, write to a_2
        // - update v_0 (counter), a_1 (read_ptr), a_2 (write_ptr)
        // - compute new condition

        // For simplicity, just simulate the updates:
        // Stack after body: [new_cond, new_counter, new_read_ptr, new_write_ptr]

        // Pop old values
        state.pop(); // v_0
        state.pop(); // a_1
        state.pop(); // a_2

        // Push new values (in reverse order to match stack positions)
        let new_write_ptr = state.new_local(); // v_2
        state.push(new_write_ptr);
        let new_read_ptr = state.new_local(); // v_3
        state.push(new_read_ptr);
        let new_counter = state.new_local(); // v_4
        state.push(new_counter);
        let new_cond = state.new_local(); // v_5
        state.push(new_cond);

        // Create phi nodes for loop (comparing with loop_entry_stack which has condition)
        state.create_loop_phis(&loop_entry_stack).unwrap();

        // Resolve names
        state.ctx.resolve_names();

        // The new condition (v_5) should resolve to same name as original condition (v_1)
        // because they're at the same stack position
        assert_eq!(
            state.ctx.get_display_name(new_cond),
            state.ctx.get_display_name(v1)
        );

        // The new counter (v_4) should resolve to same name as original counter (v_0)
        assert_eq!(
            state.ctx.get_display_name(new_counter),
            state.ctx.get_display_name(v0)
        );

        // The new read_ptr (v_3) should resolve to a_1
        let a1_id = SsaId::new(1); // a_1 has ID 1
        assert_eq!(
            state.ctx.get_display_name(new_read_ptr),
            state.ctx.get_display_name(a1_id)
        );
    }

    // ─────────────────────────────────────────────────────────────────────────────
    // Tests for Generic Helper Functions
    // ─────────────────────────────────────────────────────────────────────────────

    #[test]
    fn test_generic_binary_op_ssa() {
        let mut state = DecompilerState::new(2);
        let output: PseudocodeTemplate = binary_op(&mut state, "+");

        let result = output.resolve(&state.ctx);
        // Same operand order as string version
        assert_eq!(result, "v_0 = a_1 + a_0");
    }

    #[test]
    fn test_generic_binary_imm_op_ssa() {
        let mut state = DecompilerState::new(1);
        let output: PseudocodeTemplate = binary_imm_op(&mut state, "+", "42");

        let resolved = output.resolve(&state.ctx);
        assert_eq!(resolved, "v_0 = a_0 + 42");
    }

    #[test]
    fn test_generic_binary_op_ssa_with_phi() {
        let mut state = DecompilerState::new(2);

        // Simulate a loop where a_0 is updated
        let entry_stack = state.save_stack();

        // Pop a_0, compute result, push back
        let _a0 = state.pop();
        let _a1 = state.peek(0).unwrap();
        let v0 = state.new_local();
        state.push(v0);

        // Create phi for loop
        state.create_loop_phis(&entry_stack).unwrap();
        state.ctx.resolve_names();

        // v_0 should display as a_0 after phi resolution
        assert_eq!(state.ctx.get_display_name(v0), "a_0");

        // Now use the binary_op helper - it will create v_1
        // Stack: [v_0 (displays as a_0), a_1]
        // Pop order: b=v_0 (a_0), a=a_1
        let output: PseudocodeTemplate = binary_op(&mut state, "+");
        let result = output.resolve(&state.ctx);

        // After phi resolution, v_0 displays as a_0
        // Format: v_1 = a op b = a_1 + a_0
        assert_eq!(result, "v_1 = a_1 + a_0");
    }
}
