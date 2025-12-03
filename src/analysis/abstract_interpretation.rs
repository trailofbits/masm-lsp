//! Abstract interpretation framework for stack machine decompilation.
//!
//! This module provides a principled approach to analyzing stack-manipulating
//! loops using abstract interpretation with fixed-point computation.
//!
//! # Key Concepts
//!
//! - **Abstract Domain**: Values are represented symbolically, tracking their
//!   origin (input position) and how they were computed.
//! - **Transfer Functions**: Each instruction has a function that transforms
//!   the abstract state.
//! - **Fixed-Point Computation**: Loops are analyzed by iterating until the
//!   abstract state stabilizes.
//! - **Widening/Narrowing**: For unbounded loops, widening ensures termination
//!   while narrowing can recover precision.
//!
//! # Example
//!
//! ```text
//! repeat.5
//!     movup.5
//!     add
//!     movdn.4
//! end
//! ```
//!
//! The abstract interpretation determines:
//! - Minimum inputs required: 10
//! - Net effect per iteration: -1 (consuming)
//! - Access pattern: `a_i + a_(i+5)` for each iteration `i`

use std::fmt;

use miden_assembly_syntax::ast::{Block, Instruction, InvocationTarget, Op};

use super::contracts::{ContractStore, StackEffect};
use super::static_effect::StackLike;
use crate::symbol_resolution::SymbolResolver;

// ═══════════════════════════════════════════════════════════════════════════════
// Loop Support Types
// ═══════════════════════════════════════════════════════════════════════════════

/// A single loop term in a parametric expression.
///
/// Represents `stride * counter` where counter is identified by loop_depth.
/// Example: For `a_(5-4*i-j)`, we have two terms:
/// - LoopTerm { stride: -4, loop_depth: 0 } for `-4*i`
/// - LoopTerm { stride: -1, loop_depth: 1 } for `-j`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LoopTerm {
    /// Coefficient of the loop counter (negative for decrementing access)
    pub stride: i32,
    /// Which loop counter (0 = outermost, 1 = next inner, etc.)
    pub loop_depth: usize,
}

impl LoopTerm {
    /// Create a new loop term.
    pub fn new(stride: i32, loop_depth: usize) -> Self {
        Self { stride, loop_depth }
    }
}

/// Information about an active loop during abstract interpretation.
#[derive(Clone, Debug)]
pub struct LoopInfo {
    /// Net stack effect per iteration (negative = consuming)
    pub net_effect: i32,
    /// Stack depth when the loop was entered
    pub entry_depth: usize,
}

impl LoopInfo {
    /// Create loop info for a loop with the given net effect.
    pub fn new(net_effect: i32, entry_depth: usize) -> Self {
        Self {
            net_effect,
            entry_depth,
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// Abstract Domain: Symbolic Stack Values
// ═══════════════════════════════════════════════════════════════════════════════

/// A symbolic expression representing a stack value's computation.
///
/// This forms the core of our abstract domain, allowing us to track
/// how values are derived from inputs through operations.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum SymbolicExpr {
    /// An input parameter at a fixed position: `a_0`, `a_1`, etc.
    Input(usize),

    /// An input at a position that depends on one or more loop counters.
    /// Represents `a_(base + sum(stride_i * counter_i))`.
    ///
    /// # Examples
    /// - Single loop: `a_(3-i)` → base=3, loop_terms=[{stride:-1, loop_depth:0}]
    /// - Nested loops: `a_(5-4*i-j)` → base=5, loop_terms=[{-4,0}, {-1,1}]
    ParametricInput {
        /// Base input index (constant part)
        base: i32,
        /// Loop terms - each represents dependency on one loop counter.
        /// Empty vec is invalid; use Input(n) instead.
        loop_terms: Vec<LoopTerm>,
    },

    /// A constant literal value.
    Constant(u64),

    /// Binary operation on two expressions.
    BinaryOp {
        op: BinaryOpKind,
        left: Box<SymbolicExpr>,
        right: Box<SymbolicExpr>,
    },

    /// Unary operation on an expression.
    UnaryOp {
        op: UnaryOpKind,
        operand: Box<SymbolicExpr>,
    },

    /// A value loaded from memory at an address.
    MemoryLoad { address: Box<SymbolicExpr> },

    /// A value from the advice provider (untrusted input).
    Advice,

    /// Unknown value - we've lost track of what this is.
    /// This is the top element (⊤) of our lattice.
    Top,
}

/// Binary operation kinds for symbolic expressions.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BinaryOpKind {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Xor,
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
}

/// Unary operation kinds for symbolic expressions.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum UnaryOpKind {
    Neg,
    Not,
    Inv,
}

impl SymbolicExpr {
    /// Create an input expression for the given position.
    pub fn input(pos: usize) -> Self {
        SymbolicExpr::Input(pos)
    }

    /// Create a parametric input expression with a single loop term.
    ///
    /// This is the backward-compatible constructor for single-loop cases.
    ///
    /// # Example
    /// ```ignore
    /// let expr = SymbolicExpr::parametric(3, -1, 0);  // a_(3-i)
    /// ```
    pub fn parametric(base: i32, stride: i32, loop_depth: usize) -> Self {
        SymbolicExpr::ParametricInput {
            base,
            loop_terms: vec![LoopTerm::new(stride, loop_depth)],
        }
    }

    /// Create a parametric input expression with multiple loop terms.
    ///
    /// # Example
    /// ```ignore
    /// let expr = SymbolicExpr::parametric_multi(5, vec![
    ///     LoopTerm::new(-4, 0),  // outer loop: -4*i
    ///     LoopTerm::new(-1, 1),  // inner loop: -j
    /// ]);  // Represents a_(5-4*i-j)
    /// ```
    pub fn parametric_multi(base: i32, loop_terms: Vec<LoopTerm>) -> Self {
        debug_assert!(!loop_terms.is_empty(), "Use Input for non-parametric");
        SymbolicExpr::ParametricInput { base, loop_terms }
    }

    /// Create a constant expression.
    pub fn constant(value: u64) -> Self {
        SymbolicExpr::Constant(value)
    }

    /// Create a binary operation expression.
    pub fn binary(op: BinaryOpKind, left: SymbolicExpr, right: SymbolicExpr) -> Self {
        SymbolicExpr::BinaryOp {
            op,
            left: Box::new(left),
            right: Box::new(right),
        }
    }

    /// Check if this expression is the top element.
    pub fn is_top(&self) -> bool {
        matches!(self, SymbolicExpr::Top)
    }

    /// Check if this is a simple input reference.
    pub fn as_input(&self) -> Option<usize> {
        match self {
            SymbolicExpr::Input(i) => Some(*i),
            _ => None,
        }
    }

    /// Extract single-term parametric components (backward compatibility).
    ///
    /// Returns `Some((base, stride, loop_depth))` only for single-term cases.
    /// Returns `None` for multi-term parametric inputs or non-parametric expressions.
    pub fn as_parametric(&self) -> Option<(i32, i32, usize)> {
        match self {
            SymbolicExpr::ParametricInput { base, loop_terms } if loop_terms.len() == 1 => {
                let term = &loop_terms[0];
                Some((*base, term.stride, term.loop_depth))
            }
            _ => None,
        }
    }

    /// Extract all parametric components.
    ///
    /// Returns `Some((base, &loop_terms))` for any parametric input.
    pub fn as_parametric_multi(&self) -> Option<(i32, &[LoopTerm])> {
        match self {
            SymbolicExpr::ParametricInput { base, loop_terms } => Some((*base, loop_terms)),
            _ => None,
        }
    }

    /// Add a loop term to this expression.
    ///
    /// Used when entering a loop to lift expressions to parametric form.
    /// Recursively transforms nested expressions (BinaryOp, UnaryOp).
    ///
    /// # Example
    /// ```ignore
    /// let input = SymbolicExpr::Input(3);
    /// let parametric = input.add_loop_term(-1, 0);  // a_(3-i)
    /// let nested = parametric.add_loop_term(-1, 1);  // a_(3-i-j)
    /// ```
    pub fn add_loop_term(&self, stride: i32, loop_depth: usize) -> Self {
        match self {
            // Input(n) → ParametricInput with one term
            SymbolicExpr::Input(n) => SymbolicExpr::ParametricInput {
                base: *n as i32,
                loop_terms: vec![LoopTerm::new(stride, loop_depth)],
            },

            // ParametricInput → add another term
            SymbolicExpr::ParametricInput { base, loop_terms } => {
                let mut new_terms = loop_terms.clone();
                new_terms.push(LoopTerm::new(stride, loop_depth));
                SymbolicExpr::ParametricInput {
                    base: *base,
                    loop_terms: new_terms,
                }
            }

            // BinaryOp → recursively transform operands
            SymbolicExpr::BinaryOp { op, left, right } => SymbolicExpr::BinaryOp {
                op: *op,
                left: Box::new(left.add_loop_term(stride, loop_depth)),
                right: Box::new(right.add_loop_term(stride, loop_depth)),
            },

            // UnaryOp → recursively transform operand
            SymbolicExpr::UnaryOp { op, operand } => SymbolicExpr::UnaryOp {
                op: *op,
                operand: Box::new(operand.add_loop_term(stride, loop_depth)),
            },

            // MemoryLoad → recursively transform address
            SymbolicExpr::MemoryLoad { address } => SymbolicExpr::MemoryLoad {
                address: Box::new(address.add_loop_term(stride, loop_depth)),
            },

            // Constants, Advice, Top pass through unchanged
            _ => self.clone(),
        }
    }

    /// Lift a concrete input to parametric form for loop analysis.
    ///
    /// `a_n` becomes `a_(n + i * stride)` where `stride` is the negated net effect.
    /// This is a convenience method that calls `add_loop_term` with negated net effect.
    /// The negation accounts for the stack "sliding" as items are consumed.
    pub fn lift_to_parametric(&self, net_effect: i32, loop_depth: usize) -> Self {
        self.add_loop_term(-net_effect, loop_depth)
    }

    /// True lattice join (least upper bound).
    ///
    /// Returns the most general expression that covers both inputs.
    /// This is a sound approximation - the result may be less precise
    /// than either input, but never incorrect.
    pub fn join(&self, other: &Self) -> Self {
        if self == other {
            return self.clone();
        }

        match (self, other) {
            // Top absorbs everything
            (SymbolicExpr::Top, _) | (_, SymbolicExpr::Top) => SymbolicExpr::Top,

            // Same constants are equal (handled above), different constants -> Top
            (SymbolicExpr::Constant(_), SymbolicExpr::Constant(_)) => SymbolicExpr::Top,

            // Same inputs
            (SymbolicExpr::Input(a), SymbolicExpr::Input(b)) if a == b => self.clone(),

            // Parametric inputs can be joined if they have identical structure
            (
                SymbolicExpr::ParametricInput {
                    base: b1,
                    loop_terms: t1,
                },
                SymbolicExpr::ParametricInput {
                    base: b2,
                    loop_terms: t2,
                },
            ) if b1 == b2 && t1 == t2 => self.clone(),

            // Binary ops with same structure can join recursively
            (
                SymbolicExpr::BinaryOp {
                    op: op1,
                    left: l1,
                    right: r1,
                },
                SymbolicExpr::BinaryOp {
                    op: op2,
                    left: l2,
                    right: r2,
                },
            ) if op1 == op2 => {
                let left = l1.join(l2);
                let right = r1.join(r2);
                if left.is_top() || right.is_top() {
                    SymbolicExpr::Top
                } else {
                    SymbolicExpr::BinaryOp {
                        op: *op1,
                        left: Box::new(left),
                        right: Box::new(right),
                    }
                }
            }

            // Default: incompatible expressions -> Top (sound approximation)
            _ => SymbolicExpr::Top,
        }
    }

    /// Heuristic pattern inference (NOT a lattice join).
    ///
    /// Used for loop analysis to infer parametric patterns.
    /// Unlike `join`, this may produce MORE SPECIFIC results than inputs,
    /// which is useful for loop analysis but not sound for general merging.
    ///
    /// Example: `infer_pattern(Input(0), Input(1))` -> `ParametricInput { base: 0, stride: 1 }`
    pub fn infer_pattern(&self, other: &Self, loop_depth: usize) -> Self {
        if self == other {
            return self.clone();
        }

        match (self, other) {
            // Two different inputs might form a linear pattern
            (SymbolicExpr::Input(a), SymbolicExpr::Input(b)) => {
                let diff = (*b as i32) - (*a as i32);
                SymbolicExpr::ParametricInput {
                    base: *a as i32,
                    loop_terms: vec![LoopTerm::new(diff, loop_depth)],
                }
            }

            // Parametric with same structure but shifted base
            (
                SymbolicExpr::ParametricInput {
                    base: b1,
                    loop_terms: t1,
                },
                SymbolicExpr::ParametricInput {
                    base: b2,
                    loop_terms: t2,
                },
            ) if t1 == t2 => {
                let stride = b2 - b1;
                // Add another loop term for the new loop level
                let mut new_terms = t1.clone();
                new_terms.push(LoopTerm::new(stride, loop_depth));
                SymbolicExpr::ParametricInput {
                    base: *b1,
                    loop_terms: new_terms,
                }
            }

            // Binary ops: try to infer patterns in operands
            (
                SymbolicExpr::BinaryOp {
                    op: op1,
                    left: l1,
                    right: r1,
                },
                SymbolicExpr::BinaryOp {
                    op: op2,
                    left: l2,
                    right: r2,
                },
            ) if op1 == op2 => {
                let left = l1.infer_pattern(l2, loop_depth);
                let right = r1.infer_pattern(r2, loop_depth);
                SymbolicExpr::BinaryOp {
                    op: *op1,
                    left: Box::new(left),
                    right: Box::new(right),
                }
            }

            // Fall back to true join for incompatible patterns
            _ => self.join(other),
        }
    }

    /// Format as a human-readable string for pseudocode generation.
    ///
    /// For single-term parametric expressions, uses the provided counter_name.
    /// For multi-term parametric expressions, uses default counter names (i, j, k, ...).
    ///
    /// # Simplifications
    /// - `a_(i)` → `a_i` (base=0, stride=1)
    /// - `a_(3+i)` instead of `a_(3+1*i)` (stride=1 omits coefficient)
    /// - `a_(3*i)` instead of `a_(0+3*i)` (base=0 is omitted)
    pub fn to_pseudocode(&self, counter_name: &str) -> String {
        match self {
            SymbolicExpr::Input(n) => format!("a_{}", n),
            SymbolicExpr::ParametricInput { base, loop_terms } => {
                if loop_terms.is_empty() {
                    return format!("a_{}", base);
                }

                // For single-term, use the provided counter_name
                if loop_terms.len() == 1 {
                    let stride = loop_terms[0].stride;
                    if stride == 0 {
                        format!("a_{}", base)
                    } else if *base == 0 {
                        // Simplify: a_(i) → a_i for stride=1
                        if stride == 1 {
                            format!("a_{}", counter_name)
                        } else if stride == -1 {
                            format!("a_(-{})", counter_name)
                        } else if stride > 0 {
                            // Coefficient first: a_(3*i) not a_(i*3)
                            format!("a_({}*{})", stride, counter_name)
                        } else {
                            format!("a_(-{}*{})", -stride, counter_name)
                        }
                    } else if stride == 1 {
                        // Simplify: a_(3+i) not a_(3+1*i)
                        format!("a_({}+{})", base, counter_name)
                    } else if stride == -1 {
                        // Simplify: a_(3-i) not a_(3-1*i)
                        format!("a_({}-{})", base, counter_name)
                    } else if stride > 0 {
                        format!("a_({}+{}*{})", base, stride, counter_name)
                    } else {
                        format!("a_({}-{}*{})", base, -stride, counter_name)
                    }
                } else {
                    // Multi-term: use default counter names
                    let counter_names: Vec<&str> = vec!["i", "j", "k", "l", "m", "n"];
                    self.to_pseudocode_multi(&counter_names)
                }
            }
            SymbolicExpr::Constant(v) => format!("{}", v),
            SymbolicExpr::BinaryOp { op, left, right } => {
                let left_str = left.to_pseudocode(counter_name);
                let right_str = right.to_pseudocode(counter_name);
                let op_str = match op {
                    BinaryOpKind::Add => "+",
                    BinaryOpKind::Sub => "-",
                    BinaryOpKind::Mul => "*",
                    BinaryOpKind::Div => "/",
                    BinaryOpKind::And => "&",
                    BinaryOpKind::Or => "|",
                    BinaryOpKind::Xor => "^",
                    BinaryOpKind::Eq => "==",
                    BinaryOpKind::Neq => "!=",
                    BinaryOpKind::Lt => "<",
                    BinaryOpKind::Lte => "<=",
                    BinaryOpKind::Gt => ">",
                    BinaryOpKind::Gte => ">=",
                };
                format!("{} {} {}", left_str, op_str, right_str)
            }
            SymbolicExpr::UnaryOp { op, operand } => {
                let operand_str = operand.to_pseudocode(counter_name);
                match op {
                    UnaryOpKind::Neg => format!("-{}", operand_str),
                    UnaryOpKind::Not => format!("!{}", operand_str),
                    UnaryOpKind::Inv => format!("1/{}", operand_str),
                }
            }
            SymbolicExpr::MemoryLoad { address } => {
                format!("mem[{}]", address.to_pseudocode(counter_name))
            }
            SymbolicExpr::Advice => "advice".to_string(),
            SymbolicExpr::Top => "?".to_string(),
        }
    }

    /// Format as pseudocode with multiple counter names.
    ///
    /// # Arguments
    /// * `counter_names` - Counter names indexed by loop_depth (0 = outermost)
    ///
    /// # Simplifications
    /// - `a_(i)` → `a_i` (single term, base=0, stride=1)
    /// - `a_(3+i)` instead of `a_(3+1*i)` (stride=1 omits coefficient)
    /// - `a_(3*i)` instead of `a_(0+3*i)` (base=0 is omitted)
    ///
    /// # Example
    /// ```ignore
    /// let expr = SymbolicExpr::parametric_multi(5, vec![
    ///     LoopTerm::new(-4, 0),
    ///     LoopTerm::new(-1, 1),
    /// ]);
    /// assert_eq!(expr.to_pseudocode_multi(&["i", "j"]), "a_(5-4*i-j)");
    /// ```
    pub fn to_pseudocode_multi(&self, counter_names: &[&str]) -> String {
        match self {
            SymbolicExpr::ParametricInput { base, loop_terms } => {
                if loop_terms.is_empty() {
                    return format!("a_{}", base);
                }

                // Special case: single term with base=0 and stride=1 → a_i
                if loop_terms.len() == 1 && *base == 0 && loop_terms[0].stride == 1 {
                    let counter = counter_names
                        .get(loop_terms[0].loop_depth)
                        .copied()
                        .unwrap_or("?");
                    return format!("a_{}", counter);
                }

                // Emit positive loop terms first for readability, then the base constant,
                // followed by negative loop terms. This yields forms like `a_(i+5)` instead
                // of `a_(5+i)` while still producing `a_(5-4*i-j)` for negative strides.
                let mut positive_terms = Vec::new();
                let mut negative_terms = Vec::new();
                for term in loop_terms {
                    let counter = counter_names.get(term.loop_depth).copied().unwrap_or("?");
                    let abs_stride = term.stride.abs();
                    let formatted = if abs_stride == 1 {
                        counter.to_string()
                    } else {
                        format!("{}*{}", abs_stride, counter)
                    };
                    if term.stride >= 0 {
                        positive_terms.push(formatted);
                    } else {
                        negative_terms.push(formatted);
                    }
                }

                let mut parts = Vec::new();
                if let Some(first) = positive_terms.first() {
                    parts.push(first.clone());
                    for term in positive_terms.iter().skip(1) {
                        parts.push(format!("+{}", term));
                    }
                }

                if *base != 0 {
                    let base_abs = base.abs();
                    if *base > 0 {
                        let prefix = if parts.is_empty() { "" } else { "+" };
                        parts.push(format!("{prefix}{base_abs}"));
                    } else {
                        parts.push(format!("-{}", base_abs));
                    }
                }

                for term in negative_terms {
                    parts.push(format!("-{}", term));
                }

                let expr = parts.join("");
                let expr = expr.trim_start_matches('+');
                format!("a_({})", expr)
            }

            // For other types, delegate to single-counter version
            _ => self.to_pseudocode(counter_names.first().copied().unwrap_or("i")),
        }
    }
}

impl fmt::Display for SymbolicExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_pseudocode("i"))
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// Abstract State: Symbolic Stack
// ═══════════════════════════════════════════════════════════════════════════════

/// Abstract state representing the symbolic stack during analysis.
#[derive(Clone, Debug)]
pub struct AbstractState {
    /// The symbolic stack (index 0 = bottom, last = top).
    stack: Vec<SymbolicExpr>,

    /// Counter for generating temporary variable names.
    next_temp_id: usize,

    /// Number of inputs that have been discovered.
    discovered_inputs: usize,

    /// Stack of active loops (index 0 = outermost).
    /// Replaces the old `loop_depth: usize` field.
    loop_stack: Vec<LoopInfo>,

    /// Whether we've encountered an operation that makes tracking impossible.
    tracking_failed: bool,

    /// Reason for tracking failure, if any.
    failure_reason: Option<String>,
}

impl AbstractState {
    /// Create a new abstract state with the given number of inputs.
    pub fn new(input_count: usize) -> Self {
        let mut stack = Vec::with_capacity(input_count);
        // Push inputs so that a_0 is at the top (last in Vec)
        for i in (0..input_count).rev() {
            stack.push(SymbolicExpr::Input(i));
        }
        Self {
            stack,
            next_temp_id: 0,
            discovered_inputs: input_count,
            loop_stack: Vec::new(),
            tracking_failed: false,
            failure_reason: None,
        }
    }

    /// Create an empty abstract state.
    pub fn empty() -> Self {
        Self {
            stack: Vec::new(),
            next_temp_id: 0,
            discovered_inputs: 0,
            loop_stack: Vec::new(),
            tracking_failed: false,
            failure_reason: None,
        }
    }

    /// Get the current stack depth.
    pub fn depth(&self) -> usize {
        self.stack.len()
    }

    /// Get the number of discovered inputs.
    pub fn inputs_discovered(&self) -> usize {
        self.discovered_inputs
    }

    /// Check if tracking has failed.
    pub fn has_failed(&self) -> bool {
        self.tracking_failed
    }

    /// Get the failure reason, if any.
    pub fn failure_reason(&self) -> Option<&str> {
        self.failure_reason.as_deref()
    }

    // Note: push, pop, peek, ensure_depth, dup, swap, movup, movdn are provided
    // by the StackLike trait implementation below.

    /// Mark tracking as failed with a reason.
    pub fn fail(&mut self, reason: &str) {
        self.tracking_failed = true;
        self.failure_reason = Some(reason.to_string());
    }

    // ═══════════════════════════════════════════════════════════════════════
    // Loop Context Methods
    // ═══════════════════════════════════════════════════════════════════════

    /// Enter a new loop context.
    ///
    /// Call this when encountering a loop (repeat, while.true) with
    /// the per-iteration net stack effect.
    ///
    /// # Arguments
    /// * `net_effect` - Net stack change per iteration (negative = consuming)
    ///
    /// # Returns
    /// The loop depth after entering (1 for first loop, 2 for nested, etc.)
    pub fn enter_loop(&mut self, net_effect: i32) -> usize {
        let entry_depth = self.stack.len();
        self.loop_stack.push(LoopInfo::new(net_effect, entry_depth));
        self.loop_stack.len()
    }

    /// Exit the current (innermost) loop context.
    ///
    /// # Returns
    /// The `LoopInfo` for the exited loop, or `None` if not in a loop.
    pub fn exit_loop(&mut self) -> Option<LoopInfo> {
        self.loop_stack.pop()
    }

    /// Get the current loop nesting depth.
    ///
    /// Returns 0 when not inside any loop.
    pub fn current_loop_depth(&self) -> usize {
        self.loop_stack.len()
    }

    /// Check if currently inside a loop.
    pub fn in_loop(&self) -> bool {
        !self.loop_stack.is_empty()
    }

    /// Get information about the current (innermost) loop.
    pub fn current_loop(&self) -> Option<&LoopInfo> {
        self.loop_stack.last()
    }

    /// Get information about a loop at a specific depth.
    ///
    /// # Arguments
    /// * `depth` - Loop depth (0 = outermost, 1 = next inner, etc.)
    pub fn loop_at_depth(&self, depth: usize) -> Option<&LoopInfo> {
        self.loop_stack.get(depth)
    }

    /// Lift all stack expressions to parametric form for the current loop.
    ///
    /// Call this after entering a loop to transform all existing stack
    /// expressions to account for the loop's effect.
    ///
    /// # Arguments
    /// * `net_effect` - Net stack change per iteration
    pub fn lift_for_current_loop(&mut self, net_effect: i32) {
        let depth = self.current_loop_depth().saturating_sub(1);
        for expr in &mut self.stack {
            *expr = expr.add_loop_term(-net_effect, depth);
        }
    }

    /// Lift all stack expressions to parametric form for a specific loop depth.
    pub fn lift_for_loop(&mut self, net_effect: i32, loop_depth: usize) {
        for expr in &mut self.stack {
            *expr = expr.add_loop_term(-net_effect, loop_depth);
        }
    }

    // ═══════════════════════════════════════════════════════════════════════
    // State Operations
    // ═══════════════════════════════════════════════════════════════════════

    /// Join two abstract states (least upper bound).
    ///
    /// Used at control flow merge points.
    pub fn join(&self, other: &Self) -> Self {
        if self.tracking_failed || other.tracking_failed {
            let mut result = Self::empty();
            result.tracking_failed = true;
            result.failure_reason = self
                .failure_reason
                .clone()
                .or_else(|| other.failure_reason.clone());
            return result;
        }

        let min_depth = self.stack.len().min(other.stack.len());
        let mut joined_stack = Vec::with_capacity(min_depth);

        // Join corresponding positions from the bottom
        for i in 0..min_depth {
            let self_idx = self.stack.len() - min_depth + i;
            let other_idx = other.stack.len() - min_depth + i;
            joined_stack.push(self.stack[self_idx].join(&other.stack[other_idx]));
        }

        Self {
            stack: joined_stack,
            next_temp_id: self.next_temp_id.max(other.next_temp_id),
            discovered_inputs: self.discovered_inputs.max(other.discovered_inputs),
            loop_stack: self.loop_stack.clone(),
            tracking_failed: false,
            failure_reason: None,
        }
    }

    /// Check if this state is equivalent to another modulo variable names.
    ///
    /// Used for fixed-point detection in loops.
    pub fn equivalent_structure(&self, other: &Self) -> bool {
        if self.stack.len() != other.stack.len() {
            return false;
        }

        // For now, use simple equality
        // A more sophisticated implementation would check structural equivalence
        self.stack == other.stack
    }

    /// Lift all input references to parametric form.
    ///
    /// Used when entering a loop to make expressions depend on the loop counter.
    pub fn lift_to_parametric(&mut self, net_effect: i32) {
        let loop_depth = self.current_loop_depth();
        for expr in &mut self.stack {
            *expr = expr.lift_to_parametric(net_effect, loop_depth);
        }
    }

    /// Get a snapshot of the current state for comparison.
    pub fn snapshot(&self) -> AbstractStateSnapshot {
        AbstractStateSnapshot {
            stack: self.stack.clone(),
            discovered_inputs: self.discovered_inputs,
        }
    }

    /// Restore from a snapshot.
    pub fn restore(&mut self, snapshot: &AbstractStateSnapshot) {
        self.stack = snapshot.stack.clone();
        // Don't restore discovered_inputs - we want to keep tracking new discoveries
    }
}

// Implement StackLike trait for AbstractState
impl super::static_effect::StackLike for AbstractState {
    type Element = SymbolicExpr;

    fn depth(&self) -> usize {
        self.stack.len()
    }

    fn push(&mut self, elem: SymbolicExpr) {
        self.stack.push(elem);
    }

    fn pop(&mut self) -> SymbolicExpr {
        if let Some(expr) = self.stack.pop() {
            expr
        } else {
            // Stack underflow - discover a new input
            let input_id = self.discovered_inputs;
            self.discovered_inputs += 1;
            SymbolicExpr::Input(input_id)
        }
    }

    fn peek(&self, n: usize) -> Option<&SymbolicExpr> {
        if n < self.stack.len() {
            Some(&self.stack[self.stack.len() - 1 - n])
        } else {
            None
        }
    }

    fn ensure_depth(&mut self, needed: usize) {
        while self.stack.len() < needed {
            let input_id = self.discovered_inputs;
            self.discovered_inputs += 1;
            // Insert at the bottom (older inputs have higher indices)
            self.stack.insert(0, SymbolicExpr::Input(input_id));
        }
    }

    fn swap(&mut self, a: usize, b: usize) {
        let max_pos = a.max(b);
        self.ensure_depth(max_pos + 1);
        let len = self.stack.len();
        let idx_a = len - 1 - a;
        let idx_b = len - 1 - b;
        self.stack.swap(idx_a, idx_b);
    }

    fn movup(&mut self, n: usize) {
        if n == 0 {
            return;
        }
        self.ensure_depth(n + 1);
        let len = self.stack.len();
        let idx = len - 1 - n;
        let elem = self.stack.remove(idx);
        self.stack.push(elem);
    }

    fn movdn(&mut self, n: usize) {
        if n == 0 {
            return;
        }
        self.ensure_depth(n + 1);
        let elem = self.stack.pop().unwrap();
        let len = self.stack.len();
        // Position n from top in final stack (len+1 elements) = index (len - n)
        let idx = len - n;
        self.stack.insert(idx, elem);
    }
}

/// A lightweight snapshot of abstract state for comparison and restoration.
#[derive(Clone, Debug)]
pub struct AbstractStateSnapshot {
    stack: Vec<SymbolicExpr>,
    discovered_inputs: usize,
}

impl AbstractStateSnapshot {
    /// Get the stack depth at the snapshot.
    pub fn depth(&self) -> usize {
        self.stack.len()
    }

    /// Get the number of inputs discovered at the snapshot.
    pub fn inputs_discovered(&self) -> usize {
        self.discovered_inputs
    }
}

impl Default for AbstractStateSnapshot {
    fn default() -> Self {
        Self {
            stack: Vec::new(),
            discovered_inputs: 0,
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// Transfer Functions
// ═══════════════════════════════════════════════════════════════════════════════

/// Try to resolve a procedure call's stack effect from the contract store.
///
/// Returns `Some((inputs, outputs))` if the procedure has a known stack effect,
/// `None` otherwise.
fn resolve_procedure_call_effect(
    target: &InvocationTarget,
    resolver: Option<&SymbolResolver>,
    contracts: Option<&ContractStore>,
) -> Option<(usize, usize)> {
    let store = contracts?;
    let resolver = resolver?;

    // MAST roots can't be resolved symbolically
    if matches!(target, InvocationTarget::MastRoot(_)) {
        return None;
    }

    // Use the unified symbol resolution to get the fully-qualified path
    let resolved_path = resolver.resolve_target(target)?;
    let contract = store.get(&resolved_path)?;

    match &contract.stack_effect {
        StackEffect::Known { inputs, outputs } => Some((*inputs, *outputs)),
        _ => None,
    }
}

/// Apply the transfer function for an instruction to the abstract state.
///
/// Returns a description of the operation for pseudocode generation, if any.
///
/// When a `SymbolResolver` and `ContractStore` are provided, procedure calls can
/// be resolved to their stack effects, allowing the analysis to continue past
/// call sites instead of failing with "unknown stack effect".
pub fn transfer_instruction(
    inst: &Instruction,
    state: &mut AbstractState,
    resolver: Option<&SymbolResolver>,
    contracts: Option<&ContractStore>,
) -> Option<TransferResult> {
    let event = apply_transfer_event(inst, state, resolver, contracts);
    event.to_result()
}

/// Apply the transfer function for an instruction to the abstract state (state only).
pub fn apply_transfer_event(
    inst: &Instruction,
    state: &mut AbstractState,
    resolver: Option<&SymbolResolver>,
    contracts: Option<&ContractStore>,
) -> TransferEvent {
    if state.tracking_failed {
        return TransferEvent::None;
    }

    match inst {
        // ─────────────────────────────────────────────────────────────────────
        // Stack manipulation
        // ─────────────────────────────────────────────────────────────────────
        Instruction::Drop => {
            state.pop();
            TransferEvent::None
        }
        Instruction::DropW => {
            for _ in 0..4 {
                state.pop();
            }
            TransferEvent::None
        }

        Instruction::Dup0 => transfer_dup_event(state, 0),
        Instruction::Dup1 => transfer_dup_event(state, 1),
        Instruction::Dup2 => transfer_dup_event(state, 2),
        Instruction::Dup3 => transfer_dup_event(state, 3),
        Instruction::Dup4 => transfer_dup_event(state, 4),
        Instruction::Dup5 => transfer_dup_event(state, 5),
        Instruction::Dup6 => transfer_dup_event(state, 6),
        Instruction::Dup7 => transfer_dup_event(state, 7),
        Instruction::Dup8 => transfer_dup_event(state, 8),
        Instruction::Dup9 => transfer_dup_event(state, 9),
        Instruction::Dup10 => transfer_dup_event(state, 10),
        Instruction::Dup11 => transfer_dup_event(state, 11),
        Instruction::Dup12 => transfer_dup_event(state, 12),
        Instruction::Dup13 => transfer_dup_event(state, 13),
        Instruction::Dup14 => transfer_dup_event(state, 14),
        Instruction::Dup15 => transfer_dup_event(state, 15),

        Instruction::Swap1 => {
            state.swap(0, 1);
            TransferEvent::None
        }
        Instruction::Swap2 => {
            state.swap(0, 2);
            TransferEvent::None
        }
        Instruction::Swap3 => {
            state.swap(0, 3);
            TransferEvent::None
        }
        Instruction::Swap4 => {
            state.swap(0, 4);
            TransferEvent::None
        }
        Instruction::Swap5 => {
            state.swap(0, 5);
            TransferEvent::None
        }
        Instruction::Swap6 => {
            state.swap(0, 6);
            TransferEvent::None
        }
        Instruction::Swap7 => {
            state.swap(0, 7);
            TransferEvent::None
        }
        Instruction::Swap8 => {
            state.swap(0, 8);
            TransferEvent::None
        }
        Instruction::Swap9 => {
            state.swap(0, 9);
            TransferEvent::None
        }
        Instruction::Swap10 => {
            state.swap(0, 10);
            TransferEvent::None
        }
        Instruction::Swap11 => {
            state.swap(0, 11);
            TransferEvent::None
        }
        Instruction::Swap12 => {
            state.swap(0, 12);
            TransferEvent::None
        }
        Instruction::Swap13 => {
            state.swap(0, 13);
            TransferEvent::None
        }
        Instruction::Swap14 => {
            state.swap(0, 14);
            TransferEvent::None
        }
        Instruction::Swap15 => {
            state.swap(0, 15);
            TransferEvent::None
        }

        Instruction::MovUp2 => {
            state.movup(2);
            TransferEvent::None
        }
        Instruction::MovUp3 => {
            state.movup(3);
            TransferEvent::None
        }
        Instruction::MovUp4 => {
            state.movup(4);
            TransferEvent::None
        }
        Instruction::MovUp5 => {
            state.movup(5);
            TransferEvent::None
        }
        Instruction::MovUp6 => {
            state.movup(6);
            TransferEvent::None
        }
        Instruction::MovUp7 => {
            state.movup(7);
            TransferEvent::None
        }
        Instruction::MovUp8 => {
            state.movup(8);
            TransferEvent::None
        }
        Instruction::MovUp9 => {
            state.movup(9);
            TransferEvent::None
        }
        Instruction::MovUp10 => {
            state.movup(10);
            TransferEvent::None
        }
        Instruction::MovUp11 => {
            state.movup(11);
            TransferEvent::None
        }
        Instruction::MovUp12 => {
            state.movup(12);
            TransferEvent::None
        }
        Instruction::MovUp13 => {
            state.movup(13);
            TransferEvent::None
        }
        Instruction::MovUp14 => {
            state.movup(14);
            TransferEvent::None
        }
        Instruction::MovUp15 => {
            state.movup(15);
            TransferEvent::None
        }

        Instruction::MovDn2 => {
            state.movdn(2);
            TransferEvent::None
        }
        Instruction::MovDn3 => {
            state.movdn(3);
            TransferEvent::None
        }
        Instruction::MovDn4 => {
            state.movdn(4);
            TransferEvent::None
        }
        Instruction::MovDn5 => {
            state.movdn(5);
            TransferEvent::None
        }
        Instruction::MovDn6 => {
            state.movdn(6);
            TransferEvent::None
        }
        Instruction::MovDn7 => {
            state.movdn(7);
            TransferEvent::None
        }
        Instruction::MovDn8 => {
            state.movdn(8);
            TransferEvent::None
        }
        Instruction::MovDn9 => {
            state.movdn(9);
            TransferEvent::None
        }
        Instruction::MovDn10 => {
            state.movdn(10);
            TransferEvent::None
        }
        Instruction::MovDn11 => {
            state.movdn(11);
            TransferEvent::None
        }
        Instruction::MovDn12 => {
            state.movdn(12);
            TransferEvent::None
        }
        Instruction::MovDn13 => {
            state.movdn(13);
            TransferEvent::None
        }
        Instruction::MovDn14 => {
            state.movdn(14);
            TransferEvent::None
        }
        Instruction::MovDn15 => {
            state.movdn(15);
            TransferEvent::None
        }

        // ─────────────────────────────────────────────────────────────────────
        // Arithmetic operations
        // ─────────────────────────────────────────────────────────────────────
        Instruction::Add => transfer_binary_op_event(state, BinaryOpKind::Add),
        Instruction::Sub => transfer_binary_op_event(state, BinaryOpKind::Sub),
        Instruction::Mul => transfer_binary_op_event(state, BinaryOpKind::Mul),
        Instruction::Div => transfer_binary_op_event(state, BinaryOpKind::Div),
        Instruction::Neg => transfer_unary_op_event(state, UnaryOpKind::Neg),
        Instruction::Inv => transfer_unary_op_event(state, UnaryOpKind::Inv),

        Instruction::Incr => {
            let a = state.pop();
            let result =
                SymbolicExpr::binary(BinaryOpKind::Add, a.clone(), SymbolicExpr::Constant(1));
            state.push(result.clone());
            TransferEvent::Assignment {
                result: result.clone(),
                source: SymbolicExpr::binary(BinaryOpKind::Add, a, SymbolicExpr::Constant(1)),
            }
        }

        // ─────────────────────────────────────────────────────────────────────
        // Comparison operations
        // ─────────────────────────────────────────────────────────────────────
        Instruction::Eq => transfer_binary_op_event(state, BinaryOpKind::Eq),
        Instruction::Neq => transfer_binary_op_event(state, BinaryOpKind::Neq),
        Instruction::Lt => transfer_binary_op_event(state, BinaryOpKind::Lt),
        Instruction::Lte => transfer_binary_op_event(state, BinaryOpKind::Lte),
        Instruction::Gt => transfer_binary_op_event(state, BinaryOpKind::Gt),
        Instruction::Gte => transfer_binary_op_event(state, BinaryOpKind::Gte),

        // ─────────────────────────────────────────────────────────────────────
        // Boolean operations
        // ─────────────────────────────────────────────────────────────────────
        Instruction::And => transfer_binary_op_event(state, BinaryOpKind::And),
        Instruction::Or => transfer_binary_op_event(state, BinaryOpKind::Or),
        Instruction::Xor => transfer_binary_op_event(state, BinaryOpKind::Xor),
        Instruction::Not => transfer_unary_op_event(state, UnaryOpKind::Not),

        // ─────────────────────────────────────────────────────────────────────
        // Push operations
        // ─────────────────────────────────────────────────────────────────────
        Instruction::Push(imm) => {
            use miden_assembly_syntax::parser::PushValue;
            let value = match imm {
                miden_assembly_syntax::ast::Immediate::Value(span) => match span.inner() {
                    PushValue::Int(int_val) => {
                        use miden_assembly_syntax::parser::IntValue;
                        match int_val {
                            IntValue::U8(v) => *v as u64,
                            IntValue::U16(v) => *v as u64,
                            IntValue::U32(v) => *v as u64,
                            IntValue::Felt(f) => f.as_int(),
                        }
                    }
                    PushValue::Word(_) => 0, // Word push handled elsewhere
                },
                _ => 0,
            };
            let expr = SymbolicExpr::Constant(value);
            state.push(expr);
            TransferEvent::Push { value }
        }

        // ─────────────────────────────────────────────────────────────────────
        // Advice operations
        // ─────────────────────────────────────────────────────────────────────
        Instruction::AdvPush(n) => {
            let count = match n {
                miden_assembly_syntax::ast::Immediate::Value(v) => v.into_inner() as usize,
                _ => 1,
            };
            for _ in 0..count {
                state.push(SymbolicExpr::Advice);
            }
            TransferEvent::Advice { count }
        }

        // ─────────────────────────────────────────────────────────────────────
        // Memory operations
        // ─────────────────────────────────────────────────────────────────────
        Instruction::MemLoad => {
            let addr = state.pop();
            let result = SymbolicExpr::MemoryLoad {
                address: Box::new(addr.clone()),
            };
            state.push(result);
            TransferEvent::MemLoad { address: addr }
        }

        Instruction::MemStore => {
            let addr = state.pop();
            let value = state.pop();
            TransferEvent::MemStore {
                address: addr,
                value,
            }
        }

        // ─────────────────────────────────────────────────────────────────────
        // Static procedure calls - resolve via ContractStore if available
        // ─────────────────────────────────────────────────────────────────────
        Instruction::Exec(target) | Instruction::Call(target) | Instruction::SysCall(target) => {
            if let Some(effect) = resolve_procedure_call_effect(target, resolver, contracts) {
                // Apply the known stack effect
                for _ in 0..effect.0 {
                    state.pop();
                }
                for _ in 0..effect.1 {
                    state.push(SymbolicExpr::Top);
                }
                TransferEvent::None
            } else {
                // Can't resolve - fail tracking
                let target_name = match target {
                    InvocationTarget::Symbol(ident) => ident.as_str().to_string(),
                    InvocationTarget::Path(path) => path.inner().as_str().to_string(),
                    InvocationTarget::MastRoot(_) => "<mast_root>".to_string(),
                };
                state.fail(&format!(
                    "procedure '{}' has unknown stack effect",
                    target_name
                ));
                TransferEvent::None
            }
        }

        // ─────────────────────────────────────────────────────────────────────
        // Dynamic calls - fail tracking
        // ─────────────────────────────────────────────────────────────────────
        Instruction::DynExec | Instruction::DynCall => {
            state.fail("dynamic call has unknown stack effect");
            TransferEvent::None
        }

        // ─────────────────────────────────────────────────────────────────────
        // Default - use static stack effect if available
        // ─────────────────────────────────────────────────────────────────────
        _ => {
            use super::static_effect::StaticEffect;

            // Try to get static stack effect for the instruction
            if let Some(effect) = StaticEffect::of(inst) {
                // Pop the correct number of inputs
                for _ in 0..effect.pops {
                    state.pop();
                }
                // Push Top for each output (we don't track the semantic meaning)
                for _ in 0..effect.pushes {
                    state.push(SymbolicExpr::Top);
                }
                TransferEvent::None
            } else {
                // No static effect - instruction has dynamic stack effect
                state.fail(&format!("instruction {:?} has unknown stack effect", inst));
                TransferEvent::None
            }
        }
    }
}

/// State-only dup helper.
fn transfer_dup_event(state: &mut AbstractState, n: usize) -> TransferEvent {
    state.dup(n);
    let expr = state.peek(0).cloned().unwrap_or(SymbolicExpr::Top);
    TransferEvent::Dup {
        source_pos: n,
        value: expr,
    }
}

/// State-only binary op helper.
fn transfer_binary_op_event(state: &mut AbstractState, op: BinaryOpKind) -> TransferEvent {
    let b = state.pop();
    let a = state.pop();
    let result = SymbolicExpr::binary(op, a.clone(), b.clone());
    state.push(result.clone());

    TransferEvent::BinaryOp {
        result,
        left: a,
        op,
        right: b,
    }
}

/// State-only unary op helper.
fn transfer_unary_op_event(state: &mut AbstractState, op: UnaryOpKind) -> TransferEvent {
    let operand = state.pop();
    let result = SymbolicExpr::UnaryOp {
        op,
        operand: Box::new(operand.clone()),
    };
    state.push(result.clone());

    TransferEvent::UnaryOp {
        result,
        op,
        operand,
    }
}

/// Result of a transfer function for pseudocode generation.
#[derive(Clone, Debug)]
pub enum TransferResult {
    Assignment {
        target: String,
        source: String,
    },
    BinaryOp {
        result: String,
        left: String,
        op: String,
        right: String,
    },
    UnaryOp {
        result: String,
        op: String,
        operand: String,
    },
    Push {
        value: u64,
    },
    Dup {
        source_pos: usize,
        value: String,
    },
    Advice {
        count: usize,
    },
    MemLoad {
        address: String,
    },
    MemStore {
        address: String,
        value: String,
    },
}

/// State-only outcome of applying a transfer function.
#[derive(Clone, Debug)]
pub enum TransferEvent {
    None,
    Assignment {
        result: SymbolicExpr,
        source: SymbolicExpr,
    },
    BinaryOp {
        result: SymbolicExpr,
        left: SymbolicExpr,
        op: BinaryOpKind,
        right: SymbolicExpr,
    },
    UnaryOp {
        result: SymbolicExpr,
        op: UnaryOpKind,
        operand: SymbolicExpr,
    },
    Push {
        value: u64,
    },
    Dup {
        source_pos: usize,
        value: SymbolicExpr,
    },
    Advice {
        count: usize,
    },
    MemLoad {
        address: SymbolicExpr,
    },
    MemStore {
        address: SymbolicExpr,
        value: SymbolicExpr,
    },
}

impl TransferEvent {
    fn to_result(&self) -> Option<TransferResult> {
        match self {
            TransferEvent::None => None,
            TransferEvent::Assignment { result, source } => Some(TransferResult::Assignment {
                target: result.to_pseudocode("i"),
                source: source.to_pseudocode("i"),
            }),
            TransferEvent::BinaryOp {
                result,
                left,
                op,
                right,
            } => {
                let op_str = match op {
                    BinaryOpKind::Add => "+",
                    BinaryOpKind::Sub => "-",
                    BinaryOpKind::Mul => "*",
                    BinaryOpKind::Div => "/",
                    BinaryOpKind::And => "&",
                    BinaryOpKind::Or => "|",
                    BinaryOpKind::Xor => "^",
                    BinaryOpKind::Eq => "==",
                    BinaryOpKind::Neq => "!=",
                    BinaryOpKind::Lt => "<",
                    BinaryOpKind::Lte => "<=",
                    BinaryOpKind::Gt => ">",
                    BinaryOpKind::Gte => ">=",
                };
                Some(TransferResult::BinaryOp {
                    result: result.to_pseudocode("i"),
                    left: left.to_pseudocode("i"),
                    op: op_str.to_string(),
                    right: right.to_pseudocode("i"),
                })
            }
            TransferEvent::UnaryOp {
                result,
                op,
                operand,
            } => {
                let op_str = match op {
                    UnaryOpKind::Neg => "-",
                    UnaryOpKind::Not => "!",
                    UnaryOpKind::Inv => "1/",
                };
                Some(TransferResult::UnaryOp {
                    result: result.to_pseudocode("i"),
                    op: op_str.to_string(),
                    operand: operand.to_pseudocode("i"),
                })
            }
            TransferEvent::Push { value } => Some(TransferResult::Push { value: *value }),
            TransferEvent::Dup { source_pos, value } => Some(TransferResult::Dup {
                source_pos: *source_pos,
                value: value.to_pseudocode("i"),
            }),
            TransferEvent::Advice { count } => Some(TransferResult::Advice { count: *count }),
            TransferEvent::MemLoad { address } => Some(TransferResult::MemLoad {
                address: address.to_pseudocode("i"),
            }),
            TransferEvent::MemStore { address, value } => Some(TransferResult::MemStore {
                address: address.to_pseudocode("i"),
                value: value.to_pseudocode("i"),
            }),
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// Loop Analysis: Fixed-Point Computation
// ═══════════════════════════════════════════════════════════════════════════════

/// Result of analyzing a loop body.
#[derive(Clone, Debug)]
pub struct LoopAnalysis {
    /// Minimum number of inputs required before the loop.
    pub min_inputs_required: usize,

    /// Net stack effect per iteration (negative = consuming, positive = producing).
    pub net_effect_per_iteration: i32,

    /// Total inputs needed for the entire loop (for bounded loops).
    pub total_inputs_for_loop: Option<usize>,

    /// Whether the loop body has consistent stack behavior.
    pub is_consistent: bool,

    /// Description of the loop's computation pattern.
    pub pattern_description: Option<String>,

    /// Symbolic stack snapshot after a stable iteration (bottom → top).
    pub post_iteration_stack: Vec<SymbolicExpr>,

    /// Failure reason if analysis failed.
    pub failure_reason: Option<String>,
}

/// Maximum iterations for stack-neutral loops before we consider them
/// too complex to analyze (due to exponential expression growth).
const MAX_NEUTRAL_LOOP_ITERATIONS: usize = 16;

/// Infer parametric structure for a loop by comparing consecutive iterations.
///
/// For consuming loops we rely on `lift_for_current_loop` to add loop terms and
/// simply return the first snapshot. For neutral/producing loops we compare
/// stack slots from the top down across iterations and use `infer_pattern`
/// to attach loop terms that reflect how values shift each iteration.
fn build_parametric_stack(
    first: &[SymbolicExpr],
    second: &[SymbolicExpr],
    loop_depth: usize,
    net_effect: i32,
) -> Vec<SymbolicExpr> {
    if first.is_empty() {
        return Vec::new();
    }

    // Consuming loops already have loop terms injected via `lift_for_current_loop`.
    if net_effect < 0 {
        return first.to_vec();
    }

    let mut result = first.to_vec();
    let min_len = first.len().min(second.len());

    for i in 0..min_len {
        let idx_first = first.len() - 1 - i;
        let idx_second = second.len() - 1 - i;
        let inferred = first[idx_first].infer_pattern(&second[idx_second], loop_depth);
        result[idx_first] = inferred;
    }

    result
}

/// Analyze a repeat loop body to determine its stack effect.
///
/// This uses abstract interpretation with fixed-point computation to determine:
/// - The minimum inputs required before the loop
/// - The net stack effect per iteration
/// - The total inputs needed for the entire loop
pub fn analyze_repeat_loop(body: &Block, iteration_count: usize) -> LoopAnalysis {
    // Phase 1: Discover inputs by running one iteration from empty state
    let mut discovery_state = AbstractState::empty();
    execute_block_abstract(body, &mut discovery_state);

    if discovery_state.has_failed() {
        return LoopAnalysis {
            min_inputs_required: 0,
            net_effect_per_iteration: 0,
            total_inputs_for_loop: None,
            is_consistent: false,
            pattern_description: None,
            post_iteration_stack: Vec::new(),
            failure_reason: discovery_state.failure_reason().map(String::from),
        };
    }

    let discovered_inputs = discovery_state.inputs_discovered();

    // Phase 2: Run with discovered inputs to get stable behavior and net effect
    let mut stable_state = AbstractState::new(discovered_inputs);
    let pre_second_depth = stable_state.depth();
    execute_block_abstract(body, &mut stable_state);
    if stable_state.has_failed() {
        return LoopAnalysis {
            min_inputs_required: stable_state.inputs_discovered().max(discovered_inputs),
            net_effect_per_iteration: 0,
            total_inputs_for_loop: None,
            is_consistent: false,
            pattern_description: None,
            post_iteration_stack: Vec::new(),
            failure_reason: stable_state.failure_reason().map(String::from),
        };
    }
    let post_second_depth = stable_state.depth();

    // Calculate net effect from the stable iteration
    let net_effect = post_second_depth as i32 - pre_second_depth as i32;

    // Early check: stack-neutral loops with many iterations cause exponential
    // expression growth (e.g., `repeat.31 { dup; mul }` creates 2^31 nodes).
    // Fail fast with a clear diagnostic instead of hanging.
    if net_effect == 0 && iteration_count > MAX_NEUTRAL_LOOP_ITERATIONS {
        return LoopAnalysis {
            min_inputs_required: discovered_inputs,
            net_effect_per_iteration: 0,
            total_inputs_for_loop: Some(discovered_inputs),
            is_consistent: true,
            pattern_description: None,
            post_iteration_stack: stable_state.stack.clone(),
            failure_reason: Some(format!(
                "repeat.{} loop with stack-neutral body causes exponential complexity; \
                 decompilation skipped",
                iteration_count
            )),
        };
    }

    // Phase 3: Calculate total inputs needed for the full loop
    let total_inputs = if net_effect < 0 {
        // Consuming loop: needs more inputs for later iterations
        // Each iteration consumes |net_effect| elements
        // For N iterations, we need: initial_inputs + (N-1) * |net_effect|
        // But we also need to account for what the first iteration discovers
        let additional_per_iteration = (-net_effect) as usize;
        discovered_inputs + (iteration_count - 1) * additional_per_iteration
    } else {
        // Producing or neutral loop: discovered inputs are sufficient
        discovered_inputs
    };

    // Phase 4: Verify consistency by running two iterations from a fresh state
    // with enough inputs to avoid discovering new inputs during verification.
    // This gives us a clean comparison of net effects.
    let verify_inputs = if net_effect < 0 {
        // For consuming loops, we need enough inputs for 2 full iterations
        discovered_inputs + 2 * ((-net_effect) as usize)
    } else {
        // For neutral/producing loops, discovered inputs are sufficient
        discovered_inputs
    };

    let mut verify_state = AbstractState::new(verify_inputs);
    let pre_verify1_depth = verify_state.depth();
    execute_block_abstract(body, &mut verify_state);
    let post_verify1_depth = verify_state.depth();
    let verify1_effect = post_verify1_depth as i32 - pre_verify1_depth as i32;

    let pre_verify2_depth = verify_state.depth();
    execute_block_abstract(body, &mut verify_state);
    let post_verify2_depth = verify_state.depth();
    let verify2_effect = post_verify2_depth as i32 - pre_verify2_depth as i32;

    // Consistent if both verification iterations have the same effect as our calculated net_effect
    let is_consistent = verify1_effect == net_effect && verify2_effect == net_effect;

    // Generate pattern description
    let pattern_description = if is_consistent && net_effect != 0 {
        Some(format!(
            "Loop consumes {} element(s) per iteration",
            -net_effect
        ))
    } else if is_consistent && net_effect == 0 {
        Some("Loop preserves stack depth".to_string())
    } else {
        None
    };

    // Phase 5: Use loop context to infer parametric structure by comparing
    // consecutive iterations.
    let verify_inputs = if net_effect < 0 {
        discovered_inputs + 2 * ((-net_effect) as usize)
    } else {
        discovered_inputs
    };

    let mut verify_state = AbstractState::new(verify_inputs);
    let loop_depth = verify_state.enter_loop(net_effect).saturating_sub(1);

    if net_effect < 0 {
        verify_state.lift_for_current_loop(net_effect);
    }

    let entry_snapshot = verify_state.snapshot();
    execute_block_abstract(body, &mut verify_state);
    if verify_state.has_failed() {
        let min_inputs_required = verify_state.inputs_discovered().max(discovered_inputs);
        return LoopAnalysis {
            min_inputs_required,
            net_effect_per_iteration: net_effect,
            total_inputs_for_loop: Some(total_inputs),
            is_consistent: false,
            pattern_description,
            post_iteration_stack: Vec::new(),
            failure_reason: verify_state.failure_reason().map(String::from),
        };
    }
    let first_snapshot = verify_state.snapshot();

    execute_block_abstract(body, &mut verify_state);
    if verify_state.has_failed() {
        let min_inputs_required = verify_state.inputs_discovered().max(discovered_inputs);
        return LoopAnalysis {
            min_inputs_required,
            net_effect_per_iteration: net_effect,
            total_inputs_for_loop: Some(total_inputs),
            is_consistent: false,
            pattern_description,
            post_iteration_stack: Vec::new(),
            failure_reason: verify_state.failure_reason().map(String::from),
        };
    }
    let second_snapshot = verify_state.snapshot();

    verify_state.exit_loop();

    let min_inputs_required = verify_state.inputs_discovered().max(discovered_inputs);

    let first_effect = first_snapshot.depth() as i32 - entry_snapshot.depth() as i32;
    let second_effect = second_snapshot.depth() as i32 - first_snapshot.depth() as i32;
    let is_consistent = is_consistent && first_effect == net_effect && second_effect == net_effect;

    let failure_reason = if is_consistent {
        None
    } else {
        Some("repeat loop stack effect inconsistent".to_string())
    };

    let post_iteration_stack = if is_consistent {
        build_parametric_stack(
            &first_snapshot.stack,
            &second_snapshot.stack,
            loop_depth,
            net_effect,
        )
    } else {
        Vec::new()
    };

    LoopAnalysis {
        min_inputs_required,
        net_effect_per_iteration: net_effect,
        total_inputs_for_loop: Some(total_inputs),
        is_consistent,
        pattern_description,
        post_iteration_stack,
        failure_reason,
    }
}

/// Analyze a while loop body.
///
/// For while loops, we can't determine the exact iteration count,
/// so we analyze a single iteration to determine the stack effect pattern.
///
/// # While loop semantics in Miden
///
/// ```text
/// while.true
///     <body>        # Body must push a boolean condition
/// end
/// ```
///
/// - Entry: condition is on top of stack (consumed by while.true)
/// - Body executes if condition was true
/// - Body must push a new condition for the next iteration check
/// - Loop exits when condition is false (condition is consumed, not replaced)
///
/// # Net effect calculation
///
/// The "useful" net effect is the stack change excluding the condition mechanics:
/// - `body_effect = post_depth - pre_depth` (raw body effect)
/// - `net_effect = body_effect - 1` (subtract the condition the body must push)
///
/// If net_effect == 0, the loop is "stack-neutral" (preserves depth after exit)
/// If net_effect < 0, the loop consumes stack elements each iteration
/// If net_effect > 0, the loop produces stack elements each iteration
pub fn analyze_while_loop(body: &Block) -> LoopAnalysis {
    // Phase 1: Discover inputs by running one iteration from empty state
    let mut discovery_state = AbstractState::empty();
    execute_block_abstract(body, &mut discovery_state);

    if discovery_state.has_failed() {
        return LoopAnalysis {
            min_inputs_required: 0,
            net_effect_per_iteration: 0,
            total_inputs_for_loop: None,
            is_consistent: false,
            pattern_description: None,
            post_iteration_stack: Vec::new(),
            failure_reason: discovery_state.failure_reason().map(String::from),
        };
    }

    let discovered_inputs = discovery_state.inputs_discovered();

    // Phase 2: Compute net effect while accounting for the loop condition.
    let mut effect_state = AbstractState::new(discovered_inputs);
    effect_state.push(SymbolicExpr::Top); // Placeholder condition
    let _pre_depth = effect_state.depth();
    effect_state.pop(); // Consume condition
    let pre_body_depth = effect_state.depth();
    execute_block_abstract(body, &mut effect_state);
    if effect_state.has_failed() {
        return LoopAnalysis {
            min_inputs_required: discovered_inputs,
            net_effect_per_iteration: 0,
            total_inputs_for_loop: None,
            is_consistent: false,
            pattern_description: None,
            post_iteration_stack: Vec::new(),
            failure_reason: effect_state.failure_reason().map(String::from),
        };
    }
    let post_body_depth = effect_state.depth();

    // Body pushes a new condition; subtract it from the net effect.
    let body_effect = post_body_depth as i32 - pre_body_depth as i32;
    let net_effect = body_effect - 1;

    // Unbounded while loops with non-zero net effect are unsafe to analyze reliably.
    if net_effect != 0 {
        return LoopAnalysis {
            min_inputs_required: discovered_inputs,
            net_effect_per_iteration: net_effect,
            total_inputs_for_loop: None,
            is_consistent: false,
            pattern_description: Some(format!(
                "While body has unusual effect: {} (expected 1 for condition)",
                body_effect
            )),
            post_iteration_stack: Vec::new(),
            failure_reason: Some(
                "while loop with non-zero stack effect cannot be reliably analyzed".to_string(),
            ),
        };
    }

    // Phase 3: Run two iterations with loop context to infer parametric expressions.
    let mut iter_state = AbstractState::new(discovered_inputs);
    iter_state.push(SymbolicExpr::Top); // initial condition
    let loop_depth = iter_state.enter_loop(net_effect).saturating_sub(1);

    let mut first_snapshot = None;
    let mut second_snapshot = None;

    for iteration in 0..2 {
        iter_state.pop(); // consume condition
        execute_block_abstract(body, &mut iter_state);
        if iter_state.has_failed() {
            iter_state.exit_loop();
            return LoopAnalysis {
                min_inputs_required: discovered_inputs,
                net_effect_per_iteration: net_effect,
                total_inputs_for_loop: None,
                is_consistent: false,
                pattern_description: Some("While loop is stack-neutral".to_string()),
                post_iteration_stack: Vec::new(),
                failure_reason: iter_state.failure_reason().map(String::from),
            };
        }

        // Capture snapshot without the condition for param inference, but leave the
        // condition on the stack for the next iteration.
        let condition = iter_state.pop();
        let snapshot = iter_state.snapshot();
        iter_state.push(condition);

        if iteration == 0 {
            first_snapshot = Some(snapshot);
        } else {
            second_snapshot = Some(snapshot);
        }
    }

    iter_state.exit_loop();

    let first_snapshot = first_snapshot.unwrap_or_else(AbstractStateSnapshot::default);
    let second_snapshot = second_snapshot.unwrap_or_else(AbstractStateSnapshot::default);

    let min_inputs_required = iter_state.inputs_discovered().max(discovered_inputs);

    let is_consistent = first_snapshot.depth() == second_snapshot.depth();
    let failure_reason = if is_consistent {
        None
    } else {
        Some("while loop stack effect inconsistent".to_string())
    };

    let post_iteration_stack = if is_consistent {
        build_parametric_stack(
            &first_snapshot.stack,
            &second_snapshot.stack,
            loop_depth,
            net_effect,
        )
    } else {
        Vec::new()
    };

    LoopAnalysis {
        min_inputs_required,
        net_effect_per_iteration: net_effect,
        total_inputs_for_loop: None, // Unknown for while loops (depends on iteration count)
        is_consistent,
        pattern_description: Some("While loop is stack-neutral".to_string()),
        post_iteration_stack,
        failure_reason,
    }
}

/// Execute a block of operations on an abstract state.
fn execute_block_abstract(block: &Block, state: &mut AbstractState) {
    for op in block.iter() {
        if state.has_failed() {
            return;
        }
        execute_op_abstract(op, state);
    }
}

/// Execute a single operation on an abstract state.
fn execute_op_abstract(op: &Op, state: &mut AbstractState) {
    match op {
        Op::Inst(inst) => {
            transfer_instruction(inst.inner(), state, None, None);
        }
        Op::If {
            then_blk, else_blk, ..
        } => {
            // Pop condition
            state.pop();

            // For abstract interpretation, we need to join both branches
            let entry_snapshot = state.snapshot();

            // Execute then branch
            execute_block_abstract(then_blk, state);
            let then_snapshot = state.snapshot();

            // Execute else branch from entry state
            state.restore(&entry_snapshot);
            if !else_blk.is_empty() {
                execute_block_abstract(else_blk, state);
            }

            // Join the two exit states slot-wise to keep sound approximations.
            let else_snapshot = if else_blk.is_empty() {
                entry_snapshot.clone()
            } else {
                state.snapshot()
            };
            merge_if_snapshots(&then_snapshot, &else_snapshot, state);
        }
        Op::While { body, .. } => {
            let analysis = analyze_while_loop(body);

            if let Some(reason) = analysis.failure_reason.as_ref() {
                state.fail(reason);
                return;
            }

            if !analysis.is_consistent {
                state.fail("while loop stack effect inconsistent");
                return;
            }

            // Pop initial condition
            state.pop();

            state.enter_loop(analysis.net_effect_per_iteration);
            if analysis.net_effect_per_iteration < 0 {
                state.lift_for_current_loop(analysis.net_effect_per_iteration);
            }

            // Execute once to update state
            execute_block_abstract(body, state);
            if !state.has_failed() {
                // Pop the condition that would be pushed at end
                state.pop();
            }

            // Exit loop context
            state.exit_loop();
        }
        Op::Repeat { count, body, .. } => {
            // For nested repeat loops, check for exponential complexity first
            let analysis = analyze_repeat_loop(body, *count as usize);

            if let Some(reason) = analysis.failure_reason.as_ref() {
                state.fail(reason);
                return;
            }

            if !analysis.is_consistent {
                state.fail("repeat loop stack effect inconsistent");
                return;
            }

            state.enter_loop(analysis.net_effect_per_iteration);
            if analysis.net_effect_per_iteration < 0 {
                state.lift_for_current_loop(analysis.net_effect_per_iteration);
            }

            // Execute all iterations symbolically
            for _ in 0..*count {
                if state.has_failed() {
                    break;
                }
                execute_block_abstract(body, state);
            }

            state.exit_loop();
        }
    }
}

/// Join the exit states of an if-then-else construct.
///
/// Fails tracking if branch stack depths differ, otherwise joins each slot
/// using the SymbolicExpr lattice join.
fn merge_if_snapshots(
    then_snapshot: &AbstractStateSnapshot,
    else_snapshot: &AbstractStateSnapshot,
    state: &mut AbstractState,
) {
    if then_snapshot.depth() != else_snapshot.depth() {
        state.fail("if branches have different stack effects");
        return;
    }

    let len = then_snapshot.depth();
    let mut joined_stack = Vec::with_capacity(len);
    for i in 0..len {
        joined_stack.push(then_snapshot.stack[i].join(&else_snapshot.stack[i]));
    }

    state.stack = joined_stack;
    state.discovered_inputs = state
        .discovered_inputs
        .max(then_snapshot.discovered_inputs)
        .max(else_snapshot.discovered_inputs);
}

// ═══════════════════════════════════════════════════════════════════════════════
// Pre-analysis for Procedure Input Discovery
// ═══════════════════════════════════════════════════════════════════════════════

/// Pre-analyze a procedure body to discover the total inputs needed.
///
/// This walks through the procedure, analyzing all loops to determine
/// the maximum inputs that will be accessed.
pub fn pre_analyze_procedure(body: &Block) -> ProcedureAnalysis {
    let mut state = AbstractState::empty();
    pre_analyze_block(body, &mut state);

    ProcedureAnalysis {
        total_inputs_required: state.inputs_discovered(),
        has_dynamic_stack: state.has_failed(),
        failure_reason: state.failure_reason().map(String::from),
    }
}

/// Result of pre-analyzing a procedure.
#[derive(Clone, Debug)]
pub struct ProcedureAnalysis {
    /// Total number of inputs the procedure will access.
    pub total_inputs_required: usize,

    /// Whether the procedure has dynamic stack effects (e.g., while loops).
    pub has_dynamic_stack: bool,

    /// Failure reason if pre-analysis failed.
    pub failure_reason: Option<String>,
}

/// Pre-analyze a block to discover inputs.
fn pre_analyze_block(block: &Block, state: &mut AbstractState) {
    for op in block.iter() {
        if state.has_failed() {
            return;
        }
        pre_analyze_op(op, state);
    }
}

/// Pre-analyze a single operation.
fn pre_analyze_op(op: &Op, state: &mut AbstractState) {
    match op {
        Op::Inst(inst) => {
            transfer_instruction(inst.inner(), state, None, None);
        }
        Op::If {
            then_blk, else_blk, ..
        } => {
            state.pop(); // condition
            let snapshot = state.snapshot();

            pre_analyze_block(then_blk, state);
            let then_inputs = state.inputs_discovered();

            state.restore(&snapshot);
            if !else_blk.is_empty() {
                pre_analyze_block(else_blk, state);
            }

            // Take maximum of both branches
            if then_inputs > state.inputs_discovered() {
                // Force discovery of additional inputs
                state.ensure_depth(state.depth() + (then_inputs - state.inputs_discovered()));
            }
        }
        Op::While { body, .. } => {
            state.pop(); // condition

            // Analyze the while loop body
            let analysis = analyze_while_loop(body);

            if let Some(reason) = analysis.failure_reason.as_ref() {
                state.fail(reason);
                return;
            }

            // Execute once to update state
            pre_analyze_block(body, state);
            state.pop(); // condition at end

            // If while loop is inconsistent, mark as dynamic
            if !analysis.is_consistent {
                state.fail("while loop with variable stack effect");
            }
        }
        Op::Repeat { count, body, .. } => {
            // Analyze the repeat loop
            let analysis = analyze_repeat_loop(body, *count as usize);

            // Ensure we have enough inputs for the entire loop
            if let Some(total) = analysis.total_inputs_for_loop {
                state.ensure_depth(total);
            }

            if let Some(reason) = analysis.failure_reason.as_ref() {
                state.fail(reason);
                return;
            }

            if !analysis.is_consistent {
                state.fail("repeat loop stack effect inconsistent");
                return;
            }

            // Execute the loop to update state properly
            for _ in 0..*count {
                if state.has_failed() {
                    return;
                }
                pre_analyze_block(body, state);
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
    fn test_symbolic_expr_display() {
        assert_eq!(SymbolicExpr::Input(0).to_string(), "a_0");
        assert_eq!(SymbolicExpr::Input(5).to_string(), "a_5");
        assert_eq!(SymbolicExpr::Constant(42).to_string(), "42");

        let add = SymbolicExpr::binary(
            BinaryOpKind::Add,
            SymbolicExpr::Input(0),
            SymbolicExpr::Input(5),
        );
        assert_eq!(add.to_string(), "a_0 + a_5");
    }

    #[test]
    fn test_parametric_expr_display() {
        // Simplified: a_(i) → a_i for base=0, stride=1
        let expr = SymbolicExpr::parametric(0, 1, 0);
        assert_eq!(expr.to_pseudocode("i"), "a_i");

        // a_(5+i) - stride=1 omits coefficient
        let expr = SymbolicExpr::parametric(5, 1, 0);
        assert_eq!(expr.to_pseudocode("i"), "a_(5+i)");

        // a_(2*i) - coefficient comes first
        let expr = SymbolicExpr::parametric(0, 2, 0);
        assert_eq!(expr.to_pseudocode("i"), "a_(2*i)");

        // a_(3+2*i) - coefficient comes first
        let expr = SymbolicExpr::parametric(3, 2, 0);
        assert_eq!(expr.to_pseudocode("i"), "a_(3+2*i)");
    }

    #[test]
    fn test_abstract_state_basic_ops() {
        let mut state = AbstractState::new(3);
        assert_eq!(state.depth(), 3);

        // Stack should be [a_2, a_1, a_0] with a_0 on top
        assert_eq!(state.peek(0).unwrap().as_input(), Some(0));
        assert_eq!(state.peek(1).unwrap().as_input(), Some(1));
        assert_eq!(state.peek(2).unwrap().as_input(), Some(2));

        // Pop should return a_0
        let popped = state.pop();
        assert_eq!(popped.as_input(), Some(0));
        assert_eq!(state.depth(), 2);

        // Pop from empty should discover new input
        state.pop();
        state.pop();
        let discovered = state.pop();
        assert_eq!(discovered.as_input(), Some(3));
        assert_eq!(state.inputs_discovered(), 4);
    }

    #[test]
    fn test_abstract_state_movup() {
        let mut state = AbstractState::new(6);
        // Stack: [a_5, a_4, a_3, a_2, a_1, a_0] (a_0 on top)

        state.movup(5);
        // After movup.5: a_5 moves to top
        // Stack: [a_4, a_3, a_2, a_1, a_0, a_5]

        assert_eq!(state.peek(0).unwrap().as_input(), Some(5));
        assert_eq!(state.peek(1).unwrap().as_input(), Some(0));
    }

    #[test]
    fn test_abstract_state_movdn() {
        let mut state = AbstractState::new(5);
        // Stack: [a_4, a_3, a_2, a_1, a_0] (a_0 on top)

        state.movdn(4);
        // After movdn.4: a_0 moves to position 4
        // Stack: [a_0, a_4, a_3, a_2, a_1]

        assert_eq!(state.peek(0).unwrap().as_input(), Some(1));
        assert_eq!(state.peek(4).unwrap().as_input(), Some(0));
    }

    #[test]
    fn test_lift_to_parametric() {
        let expr = SymbolicExpr::Input(3);
        let lifted = expr.lift_to_parametric(-1, 0);

        // a_3 with net_effect=-1 should become a_(3+i)
        // Use as_parametric() to extract single-term components
        let (base, stride, loop_depth) = lifted
            .as_parametric()
            .expect("Expected single-term ParametricInput");
        assert_eq!(base, 3);
        assert_eq!(stride, 1); // -(-1) = 1
        assert_eq!(loop_depth, 0);
    }

    #[test]
    fn test_join_inputs() {
        let a = SymbolicExpr::Input(0);
        let b = SymbolicExpr::Input(1);

        // True lattice join of different inputs should be Top (sound approximation)
        let joined = a.join(&b);
        assert!(
            joined.is_top(),
            "Join of different inputs should be Top, got {:?}",
            joined
        );

        // Same inputs should join to themselves
        let same_join = a.join(&a);
        assert_eq!(same_join.as_input(), Some(0));
    }

    #[test]
    fn test_infer_pattern() {
        let a = SymbolicExpr::Input(0);
        let b = SymbolicExpr::Input(1);

        // Pattern inference can create a parametric pattern
        let pattern = a.infer_pattern(&b, 0);
        let (base, stride, _loop_depth) = pattern
            .as_parametric()
            .expect("Expected single-term ParametricInput");
        assert_eq!(base, 0);
        assert_eq!(stride, 1);
    }

    // ─────────────────────────────────────────────────────────────────────
    // LoopTerm and multi-term parametric tests
    // ─────────────────────────────────────────────────────────────────────

    #[test]
    fn test_loop_term_new() {
        let term = LoopTerm::new(-4, 0);
        assert_eq!(term.stride, -4);
        assert_eq!(term.loop_depth, 0);
    }

    #[test]
    fn test_parametric_single_term() {
        let expr = SymbolicExpr::parametric(3, -1, 0);
        assert_eq!(expr.as_parametric(), Some((3, -1, 0)));
    }

    #[test]
    fn test_parametric_multi_term() {
        let expr =
            SymbolicExpr::parametric_multi(5, vec![LoopTerm::new(-4, 0), LoopTerm::new(-1, 1)]);

        // Single-term accessor returns None for multi-term
        assert_eq!(expr.as_parametric(), None);

        // Multi-term accessor works
        let (base, terms) = expr.as_parametric_multi().unwrap();
        assert_eq!(base, 5);
        assert_eq!(terms.len(), 2);
        assert_eq!(terms[0], LoopTerm::new(-4, 0));
        assert_eq!(terms[1], LoopTerm::new(-1, 1));
    }

    #[test]
    fn test_add_loop_term_from_input() {
        let input = SymbolicExpr::Input(3);
        let parametric = input.add_loop_term(-1, 0);

        assert_eq!(parametric.as_parametric(), Some((3, -1, 0)));
    }

    #[test]
    fn test_add_loop_term_nested() {
        let input = SymbolicExpr::Input(5);
        let outer = input.add_loop_term(-4, 0);
        let nested = outer.add_loop_term(-1, 1);

        let (base, terms) = nested.as_parametric_multi().unwrap();
        assert_eq!(base, 5);
        assert_eq!(terms.len(), 2);
    }

    #[test]
    fn test_add_loop_term_binary_op() {
        let left = SymbolicExpr::Input(0);
        let right = SymbolicExpr::Input(1);
        let expr = SymbolicExpr::BinaryOp {
            op: BinaryOpKind::Add,
            left: Box::new(left),
            right: Box::new(right),
        };

        let lifted = expr.add_loop_term(-1, 0);

        match lifted {
            SymbolicExpr::BinaryOp { left, right, .. } => {
                assert!(left.as_parametric().is_some());
                assert!(right.as_parametric().is_some());
            }
            _ => panic!("Expected BinaryOp"),
        }
    }

    #[test]
    fn test_to_pseudocode_multi_single_term() {
        let expr = SymbolicExpr::parametric(3, -1, 0);
        assert_eq!(expr.to_pseudocode_multi(&["i"]), "a_(3-i)");
    }

    #[test]
    fn test_to_pseudocode_multi_nested() {
        let expr =
            SymbolicExpr::parametric_multi(5, vec![LoopTerm::new(-4, 0), LoopTerm::new(-1, 1)]);
        assert_eq!(expr.to_pseudocode_multi(&["i", "j"]), "a_(5-4*i-j)");
    }

    #[test]
    fn test_to_pseudocode_multi_positive_stride() {
        // Simplified: a_(i) → a_i for base=0, stride=1
        let expr = SymbolicExpr::parametric(0, 1, 0);
        assert_eq!(expr.to_pseudocode_multi(&["i"]), "a_i");

        // For non-zero base with positive stride=1, we get a_(3+i)
        let expr_with_base = SymbolicExpr::parametric(3, 1, 0);
        assert_eq!(expr_with_base.to_pseudocode_multi(&["i"]), "a_(i+3)");

        // For stride > 1, coefficient comes first: a_(3*i)
        let expr_stride_3 = SymbolicExpr::parametric(0, 3, 0);
        assert_eq!(expr_stride_3.to_pseudocode_multi(&["i"]), "a_(3*i)");

        // For base and stride > 1: a_(5+3*i)
        let expr_both = SymbolicExpr::parametric(5, 3, 0);
        assert_eq!(expr_both.to_pseudocode_multi(&["i"]), "a_(3*i+5)");
    }

    // ─────────────────────────────────────────────────────────────────────
    // AbstractState loop context tests
    // ─────────────────────────────────────────────────────────────────────

    #[test]
    fn test_abstract_state_enter_exit_loop() {
        let mut state = AbstractState::new(4);

        assert_eq!(state.current_loop_depth(), 0);
        assert!(!state.in_loop());

        let depth1 = state.enter_loop(-1);
        assert_eq!(depth1, 1);
        assert_eq!(state.current_loop_depth(), 1);
        assert!(state.in_loop());

        let depth2 = state.enter_loop(-1);
        assert_eq!(depth2, 2);
        assert_eq!(state.current_loop_depth(), 2);

        let info = state.exit_loop().unwrap();
        assert_eq!(info.net_effect, -1);
        assert_eq!(state.current_loop_depth(), 1);

        state.exit_loop();
        assert_eq!(state.current_loop_depth(), 0);
        assert!(!state.in_loop());
    }

    #[test]
    fn test_abstract_state_lift_for_current_loop() {
        let mut state = AbstractState::new(4);
        state.enter_loop(-1);
        state.lift_for_current_loop(-1);

        // All inputs should now be parametric
        for i in 0..4 {
            let expr = state.peek(i).unwrap();
            assert!(
                expr.as_parametric().is_some(),
                "Expected parametric at position {}",
                i
            );
        }
    }

    #[test]
    fn test_if_merge_slotwise_join() {
        let then_snapshot = AbstractStateSnapshot {
            stack: vec![SymbolicExpr::Input(0), SymbolicExpr::Input(1)],
            discovered_inputs: 2,
        };
        let else_snapshot = AbstractStateSnapshot {
            stack: vec![SymbolicExpr::Input(0), SymbolicExpr::Input(2)],
            discovered_inputs: 3,
        };

        let mut state = AbstractState::empty();
        merge_if_snapshots(&then_snapshot, &else_snapshot, &mut state);

        assert!(!state.has_failed());
        assert_eq!(state.depth(), 2);
        // Top because inputs differ at the same slot
        assert!(matches!(state.peek(0), Some(SymbolicExpr::Top)));
        // Shared input preserved
        assert_eq!(state.peek(1).and_then(SymbolicExpr::as_input), Some(0));
        // discovered_inputs should reflect the max across branches
        assert_eq!(state.inputs_discovered(), 3);
    }

    #[test]
    fn test_if_merge_depth_mismatch_fails() {
        let then_snapshot = AbstractStateSnapshot {
            stack: vec![SymbolicExpr::Input(0)],
            discovered_inputs: 1,
        };
        let else_snapshot = AbstractStateSnapshot {
            stack: vec![SymbolicExpr::Input(0), SymbolicExpr::Input(1)],
            discovered_inputs: 2,
        };

        let mut state = AbstractState::empty();
        merge_if_snapshots(&then_snapshot, &else_snapshot, &mut state);

        assert!(state.has_failed());
        assert_eq!(
            state.failure_reason(),
            Some("if branches have different stack effects")
        );
    }

    #[test]
    fn test_apply_transfer_event_binary() {
        use miden_assembly_syntax::ast::Instruction;

        let mut state = AbstractState::new(2);
        let event = apply_transfer_event(&Instruction::Add, &mut state, None, None);
        assert!(matches!(
            event,
            TransferEvent::BinaryOp {
                op: BinaryOpKind::Add,
                ..
            }
        ));
        // Two inputs replaced by one result
        assert_eq!(state.depth(), 1);
    }

    #[test]
    fn test_apply_transfer_event_dup() {
        use miden_assembly_syntax::ast::Instruction;

        let mut state = AbstractState::new(1);
        let event = apply_transfer_event(&Instruction::Dup0, &mut state, None, None);
        assert!(matches!(event, TransferEvent::Dup { source_pos: 0, .. }));
        assert_eq!(state.depth(), 2);
    }
}
