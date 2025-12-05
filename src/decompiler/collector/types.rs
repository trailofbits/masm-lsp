use std::collections::HashMap;

use miden_debug_types::SourceSpan;

use crate::analysis::{abstract_interpretation::LoopTerm, SymbolicExpr};
use crate::decompiler::ssa::{PseudocodeTemplate, SsaContext, SsaId};

/// Related information for a tracking failure diagnostic.
#[derive(Clone)]
pub struct RelatedInfo {
    /// The span of the related location
    pub span: SourceSpan,
    /// Message explaining the related location
    pub message: String,
}

/// A tracking failure that should be reported as a diagnostic.
pub(crate) struct TrackingFailure {
    /// The span where decompilation failed
    pub span: SourceSpan,
    /// The reason for the failure
    pub reason: String,
    /// The name of the procedure where failure occurred
    pub proc_name: String,
    /// Optional related information (e.g., the root cause in another procedure)
    pub related: Option<RelatedInfo>,
}

/// A term in a parametric expression representing `stride * counter`.
///
/// Used at hint resolution time to compute parametric variable names
/// based on the loop context at the time the hint was created.
#[derive(Debug, Clone)]
pub(crate) struct ParametricTerm {
    /// Coefficient of the loop counter (negative for decrementing access)
    pub stride: i32,
    /// Index into the counter names list (0 = outermost loop)
    pub counter_idx: usize,
}

impl ParametricTerm {
    pub fn new(stride: i32, counter_idx: usize) -> Self {
        Self {
            stride,
            counter_idx,
        }
    }
}

/// A parametric expression of the form `base + stride_0 * counter_0 + stride_1 * counter_1 + ...`
///
/// Computed at hint creation time based on the active loops and their stack effects.
/// Used at resolution time to generate display names like `a_(3-i)`.
#[derive(Debug, Clone)]
pub(crate) struct ParametricExpr {
    /// Variable prefix ("a" for arguments, "v" for locals)
    pub prefix: char,
    /// Base index (constant part)
    pub base: i32,
    /// Loop terms - each represents dependency on one loop counter
    pub terms: Vec<ParametricTerm>,
}

impl ParametricExpr {
    /// Create a new parametric expression for an argument.
    pub fn argument(base: i32, terms: Vec<ParametricTerm>) -> Self {
        Self {
            prefix: 'a',
            base,
            terms,
        }
    }

    /// Create a parametric expression for a loop-produced output (r-indexed).
    pub fn output(base: i32, counter_idx: usize) -> Self {
        Self {
            prefix: 'r',
            base,
            terms: vec![ParametricTerm::new(1, counter_idx)],
        }
    }

    /// Create a parametric expression for a loop-produced local (v-indexed).
    pub fn local(base: i32, counter_idx: usize) -> Self {
        Self {
            prefix: 'v',
            base,
            terms: vec![ParametricTerm::new(1, counter_idx)],
        }
    }

    /// Format the parametric expression using the given counter names.
    pub fn format(&self, counter_names: &[String]) -> String {
        let loop_terms: Vec<LoopTerm> = self
            .terms
            .iter()
            .map(|t| LoopTerm::new(t.stride, t.counter_idx))
            .collect();
        let symbolic = if loop_terms.is_empty() {
            SymbolicExpr::Input(self.base as usize)
        } else {
            SymbolicExpr::ParametricInput {
                base: self.base,
                loop_terms,
            }
        };

        let counters: Vec<&str> = counter_names.iter().map(|s| s.as_str()).collect();
        let formatted = symbolic.to_pseudocode_multi(&counters);
        formatted.replacen("a_", &format!("{}_", self.prefix), 1)
    }

    pub fn from_symbolic(expr: &SymbolicExpr) -> Option<Self> {
        match expr {
            SymbolicExpr::ParametricInput { base, loop_terms } => {
                let terms = loop_terms
                    .iter()
                    .map(|t| ParametricTerm::new(t.stride, t.loop_depth))
                    .collect();
                Some(ParametricExpr::argument(*base, terms))
            }
            SymbolicExpr::Input(i) => Some(ParametricExpr::argument(*i as i32, Vec::new())),
            _ => None,
        }
    }
}

/// Information about an active loop during decompilation.
///
/// Stored in the collector's loop_stack to track loop context for parametric naming.
#[derive(Debug, Clone)]
pub(crate) struct ActiveLoop {
    /// Counter variable name (e.g., "i", "j", "k")
    pub counter_name: String,
    /// Symbolic stack snapshot after one iteration (with loop term offsets applied)
    pub analyzed_stack: Vec<SymbolicExpr>,
    /// Net stack effect per iteration for this loop
    pub net_effect: i32,
    /// Next local index when the loop was entered (to identify loop-created locals)
    pub local_base: usize,
}

impl ActiveLoop {
    pub fn new(
        counter_name: String,
        analyzed_stack: Vec<SymbolicExpr>,
        net_effect: i32,
        local_base: usize,
    ) -> Self {
        Self {
            counter_name,
            analyzed_stack,
            net_effect,
            local_base,
        }
    }

    pub fn expr_for_argument(&self, arg_idx: usize) -> Option<SymbolicExpr> {
        self.analyzed_stack
            .iter()
            .rev()
            .find_map(|expr| find_param_expr(expr, arg_idx))
    }
}

pub(crate) fn find_param_expr(expr: &SymbolicExpr, arg_idx: usize) -> Option<SymbolicExpr> {
    match expr {
        SymbolicExpr::Input(i) if *i == arg_idx => Some(SymbolicExpr::Input(*i)),
        SymbolicExpr::ParametricInput { base, .. } if *base == arg_idx as i32 => Some(expr.clone()),
        SymbolicExpr::BinaryOp { left, right, .. } => {
            find_param_expr(left, arg_idx).or_else(|| find_param_expr(right, arg_idx))
        }
        SymbolicExpr::UnaryOp { operand, .. } => find_param_expr(operand, arg_idx),
        SymbolicExpr::MemoryLoad { address } => find_param_expr(address, arg_idx),
        _ => None,
    }
}

pub(crate) fn shift_loop_depth(expr: &SymbolicExpr, offset: usize) -> SymbolicExpr {
    match expr {
        SymbolicExpr::ParametricInput { base, loop_terms } => {
            let shifted_terms = loop_terms
                .iter()
                .map(|t| LoopTerm::new(t.stride, t.loop_depth + offset))
                .collect();
            SymbolicExpr::ParametricInput {
                base: *base,
                loop_terms: shifted_terms,
            }
        }
        SymbolicExpr::BinaryOp { op, left, right } => SymbolicExpr::BinaryOp {
            op: *op,
            left: Box::new(shift_loop_depth(left, offset)),
            right: Box::new(shift_loop_depth(right, offset)),
        },
        SymbolicExpr::UnaryOp { op, operand } => SymbolicExpr::UnaryOp {
            op: *op,
            operand: Box::new(shift_loop_depth(operand, offset)),
        },
        SymbolicExpr::MemoryLoad { address } => SymbolicExpr::MemoryLoad {
            address: Box::new(shift_loop_depth(address, offset)),
        },
        _ => expr.clone(),
    }
}

pub(crate) fn shift_loop_stack(stack: &[SymbolicExpr], offset: usize) -> Vec<SymbolicExpr> {
    stack
        .iter()
        .map(|expr| shift_loop_depth(expr, offset))
        .collect()
}

/// A hint template that will be resolved to a final string after phi resolution.
#[derive(Clone)]
pub(crate) struct SsaHint {
    /// Line number for the hint
    pub line: u32,
    /// Template to resolve
    pub template: PseudocodeTemplate,
    /// Indentation level when this hint was created
    pub indent_level: usize,
    /// Map from SSA IDs to parametric expressions (computed at hint creation time)
    pub parametric_map: HashMap<SsaId, ParametricExpr>,
    /// Counter names at the time of hint creation (for formatting parametric expressions)
    pub counter_names: Vec<String>,
}

impl SsaHint {
    pub fn new(line: u32, template: PseudocodeTemplate, indent_level: usize) -> Self {
        Self {
            line,
            template,
            indent_level,
            parametric_map: HashMap::new(),
            counter_names: Vec::new(),
        }
    }

    pub fn with_parametric(
        line: u32,
        template: PseudocodeTemplate,
        indent_level: usize,
        parametric_map: HashMap<SsaId, ParametricExpr>,
        counter_names: Vec<String>,
    ) -> Self {
        Self {
            line,
            template,
            indent_level,
            parametric_map,
            counter_names,
        }
    }

    pub fn resolve(&self, ctx: &SsaContext) -> String {
        let indent = "    ".repeat(self.indent_level);
        let content = self.template.resolve_with_overrides(ctx, |id| {
            self.parametric_map
                .get(&id)
                .map(|expr| expr.format(&self.counter_names))
        });
        format!("{}{}", indent, content)
    }
}
