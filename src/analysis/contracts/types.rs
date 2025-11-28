//! Contract types for procedure analysis.
//!
//! This module contains the core types for describing procedure behavior:
//! - `ValidationBehavior`: Whether a procedure validates its inputs
//! - `StackEffect`: How a procedure affects the stack
//! - `ProcContract`: Complete contract describing procedure behavior

use tower_lsp::lsp_types::Range;

use crate::symbol_path::SymbolPath;

// ═══════════════════════════════════════════════════════════════════════════
// Validation Behavior
// ═══════════════════════════════════════════════════════════════════════════

/// Describes whether a procedure validates its inputs.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub enum ValidationBehavior {
    /// Procedure does not validate inputs
    #[default]
    None,
    /// Procedure validates inputs (has u32assert* at start)
    ValidatesU32,
}

// ═══════════════════════════════════════════════════════════════════════════
// Stack Effect
// ═══════════════════════════════════════════════════════════════════════════

/// Stack effect of a procedure - how it changes the stack depth.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub enum StackEffect {
    /// Stack effect is completely unknown
    #[default]
    Unknown,
    /// Inputs are known but outputs are unknown (e.g., due to while loops with unknown iteration count)
    KnownInputs { inputs: usize },
    /// Known stack effect: (inputs_consumed, outputs_produced)
    /// Net change = outputs_produced - inputs_consumed
    Known { inputs: usize, outputs: usize },
}

impl StackEffect {
    /// Get the number of inputs if known.
    pub fn inputs(&self) -> Option<usize> {
        match self {
            StackEffect::Unknown => None,
            StackEffect::KnownInputs { inputs } => Some(*inputs),
            StackEffect::Known { inputs, .. } => Some(*inputs),
        }
    }

    /// Get the number of outputs if known.
    pub fn outputs(&self) -> Option<usize> {
        match self {
            StackEffect::Unknown | StackEffect::KnownInputs { .. } => None,
            StackEffect::Known { outputs, .. } => Some(*outputs),
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Procedure Contract
// ═══════════════════════════════════════════════════════════════════════════

/// Contract describing expected behavior of a procedure.
#[derive(Clone, Debug)]
pub struct ProcContract {
    /// Full path of the procedure
    pub path: SymbolPath,
    /// Whether the procedure validates its inputs
    pub validates: ValidationBehavior,
    /// Whether the procedure uses u32 operations (implies u32 input requirements)
    pub uses_u32_ops: bool,
    /// Whether the procedure reads from advice stack
    pub reads_advice: bool,
    /// Whether the procedure performs Merkle operations
    pub uses_merkle_ops: bool,
    /// Stack effect of this procedure
    pub stack_effect: StackEffect,
    /// Location of the procedure definition
    pub definition_range: Option<Range>,
}

impl ProcContract {
    /// Returns true if this procedure validates its inputs.
    pub fn validates_inputs(&self) -> bool {
        matches!(self.validates, ValidationBehavior::ValidatesU32)
    }

    /// Returns true if this procedure requires u32 inputs based on its operations.
    pub fn requires_u32_inputs(&self) -> bool {
        self.uses_u32_ops && !self.validates_inputs()
    }
}
