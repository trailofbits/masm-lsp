//! SSA (Static Single Assignment) representation for decompilation.
//!
//! This module tracks variable identity across control flow, enabling clean
//! pseudocode generation where updates show as `a_1 = a_1 + 1` rather than
//! anonymous temporaries.

pub mod context;
pub mod helpers;
pub mod ids;
pub mod pseudocode;
pub mod signature;
pub mod stack;
pub mod state;

#[cfg(test)]
mod tests;

pub use context::{PhiNode, SsaContext};
pub use helpers::{
    binary_imm_op, binary_op, comparison, comparison_imm, dup, ext2_binary_op, ext2_unary_fn,
    ext2_unary_op, unary_fn, unary_op,
};
pub use ids::{SsaId, SsaValue, VarKind};
pub use pseudocode::{PseudocodeBuilder, PseudocodeSegment, PseudocodeTemplate};
pub use signature::{extract_declaration_prefix, format_procedure_signature};
pub use stack::SsaStack;
pub use state::{DecompilerState, LoopPhiError, LOOP_COUNTER_NAMES};
