//! Decompiler for generating pseudocode from Miden assembly.
//!
//! This module converts Miden assembly instructions into readable pseudocode
//! by tracking a symbolic stack with named variables.
//!
//! # Module Structure
//!
//! - `collector`: AST visitor that collects hints for all procedures
//! - `ssa`: SSA representation for tracking variable identity across control flow

mod collector;
pub mod ssa;

// Re-export public API
pub use collector::{collect_decompilation_hints, DecompilationResult};

// Re-export SSA types and generic helpers
pub use ssa::{
    // Generic helper functions
    binary_imm_op,
    binary_op,
    comparison,
    comparison_imm,
    dup,
    ext2_binary_op,
    ext2_unary_fn,
    ext2_unary_op,
    unary_fn,
    unary_op,
    DecompilerState,
    PseudocodeBuilder,
    PseudocodeSegment,
    PseudocodeTemplate,
    // SSA types
    SsaContext,
    SsaId,
    SsaStack,
    SsaValue,
    // Output types
    VarKind,
};
