//! Static analysis for Miden assembly.
//!
//! This module provides security analysis to detect potential issues:
//! - Unvalidated advice values used in u32 operations
//! - Missing range checks before Merkle operations
//! - Untrusted values passed to procedures requiring u32 inputs
//!
//! The analysis is structured as:
//! - `types`: Core types (Bounds, Source, Taint, AnalysisState)
//! - `checker`: Pluggable checker trait and common utilities
//! - `checkers`: Individual security checkers (u32, merkle, procedure calls)
//! - `stack_effects`: How instructions affect the symbolic stack
//! - `analyzer`: Main analyzer composing checkers with stack simulation
//! - `contracts`: Workspace-wide procedure contract inference

pub mod analyzer;
pub mod checker;
pub mod checkers;
pub mod contracts;
pub mod stack_effects;
pub mod types;

// Re-export the main entry point
pub use analyzer::analyze_module;

// Re-export contract types for workspace integration
pub use contracts::{
    infer_module_contracts, ContractStore, ProcContract, StackEffect, ValidationBehavior,
};

// Re-export core types for external use
pub use types::{AnalysisState, Bounds, Source, Taint};
