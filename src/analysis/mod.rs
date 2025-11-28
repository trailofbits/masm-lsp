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
//! - `stack_ops`: Shared stack operations and fast-path analysis
//! - `analyzer`: Main analyzer composing checkers with stack simulation
//! - `contracts`: Workspace-wide procedure contract inference
//! - `while_loops`: Loop bound inference for while loops with counter patterns
//! - `abstract_interp`: Abstract interpretation framework for decompilation

pub mod abstract_interp;
pub mod analyzer;
pub mod checker;
pub mod checkers;
pub mod contracts;
pub mod stack_effects;
pub mod stack_ops;
pub mod types;
pub mod utils;
pub mod while_loops;

// Re-export the main entry point
pub use analyzer::analyze_module;

// Re-export contract types for workspace integration
pub use contracts::{
    infer_module_contracts, parse_procedure_signature, ContractStore, ParsedSignature,
    ProcContract, StackEffect, ValidationBehavior,
};

// Re-export core types for external use
pub use types::{AnalysisState, Bounds, Source, Taint};

// Re-export abstract interpretation types for decompilation
pub use abstract_interp::{
    analyze_repeat_loop, analyze_while_loop, pre_analyze_procedure, AbstractState, LoopAnalysis,
    ProcedureAnalysis, SymbolicExpr,
};

// Re-export stack operations for shared use
pub use stack_ops::{
    analyze_procedure_simple, analyze_procedure_tiered, static_effect, SimpleStackAnalysis,
    StackEffectResult, StackLike, StaticEffect,
};
