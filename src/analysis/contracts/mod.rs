//! Procedure contract inference.
//!
//! This module provides automatic inference of procedure contracts from their
//! implementations, enabling workspace-wide inter-procedural taint analysis.
//!
//! # Module Structure
//!
//! - `types`: Core contract types (`ProcContract`, `StackEffect`, `ValidationBehavior`, `InputKind`, `InputSignature`)
//! - `signature`: Procedure signature parsing for explicit type annotations
//! - `store`: Workspace-wide contract storage (`ContractStore`)
//! - `inference`: Contract inference from procedure implementations

pub mod inference;
pub mod signature;
pub mod store;
pub mod types;

// Re-export main types for convenient access
pub use inference::{infer_module_contracts, infer_module_contracts_with_store};
pub use signature::{extract_and_parse_signature, parse_procedure_signature, ParsedSignature};
pub use store::ContractStore;
pub use types::{
    InputKind, InputSignature, OutputKind, ProcContract, ProcSignature, StackEffect,
    ValidationBehavior,
};
