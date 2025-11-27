//! Security checkers for Miden assembly analysis.
//!
//! This module contains individual checkers that detect specific security issues:
//! - `u32_validation`: Unvalidated advice in u32 operations
//! - `merkle_depth`: Unvalidated Merkle tree depth
//! - `procedure_call`: Untrusted values passed to procedures

mod merkle_depth;
mod procedure_call;
mod u32_validation;

pub use merkle_depth::MerkleDepthChecker;
pub use procedure_call::ProcedureCallChecker;
pub use u32_validation::U32ValidationChecker;

use super::checker::Checker;

/// Create the default set of checkers.
pub fn default_checkers() -> Vec<Box<dyn Checker>> {
    vec![
        Box::new(U32ValidationChecker),
        Box::new(MerkleDepthChecker),
        Box::new(ProcedureCallChecker),
    ]
}
