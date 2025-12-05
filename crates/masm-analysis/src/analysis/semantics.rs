//! Instruction semantics facade re-exported from `masm-instructions`.
//!
//! Keeping this module allows downstream code to keep using
//! `crate::analysis::semantics::semantics_of` while the authoritative logic
//! lives in the `masm-instructions` crate.

pub use masm_instructions::{semantics_of, InstructionEffect};
