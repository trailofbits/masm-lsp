//! Core types for static analysis.
//!
//! This module contains the fundamental data structures used for tracking
//! value provenance, bounds, and validation state during analysis.
//!
//! # Module Structure
//!
//! - `bounds`: Value bounds tracking (`Bounds`, `U32_MAX`, `FIELD_MODULUS`)
//! - `taint`: Value tracking types (`Source`, `ValidationState`, `TrackedValue`)
//! - `stack`: Symbolic stack implementation (`SymbolicStack`)
//! - `state`: Per-procedure analysis state (`AnalysisState`)

pub mod analysis_state;
pub mod bounds;
pub mod symbolic_stack;
pub mod taint;

// Re-export main types for convenient access
pub use analysis_state::AnalysisState;
pub use bounds::{Bounds, FIELD_MODULUS, U32_MAX};
pub use symbolic_stack::SymbolicStack;
pub use taint::{TrackedValue, ValidationState, ValueOrigin};
