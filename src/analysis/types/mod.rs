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

pub mod bounds;
pub mod state;
pub mod taint;
pub mod symbolic_stack;

// Re-export main types for convenient access
pub use bounds::{Bounds, FIELD_MODULUS, U32_MAX};
pub use state::AnalysisState;
pub use symbolic_stack::SymbolicStack;
pub use taint::{TrackedValue, ValidationState, ValueOrigin};
