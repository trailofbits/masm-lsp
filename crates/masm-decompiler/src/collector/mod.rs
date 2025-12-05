//! Decompilation hint collection and visitor logic.
//!
//! Traverses the AST and generates pseudocode hints for each procedure using
//! SSA-based tracking with phi nodes.

mod api;
mod handlers;
mod pseudocode_collector;
mod types;

#[cfg(test)]
mod tests;

pub use api::{collect_decompilation_hints, DecompilationResult};
