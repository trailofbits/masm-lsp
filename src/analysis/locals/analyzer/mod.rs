//! Analyzer for detecting uninitialized local variable access.

mod analyzer_impl;

#[cfg(test)]
mod tests;

pub use analyzer_impl::{analyze_locals, analyze_locals_with_contracts};
