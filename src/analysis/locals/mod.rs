//! Uninitialized local variable detection.
//!
//! This module provides analysis to detect reads from local variables that
//! may not have been initialized. In Miden assembly, procedure-local memory
//! is NOT initialized to zero, so reading before writing produces garbage.
//!
//! # Analysis Overview
//!
//! The analysis tracks initialization state for each local variable index
//! using a three-state lattice:
//! - `Uninitialized`: Local has not been written on any path
//! - `MaybeInitialized`: Local has been written on some but not all paths
//! - `Initialized`: Local has been written on all paths
//!
//! # Control Flow Handling
//!
//! - **If/Else**: States from both branches are merged using the meet operation.
//!   A local is only `Initialized` if initialized in both branches.
//! - **While loops**: Since the loop may not execute, initializations inside
//!   the body result in `MaybeInitialized` state after the loop.
//! - **Repeat loops**: For count > 0, the loop always executes, so
//!   initializations in the body are reliable.
//!
//! # Example
//!
//! ```text
//! proc.example.2
//!     loc_load.0      # WARNING: read from uninitialized local 0
//!     drop
//!
//!     if.true
//!         push.42
//!         loc_store.1
//!     end
//!     loc_load.1      # WARNING: local 1 may not be initialized on all code paths
//!     drop
//! end
//! ```

mod analyzer;
mod state;

pub use analyzer::analyze_locals;
pub use state::{LocalState, LocalsState};
