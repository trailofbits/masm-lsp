//! Generated instruction metadata and semantics used across the tooling.
//!
//! Exposes the compile-time generated maps for instruction docs and
//! doc-derived stack effects so that other crates (analysis, LSP) can
//! share a single source of truth.

mod generated {
    include!(concat!(env!("OUT_DIR"), "/instruction_map.rs"));
}

mod instruction_type;
mod semantics;

pub use generated::InstructionInfo;
pub use generated::INSTRUCTION_MAP;
pub use instruction_type::{type_of, InstructionType, LocalOp, MemoryOp};
pub use semantics::{
    apply_stack_manipulation, semantics_of, InstructionEffect, StackLike, StackLikeResult, StackOp,
    StackOpExt,
};
