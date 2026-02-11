//! LSP server implementation for MASM.
//!
//! This module wires together the backend, cache, and LSP handlers.

mod backend;
mod cache;
mod config;
mod helpers;
mod lsp;

#[cfg(test)]
mod tests;

pub use backend::Backend;
pub use config::{
    extract_code_lens_stack_effects, extract_inlay_hint_type, extract_library_paths,
};
pub use helpers::{
    determine_module_kind_from_ast, extract_doc_comment, extract_procedure_signature,
    is_on_use_statement,
};
