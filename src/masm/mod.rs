//! MASM-specific language services.
//!
//! Contains the parser integration, symbol resolution, diagnostics, and
//! LSP feature implementations that are specific to Miden Assembly source
//! files (`.masm`).

pub mod code_lens;
pub mod cursor_resolution;
pub mod diagnostics;
pub mod index;
pub mod inlay_hints;
pub mod module_path;
