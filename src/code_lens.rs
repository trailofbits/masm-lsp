use miden_assembly_syntax::ast::Module;
use miden_debug_types::DefaultSourceManager;
use tower_lsp::lsp_types::CodeLens;

/// Collect code lenses for a module.
///
/// Currently disabled; returns an empty list.
pub fn collect_code_lenses(_module: &Module, _sources: &DefaultSourceManager) -> Vec<CodeLens> {
    Vec::new()
}
