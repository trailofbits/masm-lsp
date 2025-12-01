use crate::symbol_path::SymbolPath;
use crate::util::{extract_token_at_position, to_miden_uri};
use miden_assembly_syntax::ast::{
    visit::Visit, InvocationTarget, LocalSymbolResolutionError, LocalSymbolResolver, Module,
    SymbolResolution,
};
use miden_debug_types::{DefaultSourceManager, SourceManager, Spanned};
use thiserror::Error;
use tower_lsp::lsp_types::{Position, Url};

/// Errors that can occur during symbol resolution.
#[derive(Debug, Error)]
pub enum ResolutionError {
    #[error("source file not found: {0}")]
    SourceNotFound(Url),

    #[error("no token at position {line}:{column}")]
    NoTokenAtPosition { line: u32, column: u32 },

    #[error("invalid position {line}:{column} in source")]
    InvalidPosition { line: u32, column: u32 },

    #[error("symbol resolution failed: {0}")]
    SymbolResolution(#[from] LocalSymbolResolutionError),

    #[error("symbol not found: {0}")]
    SymbolNotFound(String),

    #[error("invalid item index in module")]
    InvalidItemIndex,
}

#[derive(Debug, Clone)]
pub struct ResolvedSymbol {
    pub path: SymbolPath,
    pub name: String,
}

/// Resolve the symbol under `position` in `module`.
///
/// Returns `Ok(symbol)` if a symbol was found and resolved, or an error describing
/// why resolution failed.
pub fn resolve_symbol_at_position(
    uri: &Url,
    module: &Module,
    source_manager: &DefaultSourceManager,
    position: Position,
) -> Result<ResolvedSymbol, ResolutionError> {
    let source = source_manager.get_by_uri(&to_miden_uri(uri));
    let source_file = source.ok_or_else(|| ResolutionError::SourceNotFound(uri.clone()))?;

    // First, try to find an invocation target at this position using AST traversal.
    // This is more robust than token extraction as it uses the parsed AST structure.
    if let Some(offset) = position_to_offset(&source_file, position) {
        if let Some(target) = find_invocation_at_offset(module, offset) {
            return resolve_invocation_target(module, &target);
        }
    }

    // Fallback to token-based resolution for non-invocation symbols (e.g., proc definitions)
    let raw_token = extract_token_at_position(&source_file, position).ok_or(
        ResolutionError::NoTokenAtPosition {
            line: position.line,
            column: position.character,
        },
    )?;

    // Strip instruction prefixes if present (handles cursor on `exec.f` → `f`)
    let token = strip_instruction_prefix(&raw_token);

    if has_local_name(module, token.as_str()) {
        return Ok(ResolvedSymbol {
            path: SymbolPath::from_module_and_name(module, token.as_str()),
            name: token,
        });
    }

    let resolver = LocalSymbolResolver::from(module);
    if let Some(resolution) = resolver.resolve(token.as_str())? {
        let resolved = match resolution {
            SymbolResolution::Local(span) => {
                let item = module
                    .get(span.into_inner())
                    .ok_or(ResolutionError::InvalidItemIndex)?;
                let name = item.name();
                let path = SymbolPath::from_module_and_name(module, name.as_str());
                ResolvedSymbol { path, name: token }
            }
            SymbolResolution::External(path) => {
                let local_path = SymbolPath::from_module_and_name(module, token.as_str());
                if has_local_name(module, token.as_str()) {
                    ResolvedSymbol {
                        path: local_path,
                        name: token,
                    }
                } else {
                    ResolvedSymbol {
                        path: SymbolPath::new(path.into_inner().as_str()),
                        name: token,
                    }
                }
            }
            SymbolResolution::Module { path, .. } => {
                let local_path = SymbolPath::from_module_and_name(module, token.as_str());
                if has_local_name(module, token.as_str()) {
                    ResolvedSymbol {
                        path: local_path,
                        name: token,
                    }
                } else {
                    ResolvedSymbol {
                        path: SymbolPath::new(path.as_str()),
                        name: token,
                    }
                }
            }
            SymbolResolution::Exact { path, .. } => ResolvedSymbol {
                path: SymbolPath::new(path.into_inner().as_str()),
                name: token,
            },
            SymbolResolution::MastRoot(_) => {
                return Err(ResolutionError::SymbolNotFound(token));
            }
        };
        return Ok(resolved);
    }

    Err(ResolutionError::SymbolNotFound(token))
}

fn has_local_name(module: &Module, name: &str) -> bool {
    module.items().any(|item| item.name().as_str() == name)
}

/// Strip common instruction and definition prefixes from a token.
/// E.g., "exec.foo" → "foo", "call.bar::baz" → "bar::baz", "proc.foo" → "foo"
fn strip_instruction_prefix(token: &str) -> String {
    const PREFIXES: &[&str] = &[
        "exec.", "call.", "syscall.", "procref.", // invocation prefixes
        "proc.", "export.", "const.",             // definition prefixes
    ];
    for prefix in PREFIXES {
        if let Some(rest) = token.strip_prefix(prefix) {
            return rest.to_string();
        }
    }
    token.to_string()
}

/// Find an invocation target at a given byte offset using AST traversal.
pub fn find_invocation_at_offset(module: &Module, offset: u32) -> Option<InvocationTarget> {
    let mut finder = InvocationFinder {
        offset,
        found: None,
    };
    let _ = miden_assembly_syntax::ast::visit::visit_module(&mut finder, module);
    finder.found
}

struct InvocationFinder {
    offset: u32,
    found: Option<InvocationTarget>,
}

impl InvocationFinder {
    fn check_target(&mut self, target: &InvocationTarget, keyword_len: u32) {
        let span = target.span();
        let range = span.into_range();
        // Check if offset is in the target itself
        if self.offset >= range.start && self.offset < range.end {
            self.found = Some(target.clone());
            return;
        }
        // Also check if offset is in the instruction keyword before the target.
        // The keyword length includes the trailing dot (e.g., "exec." = 5, "syscall." = 8).
        if range.start >= keyword_len {
            let keyword_start = range.start - keyword_len;
            if self.offset >= keyword_start && self.offset < range.start {
                self.found = Some(target.clone());
            }
        }
    }
}

impl Visit for InvocationFinder {
    fn visit_exec(&mut self, target: &InvocationTarget) -> core::ops::ControlFlow<()> {
        self.check_target(target, 5); // "exec." = 5 chars
        if self.found.is_some() {
            core::ops::ControlFlow::Break(())
        } else {
            core::ops::ControlFlow::Continue(())
        }
    }

    fn visit_call(&mut self, target: &InvocationTarget) -> core::ops::ControlFlow<()> {
        self.check_target(target, 5); // "call." = 5 chars
        if self.found.is_some() {
            core::ops::ControlFlow::Break(())
        } else {
            core::ops::ControlFlow::Continue(())
        }
    }

    fn visit_syscall(&mut self, target: &InvocationTarget) -> core::ops::ControlFlow<()> {
        self.check_target(target, 8); // "syscall." = 8 chars
        if self.found.is_some() {
            core::ops::ControlFlow::Break(())
        } else {
            core::ops::ControlFlow::Continue(())
        }
    }

    fn visit_procref(&mut self, target: &InvocationTarget) -> core::ops::ControlFlow<()> {
        self.check_target(target, 8); // "procref." = 8 chars
        if self.found.is_some() {
            core::ops::ControlFlow::Break(())
        } else {
            core::ops::ControlFlow::Continue(())
        }
    }
}

/// Resolve an invocation target to a symbol path.
pub fn resolve_invocation_target(
    module: &Module,
    target: &InvocationTarget,
) -> Result<ResolvedSymbol, ResolutionError> {
    // Extract the original target string for the `name` field
    let target_str = match target {
        InvocationTarget::Symbol(ident) => ident.as_str().to_string(),
        InvocationTarget::Path(path) => path.inner().as_str().to_string(),
        InvocationTarget::MastRoot(_) => {
            return Err(ResolutionError::SymbolNotFound("MAST root".to_string()));
        }
    };

    // Use the unified symbol resolution service
    let resolver = crate::symbol_resolution::create_resolver(module);
    match resolver.resolve_target(target) {
        Some(path) => Ok(ResolvedSymbol {
            path,
            name: target_str,
        }),
        None => Err(ResolutionError::SymbolNotFound(target_str)),
    }
}

/// Convert an LSP position to a byte offset in the source file.
fn position_to_offset(source: &miden_debug_types::SourceFile, pos: Position) -> Option<u32> {
    let mut offset = 0usize;
    for (i, line) in source.as_str().split_inclusive('\n').enumerate() {
        if i as u32 == pos.line {
            if (pos.character as usize) <= line.len() {
                offset += pos.character as usize;
                return Some(offset as u32);
            } else {
                return None;
            }
        }
        offset += line.len();
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn strip_instruction_prefix_all_variants() {
        // Invocation prefixes
        assert_eq!(strip_instruction_prefix("exec.foo"), "foo");
        assert_eq!(strip_instruction_prefix("call.bar::baz"), "bar::baz");
        assert_eq!(strip_instruction_prefix("syscall.kernel_proc"), "kernel_proc");
        assert_eq!(strip_instruction_prefix("procref.target"), "target");

        // Definition prefixes
        assert_eq!(strip_instruction_prefix("proc.my_procedure"), "my_procedure");
        assert_eq!(strip_instruction_prefix("export.public_proc"), "public_proc");
        assert_eq!(strip_instruction_prefix("const.MY_CONST"), "MY_CONST");

        // Qualified paths with prefixes
        assert_eq!(strip_instruction_prefix("exec.::module::proc"), "::module::proc");

        // No prefix - returns unchanged
        assert_eq!(strip_instruction_prefix("foo"), "foo");
        assert_eq!(strip_instruction_prefix("bar::baz"), "bar::baz");
        assert_eq!(strip_instruction_prefix(""), "");

        // Partial matches - should not strip (e.g., "execute" starts with "exec" but not "exec.")
        assert_eq!(strip_instruction_prefix("execute"), "execute");
        assert_eq!(strip_instruction_prefix("caller"), "caller");
    }

    #[test]
    fn resolution_error_display() {
        let err = ResolutionError::SourceNotFound(Url::parse("file:///test.masm").unwrap());
        assert!(err.to_string().contains("source file not found"));

        let err = ResolutionError::NoTokenAtPosition { line: 5, column: 10 };
        assert!(err.to_string().contains("5:10"));

        let err = ResolutionError::InvalidPosition { line: 0, column: 0 };
        assert!(err.to_string().contains("invalid position"));

        let err = ResolutionError::SymbolNotFound("missing".to_string());
        assert!(err.to_string().contains("missing"));
    }
}
