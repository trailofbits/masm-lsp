use crate::SymbolPath;
use std::sync::Arc;

use crate::util::to_miden_uri;
use miden_assembly_syntax::ast::{visit::Visit, Immediate, InvocationTarget, Module, Procedure};
use miden_assembly_syntax::parser::{PushValue, WordValue};
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

    #[error("symbol not found: {0}")]
    SymbolNotFound(String),
}

#[derive(Debug, Clone)]
pub struct ResolvedSymbol {
    pub path: SymbolPath,
    pub name: String,
}

/// Find a constant symbol candidate under `position`.
///
/// Returns the token text that should be resolved as a constant symbol/path, e.g.:
/// - `FOO` from `push.FOO`
/// - `constants::FOO` from `push.constants::FOO`
/// - `FOO` from `const FOO = ...`
pub fn find_constant_candidate_at_position(
    uri: &Url,
    module: &Module,
    source_manager: Arc<DefaultSourceManager>,
    position: Position,
) -> Result<String, ResolutionError> {
    let source = source_manager.get_by_uri(&to_miden_uri(uri));
    let source_file = source.ok_or_else(|| ResolutionError::SourceNotFound(uri.clone()))?;

    let offset =
        position_to_offset(&source_file, position).ok_or(ResolutionError::InvalidPosition {
            line: position.line,
            column: position.character,
        })?;

    // Constant definition name (e.g. `const FOO = 1`)
    if let Some(name) = find_constant_definition_at_offset(module, offset) {
        return Ok(name);
    }

    // Constant reference in constant expressions (e.g. `const BAR = FOO + 1`)
    if let Some(path) = find_constant_expr_ref_at_offset(module, offset) {
        return Ok(path);
    }

    // Constant-like immediates (e.g. `push.FOO`, including folded immediates where the
    // AST value is literal but source span still points at the original token).
    if let Some(name) = find_constant_immediate_at_offset(module, source_file.as_str(), offset) {
        return Ok(name);
    }

    Err(ResolutionError::NoTokenAtPosition {
        line: position.line,
        column: position.character,
    })
}

/// Resolve the symbol under `position` in `module`.
///
/// Uses AST traversal to find symbols at the cursor position:
/// 1. Invocation targets (exec.foo, call.bar, etc.)
/// 2. Procedure definitions (proc foo, export bar)
///
/// Returns `Ok(symbol)` if a symbol was found and resolved, or an error describing
/// why resolution failed.
pub fn resolve_symbol_at_position(
    uri: &Url,
    module: &Module,
    source_manager: Arc<DefaultSourceManager>,
    position: Position,
) -> Result<ResolvedSymbol, ResolutionError> {
    let source = source_manager.get_by_uri(&to_miden_uri(uri));
    let source_file = source.ok_or_else(|| ResolutionError::SourceNotFound(uri.clone()))?;

    let offset =
        position_to_offset(&source_file, position).ok_or(ResolutionError::InvalidPosition {
            line: position.line,
            column: position.character,
        })?;

    // Try to find an invocation target at this position (exec.foo, call.bar, etc.)
    if let Some(target) = find_invocation_at_offset(module, offset) {
        return resolve_invocation_target(module, source_manager.clone(), &target);
    }

    // Try to find a procedure definition at this position (proc foo, export bar)
    if let Some(proc_name) = find_procedure_definition_at_offset(module, offset) {
        return resolve_procedure_definition(module, source_manager.clone(), &proc_name);
    }

    Err(ResolutionError::NoTokenAtPosition {
        line: position.line,
        column: position.character,
    })
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

/// Find a procedure definition name at a given byte offset using AST traversal.
///
/// Returns the procedure name if the offset is on a procedure name in its definition.
pub fn find_procedure_definition_at_offset(module: &Module, offset: u32) -> Option<String> {
    let mut finder = ProcDefinitionFinder {
        offset,
        found: None,
    };
    let _ = miden_assembly_syntax::ast::visit::visit_module(&mut finder, module);
    finder.found
}

struct ProcDefinitionFinder {
    offset: u32,
    found: Option<String>,
}

impl Visit for ProcDefinitionFinder {
    fn visit_procedure(&mut self, proc: &Procedure) -> core::ops::ControlFlow<()> {
        let name_span = proc.name().span();
        let range = name_span.into_range();
        if self.offset >= range.start && self.offset < range.end {
            self.found = Some(proc.name().as_str().to_string());
            return core::ops::ControlFlow::Break(());
        }
        // Continue to check nested procedures/invocations
        core::ops::ControlFlow::Continue(())
    }
}

/// Resolve a procedure definition to a symbol path.
fn resolve_procedure_definition(
    module: &Module,
    source_manager: Arc<DefaultSourceManager>,
    name: &str,
) -> Result<ResolvedSymbol, ResolutionError> {
    // Use the unified symbol resolution service
    let resolver = crate::symbol_resolution::create_resolver(module, source_manager);
    let path = resolver
        .resolve_symbol(name)
        .map_err(|_| ResolutionError::SymbolNotFound(name.to_string()))?;
    Ok(ResolvedSymbol {
        path,
        name: name.to_string(),
    })
}

/// Resolve an invocation target to a symbol path.
pub fn resolve_invocation_target(
    module: &Module,
    source_manager: Arc<DefaultSourceManager>,
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
    let resolver = crate::symbol_resolution::create_resolver(module, source_manager);
    match resolver.resolve_target(target) {
        Ok(Some(path)) => Ok(ResolvedSymbol {
            path,
            name: target_str,
        }),
        Ok(None) | Err(_) => Err(ResolutionError::SymbolNotFound(target_str)),
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

fn find_constant_definition_at_offset(module: &Module, offset: u32) -> Option<String> {
    for constant in module.constants() {
        let range = constant.name().span().into_range();
        if offset >= range.start && offset < range.end {
            return Some(constant.name().as_str().to_string());
        }
    }
    None
}

fn find_constant_expr_ref_at_offset(module: &Module, offset: u32) -> Option<String> {
    let mut finder = ConstantExprRefFinder {
        offset,
        found: None,
    };
    let _ = miden_assembly_syntax::ast::visit::visit_module(&mut finder, module);
    finder.found
}

struct ConstantExprRefFinder {
    offset: u32,
    found: Option<String>,
}

impl Visit for ConstantExprRefFinder {
    fn visit_constant_ref(
        &mut self,
        path: &miden_debug_types::Span<std::sync::Arc<miden_assembly_syntax::ast::Path>>,
    ) -> core::ops::ControlFlow<()> {
        let range = path.span().into_range();
        if self.offset >= range.start && self.offset < range.end {
            self.found = Some(path.inner().as_str().to_string());
            return core::ops::ControlFlow::Break(());
        }
        core::ops::ControlFlow::Continue(())
    }
}

fn find_constant_immediate_at_offset(module: &Module, source: &str, offset: u32) -> Option<String> {
    let mut finder = ConstantImmediateFinder {
        offset,
        source,
        found: None,
    };
    let _ = miden_assembly_syntax::ast::visit::visit_module(&mut finder, module);
    finder.found
}

struct ConstantImmediateFinder<'a> {
    offset: u32,
    source: &'a str,
    found: Option<String>,
}

impl ConstantImmediateFinder<'_> {
    fn check_immediate<T>(&mut self, immediate: &Immediate<T>) -> core::ops::ControlFlow<()> {
        let span = immediate.span();
        let range = span.into_range();
        if self.offset < range.start || self.offset >= range.end {
            return core::ops::ControlFlow::Continue(());
        }

        let candidate = match immediate {
            Immediate::Constant(ident) => Some(ident.as_str().to_string()),
            Immediate::Value(_) => {
                extract_token_in_span(self.source, range.start, range.end, self.offset)
            }
        };

        if let Some(name) = candidate {
            self.found = Some(name);
            core::ops::ControlFlow::Break(())
        } else {
            core::ops::ControlFlow::Continue(())
        }
    }
}

impl Visit for ConstantImmediateFinder<'_> {
    fn visit_immediate_push_value(
        &mut self,
        immediate: &Immediate<PushValue>,
    ) -> core::ops::ControlFlow<()> {
        self.check_immediate(immediate)
    }

    fn visit_immediate_word_value(
        &mut self,
        immediate: &Immediate<WordValue>,
    ) -> core::ops::ControlFlow<()> {
        self.check_immediate(immediate)
    }

    fn visit_immediate_felt(
        &mut self,
        immediate: &Immediate<miden_assembly_syntax::Felt>,
    ) -> core::ops::ControlFlow<()> {
        self.check_immediate(immediate)
    }

    fn visit_immediate_u32(&mut self, immediate: &Immediate<u32>) -> core::ops::ControlFlow<()> {
        self.check_immediate(immediate)
    }

    fn visit_immediate_u16(&mut self, immediate: &Immediate<u16>) -> core::ops::ControlFlow<()> {
        self.check_immediate(immediate)
    }

    fn visit_immediate_u8(&mut self, immediate: &Immediate<u8>) -> core::ops::ControlFlow<()> {
        self.check_immediate(immediate)
    }

    fn visit_immediate_error_message(
        &mut self,
        immediate: &Immediate<std::sync::Arc<str>>,
    ) -> core::ops::ControlFlow<()> {
        self.check_immediate(immediate)
    }
}

fn extract_token_in_span(source: &str, start: u32, end: u32, offset: u32) -> Option<String> {
    let start = start as usize;
    let end = end as usize;
    let offset = offset as usize;
    if start >= end || end > source.len() || offset < start || offset >= end {
        return None;
    }

    let span_text = source.get(start..end)?;
    let rel = offset - start;
    let bytes = span_text.as_bytes();

    let mut left = rel;
    while left > 0 && is_constant_token_char(bytes[left - 1] as char) {
        left -= 1;
    }

    let mut right = rel;
    while right < bytes.len() && is_constant_token_char(bytes[right] as char) {
        right += 1;
    }

    let token = span_text.get(left..right)?.trim();
    if token.is_empty() {
        None
    } else {
        Some(token.to_string())
    }
}

fn is_constant_token_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || matches!(c, '_' | ':' | '$')
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn resolution_error_display() {
        let err = ResolutionError::SourceNotFound(Url::parse("file:///test.masm").unwrap());
        assert!(err.to_string().contains("source file not found"));

        let err = ResolutionError::NoTokenAtPosition {
            line: 5,
            column: 10,
        };
        assert!(err.to_string().contains("5:10"));

        let err = ResolutionError::InvalidPosition { line: 0, column: 0 };
        assert!(err.to_string().contains("invalid position"));

        let err = ResolutionError::SymbolNotFound("missing".to_string());
        assert!(err.to_string().contains("missing"));
    }
}
