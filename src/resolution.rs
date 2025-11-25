use crate::util::extract_token_at_position;
use miden_assembly_syntax::ast::{
    LocalSymbolResolutionError, LocalSymbolResolver, Module, SymbolResolution,
};
use miden_debug_types::{DefaultSourceManager, SourceManager, Spanned, Uri};
use tower_lsp::lsp_types::{Position, Url};

#[derive(Debug, Clone)]
pub struct ResolvedSymbol {
    pub path: String,
    pub name: String,
}

/// Resolve the symbol under `position` in `module`.
pub fn resolve_symbol_at_position(
    uri: &Url,
    module: &Module,
    source_manager: &DefaultSourceManager,
    position: Position,
) -> Result<Option<ResolvedSymbol>, LocalSymbolResolutionError> {
    let source = source_manager.get_by_uri(&Uri::new(uri.as_str()));
    let Some(source_file) = source else {
        return Ok(None);
    };
    let Some(token) = extract_token_at_position(&source_file, position) else {
        return Ok(None);
    };

    if has_local_name(module, token.as_str()) {
        return Ok(Some(ResolvedSymbol {
            path: build_item_path(module, token.as_str()),
            name: token,
        }));
    }

    let resolver = LocalSymbolResolver::from(module);
    if let Some(resolution) = resolver.resolve(token.as_str())? {
        let resolved = match resolution {
            SymbolResolution::Local(span) => {
                let item = module.get(span.into_inner()).unwrap_or_else(|| {
                    panic!("invalid item index {:?}", span.into_inner());
                });
                let name = item.name();
                let path = build_item_path(module, name.as_str());
                ResolvedSymbol { path, name: token }
            }
            SymbolResolution::External(path) => {
                let local_path = build_item_path(module, token.as_str());
                if has_local_name(module, token.as_str()) {
                    ResolvedSymbol {
                        path: local_path,
                        name: token,
                    }
                } else {
                    ResolvedSymbol {
                        path: path.into_inner().as_str().to_string(),
                        name: token,
                    }
                }
            }
            SymbolResolution::Module { path, .. } => {
                let local_path = build_item_path(module, token.as_str());
                if has_local_name(module, token.as_str()) {
                    ResolvedSymbol {
                        path: local_path,
                        name: token,
                    }
                } else {
                    ResolvedSymbol {
                        path: path.as_str().to_string(),
                        name: token,
                    }
                }
            }
            SymbolResolution::Exact { path, .. } => ResolvedSymbol {
                path: path.into_inner().as_str().to_string(),
                name: token,
            },
            SymbolResolution::MastRoot(_) => return Ok(None),
        };
        return Ok(Some(resolved));
    }

    Ok(None)
}

pub fn build_item_path(module: &Module, name: &str) -> String {
    let mut buf = module.path().to_path_buf();
    buf.push(name);
    buf.to_string()
}

/// Resolve a symbol referenced by a span using the provided resolver.
pub fn resolve_symbol_at_span(
    module: &Module,
    resolver: &LocalSymbolResolver,
    target_span: miden_debug_types::SourceSpan,
) -> Option<String> {
    // Attempt to resolve by comparing spans with item names
    for item in module.items() {
        if item.name().span() == target_span {
            return Some(build_item_path(module, item.name().as_str()));
        }
    }

    // Try resolving via local symbol table by scanning imports/aliases
    for import in module.aliases() {
        if import.span() == target_span {
            if let Some(resolved) = resolver.resolve(import.name().as_str()).ok().flatten() {
                return match resolved {
                    SymbolResolution::Local(idx) => {
                        let item = module.get(idx.into_inner())?;
                        Some(build_item_path(module, item.name().as_str()))
                    }
                    SymbolResolution::External(path) => {
                        Some(path.into_inner().as_str().to_string())
                    }
                    SymbolResolution::Module { path, .. } => Some(path.as_str().to_string()),
                    SymbolResolution::Exact { path, .. } => {
                        Some(path.into_inner().as_str().to_string())
                    }
                    SymbolResolution::MastRoot(_) => None,
                };
            }
        }
    }

    None
}

fn has_local_name(module: &Module, name: &str) -> bool {
    module.items().any(|item| item.name().as_str() == name)
}
