use std::collections::HashMap;

use miden_assembly_syntax::ast::{Export, Module};
use miden_debug_types::{DefaultSourceManager, Spanned};
use tower_lsp::lsp_types::{Location, Position, Range, Url};

use crate::diagnostics::span_to_range;

#[derive(Clone, Debug)]
pub struct Definition {
    pub path: String,
    pub range: Range,
}

#[derive(Clone, Debug)]
pub struct Reference {
    pub path: String,
    pub range: Range,
}

#[derive(Clone, Debug)]
pub struct DocumentSymbols {
    pub module: Box<Module>,
    pub definitions: Vec<Definition>,
    pub references: Vec<Reference>,
}

impl DocumentSymbols {
    pub fn new(
        module: Box<Module>,
        definitions: Vec<Definition>,
        references: Vec<Reference>,
    ) -> Self {
        Self {
            module,
            definitions,
            references,
        }
    }
}

#[derive(Default, Debug, Clone)]
pub struct WorkspaceIndex {
    definitions: HashMap<String, Location>,
    def_by_uri: HashMap<Url, Vec<String>>,
    references: HashMap<String, Vec<Location>>,
    refs_by_uri: HashMap<Url, Vec<String>>,
}

impl WorkspaceIndex {
    pub fn update_document(&mut self, uri: Url, defs: &[Definition], refs: &[Reference]) {
        if let Some(paths) = self.def_by_uri.remove(&uri) {
            for path in paths {
                self.definitions.remove(&path);
            }
        }
        if let Some(paths) = self.refs_by_uri.remove(&uri) {
            for path in paths {
                if let Some(entries) = self.references.get_mut(&path) {
                    entries.retain(|loc| loc.uri != uri);
                }
            }
        }

        let mut def_paths = Vec::with_capacity(defs.len());
        for def in defs {
            def_paths.push(def.path.clone());
            self.definitions
                .insert(def.path.clone(), Location::new(uri.clone(), def.range));
            if let Some(name) = def.path.rsplit("::").next() {
                self.definitions
                    .entry(name.to_string())
                    .or_insert(Location::new(uri.clone(), def.range));
            }
        }
        self.def_by_uri.insert(uri.clone(), def_paths);

        let mut ref_paths: Vec<String> = Vec::new();
        for r in refs {
            ref_paths.push(r.path.clone());
            self.references
                .entry(r.path.clone())
                .or_default()
                .push(Location::new(uri.clone(), r.range));
        }
        self.refs_by_uri.insert(uri, ref_paths);
    }

    pub fn definition(&self, path: &str) -> Option<Location> {
        self.definitions.get(path).cloned()
    }

    pub fn references(&self, path: &str) -> Vec<Location> {
        self.references.get(path).cloned().unwrap_or_default()
    }

    pub fn definition_by_name(&self, name: &str) -> Option<Location> {
        self.definitions
            .iter()
            .find_map(|(path, loc)| {
                if path.rsplit("::").next().map(|n| n == name).unwrap_or(false) {
                    Some(loc.clone())
                } else {
                    None
                }
            })
    }

    pub fn workspace_symbols(&self, query: &str) -> Vec<(String, Location)> {
        self.definitions
            .iter()
            .filter(|(name, _)| {
                name.contains(query)
                    || name
                        .rsplit("::")
                        .next()
                        .map(|last| last.contains(query))
                        .unwrap_or(false)
            })
            .map(|(name, loc)| (name.clone(), loc.clone()))
            .collect()
    }
}

pub fn build_document_symbols(
    module: Box<Module>,
    source_manager: &DefaultSourceManager,
) -> DocumentSymbols {
    let defs = collect_definitions(&module, source_manager);
    let refs = collect_references(&module, source_manager);
    DocumentSymbols::new(module, defs, refs)
}

fn collect_definitions(module: &Module, source_manager: &DefaultSourceManager) -> Vec<Definition> {
    let mut defs = Vec::new();
    for item in module.items() {
        let name = item.name().as_str();
        let path = build_item_path(module, name);
        let range = span_to_range(source_manager, item.name().span()).unwrap_or_else(zero_range);
        defs.push(Definition { path, range });
    }
    defs
}

fn collect_references(module: &Module, source_manager: &DefaultSourceManager) -> Vec<Reference> {
    let mut refs = Vec::new();

    for item in module.items() {
        if let Export::Procedure(proc) = item {
            for invoke in proc.invoked() {
                let path = infer_path_from_target(module, &invoke.target);
                let Some(path) = path else { continue };
                let range =
                    span_to_range(source_manager, invoke.target.span()).unwrap_or_else(zero_range);
                refs.push(Reference { path, range });
            }
        }
    }
    refs
}

fn build_item_path(module: &Module, name: &str) -> String {
    let mut buf = module.path().to_path_buf();
    buf.push(name);
    buf.to_string()
}

fn zero_range() -> Range {
    Range::new(Position::new(0, 0), Position::new(0, 0))
}

fn infer_path_from_target(
    module: &Module,
    target: &miden_assembly_syntax::ast::InvocationTarget,
) -> Option<String> {
    match target {
        miden_assembly_syntax::ast::InvocationTarget::Symbol(ident) => {
            Some(build_item_path(module, ident.as_str()))
        }
        miden_assembly_syntax::ast::InvocationTarget::Path(path) => {
            let path = path.inner();
            if path.is_absolute() {
                Some(path.as_str().to_string())
            } else {
                Some(build_item_path(module, path.as_str()))
            }
        }
        miden_assembly_syntax::ast::InvocationTarget::MastRoot(_) => None,
    }
}
