use std::collections::HashMap;

use miden_assembly_syntax::ast::{visit::Visit, InvocationTarget, Module};
use miden_debug_types::{DefaultSourceManager, Spanned};
use tower_lsp::lsp_types::{Location, Position, Range, Url};

use crate::diagnostics::span_to_range;
use crate::resolution::resolve_symbol_at_span;

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
pub struct UnresolvedReference {
    pub target: String,
    pub range: Range,
}

#[derive(Clone, Debug)]
pub struct DocumentSymbols {
    pub module: Box<Module>,
    pub definitions: Vec<Definition>,
    pub references: Vec<Reference>,
    pub unresolved: Vec<UnresolvedReference>,
}

impl DocumentSymbols {
    pub fn new(
        module: Box<Module>,
        definitions: Vec<Definition>,
        references: Vec<Reference>,
        unresolved: Vec<UnresolvedReference>,
    ) -> Self {
        Self {
            module,
            definitions,
            references,
            unresolved,
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
        self.definitions.iter().find_map(|(path, loc)| {
            if path.rsplit("::").next().map(|n| n == name).unwrap_or(false) {
                Some(loc.clone())
            } else {
                None
            }
        })
    }

    pub fn definition_by_suffix(&self, suffix: &str) -> Option<Location> {
        self.definitions.iter().find_map(|(path, loc)| {
            if path.ends_with(suffix) {
                Some(loc.clone())
            } else {
                None
            }
        })
    }

    pub fn references_by_suffix(&self, suffix: &str) -> Vec<Location> {
        self.references
            .iter()
            .filter(|(path, _)| path.ends_with(suffix))
            .flat_map(|(_, locs)| locs.clone())
            .collect()
    }

    pub fn definitions_by_suffix(&self, suffix: &str) -> Vec<Location> {
        self.definitions
            .iter()
            .filter(|(path, _)| path.ends_with(suffix))
            .map(|(_, loc)| loc.clone())
            .collect()
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
    let (refs, unresolved) = collect_references(&module, source_manager, &defs);
    DocumentSymbols::new(module, defs, refs, unresolved)
}

fn collect_definitions(module: &Module, source_manager: &DefaultSourceManager) -> Vec<Definition> {
    let mut defs = Vec::new();
    // Include module itself as a definition so module-level lookups can resolve.
    if let Some(range) = span_to_range(source_manager, module.span()) {
        defs.push(Definition {
            path: module.path().to_string(),
            range,
        });
    }
    for item in module.items() {
        let name = item.name().as_str();
        let path = build_item_path(module, name);
        let range = span_to_range(source_manager, item.name().span()).unwrap_or_else(zero_range);
        defs.push(Definition { path, range });
    }
    defs
}

fn defs_to_set(defs: &[Definition]) -> std::collections::HashSet<String> {
    defs.iter().map(|d| d.path.clone()).collect()
}

fn collect_references(
    module: &Module,
    source_manager: &DefaultSourceManager,
    defs: &[Definition],
) -> (Vec<Reference>, Vec<UnresolvedReference>) {
    let resolver = miden_assembly_syntax::ast::LocalSymbolResolver::from(module);
    let mut collector = InvocationCollector {
        module,
        resolver,
        source_manager,
        refs: Vec::new(),
        unresolved: Vec::new(),
        known_defs: defs_to_set(defs),
    };
    let _ = miden_assembly_syntax::ast::visit::visit_module(&mut collector, module);
    (collector.refs, collector.unresolved)
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
            Some(path.as_str().to_string())
        }
        miden_assembly_syntax::ast::InvocationTarget::MastRoot(_) => None,
    }
}

struct InvocationCollector<'a> {
    module: &'a Module,
    resolver: miden_assembly_syntax::ast::LocalSymbolResolver,
    source_manager: &'a DefaultSourceManager,
    refs: Vec<Reference>,
    unresolved: Vec<UnresolvedReference>,
    known_defs: std::collections::HashSet<String>,
}

impl<'a> InvocationCollector<'a> {
    fn push_target(&mut self, target: &InvocationTarget) {
        let resolved = resolve_symbol_at_span(self.module, &self.resolver, target.span());
        let path = resolved
            .clone()
            .or_else(|| infer_path_from_target(self.module, target));
        let range = span_to_range(self.source_manager, target.span()).unwrap_or_else(zero_range);
        match path {
            Some(path) => {
                if resolved.is_none()
                    && !self.known_defs.contains(&path)
                    && !self.known_defs.iter().any(|p| p.ends_with(&path))
                {
                    self.unresolved.push(UnresolvedReference {
                        target: target.to_string(),
                        range,
                    });
                }
                self.refs.push(Reference { path, range })
            }
            None => self.unresolved.push(UnresolvedReference {
                target: target.to_string(),
                range,
            }),
        }
    }
}

impl<'a> Visit for InvocationCollector<'a> {
    fn visit_exec(&mut self, target: &InvocationTarget) -> core::ops::ControlFlow<()> {
        self.push_target(target);
        core::ops::ControlFlow::Continue(())
    }

    fn visit_call(&mut self, target: &InvocationTarget) -> core::ops::ControlFlow<()> {
        self.push_target(target);
        core::ops::ControlFlow::Continue(())
    }

    fn visit_syscall(&mut self, target: &InvocationTarget) -> core::ops::ControlFlow<()> {
        self.push_target(target);
        core::ops::ControlFlow::Continue(())
    }

    fn visit_procref(&mut self, target: &InvocationTarget) -> core::ops::ControlFlow<()> {
        self.push_target(target);
        core::ops::ControlFlow::Continue(())
    }
}
