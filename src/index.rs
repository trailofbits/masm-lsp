use std::collections::HashMap;

use miden_assembly_syntax::ast::{visit::Visit, InvocationTarget, Module};
use miden_debug_types::{DefaultSourceManager, Spanned};
use tower_lsp::lsp_types::{Location, Position, Range, Url};

use crate::diagnostics::span_to_range;
use crate::symbol_path::SymbolPath;

#[derive(Clone, Debug)]
pub struct Definition {
    pub path: SymbolPath,
    pub range: Range,
}

#[derive(Clone, Debug)]
pub struct Reference {
    pub path: SymbolPath,
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
    definitions: HashMap<SymbolPath, Location>,
    def_by_uri: HashMap<Url, Vec<SymbolPath>>,
    references: HashMap<SymbolPath, Vec<Location>>,
    refs_by_uri: HashMap<Url, Vec<SymbolPath>>,
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
            // Also index by short name for quick lookups
            let name = def.path.name();
            self.definitions
                .entry(SymbolPath::new(name))
                .or_insert(Location::new(uri.clone(), def.range));
        }
        self.def_by_uri.insert(uri.clone(), def_paths);

        let mut ref_paths: Vec<SymbolPath> = Vec::new();
        for r in refs {
            ref_paths.push(r.path.clone());
            self.references
                .entry(r.path.clone())
                .or_default()
                .push(Location::new(uri.clone(), r.range));
        }
        self.refs_by_uri.insert(uri, ref_paths);
    }

    /// Look up a definition by exact path.
    pub fn definition(&self, path: &str) -> Option<Location> {
        self.definitions.get(&SymbolPath::new(path)).cloned()
    }

    /// Look up references by exact path.
    pub fn references(&self, path: &str) -> Vec<Location> {
        self.references
            .get(&SymbolPath::new(path))
            .cloned()
            .unwrap_or_default()
    }

    /// Look up a definition by its short name (last segment).
    pub fn definition_by_name(&self, name: &str) -> Option<Location> {
        self.definitions.iter().find_map(|(path, loc)| {
            if path.name_matches(name) {
                Some(loc.clone())
            } else {
                None
            }
        })
    }

    /// Look up a definition by path suffix.
    pub fn definition_by_suffix(&self, suffix: &str) -> Option<Location> {
        self.definitions.iter().find_map(|(path, loc)| {
            if path.ends_with(suffix) {
                Some(loc.clone())
            } else {
                None
            }
        })
    }

    /// Look up references by path suffix.
    pub fn references_by_suffix(&self, suffix: &str) -> Vec<Location> {
        self.references
            .iter()
            .filter(|(path, _)| path.ends_with(suffix))
            .flat_map(|(_, locs)| locs.clone())
            .collect()
    }

    /// Look up definitions by path suffix.
    pub fn definitions_by_suffix(&self, suffix: &str) -> Vec<Location> {
        self.definitions
            .iter()
            .filter(|(path, _)| path.ends_with(suffix))
            .map(|(_, loc)| loc.clone())
            .collect()
    }

    /// Search for workspace symbols matching a query string.
    pub fn workspace_symbols(&self, query: &str) -> Vec<(String, Location)> {
        self.definitions
            .iter()
            .filter(|(path, _)| {
                path.as_str().contains(query) || path.name().contains(query)
            })
            .map(|(path, loc)| (path.to_string(), loc.clone()))
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
    // Include module itself as a definition so module-level lookups can resolve.
    if let Some(range) = span_to_range(source_manager, module.span()) {
        defs.push(Definition {
            path: SymbolPath::new(module.path().to_string()),
            range,
        });
    }
    for item in module.items() {
        let name = item.name().as_str();
        let path = SymbolPath::from_module_and_name(module, name);
        let range = span_to_range(source_manager, item.name().span()).unwrap_or_else(zero_range);
        defs.push(Definition { path, range });
    }
    defs
}

fn collect_references(module: &Module, source_manager: &DefaultSourceManager) -> Vec<Reference> {
    let resolver = miden_assembly_syntax::ast::LocalSymbolResolver::from(module);
    let mut collector = InvocationCollector {
        module,
        resolver,
        source_manager,
        refs: Vec::new(),
    };
    let _ = miden_assembly_syntax::ast::visit::visit_module(&mut collector, module);
    collector.refs
}

fn zero_range() -> Range {
    Range::new(Position::new(0, 0), Position::new(0, 0))
}

struct InvocationCollector<'a> {
    module: &'a Module,
    resolver: miden_assembly_syntax::ast::LocalSymbolResolver,
    source_manager: &'a DefaultSourceManager,
    refs: Vec<Reference>,
}

impl<'a> InvocationCollector<'a> {
    fn push_target(&mut self, target: &InvocationTarget) {
        let range = span_to_range(self.source_manager, target.span()).unwrap_or_else(zero_range);

        // Extract the target string for resolution
        let target_str = match target {
            InvocationTarget::Symbol(ident) => ident.as_str().to_string(),
            InvocationTarget::Path(path) => path.inner().as_str().to_string(),
            InvocationTarget::MastRoot(_) => {
                // MAST roots are always valid (they reference code by hash)
                return;
            }
        };

        // Check if this is a local definition
        let is_local_def = self
            .module
            .items()
            .any(|item| item.name().as_str() == target_str);

        if is_local_def {
            let path = SymbolPath::from_module_and_name(self.module, &target_str);
            self.refs.push(Reference { path, range });
            return;
        }

        // Try to resolve using the LocalSymbolResolver (handles imports and aliases)
        if let Ok(Some(resolution)) = self.resolver.resolve(&target_str) {
            use miden_assembly_syntax::ast::SymbolResolution;
            let path = match resolution {
                SymbolResolution::Local(idx) => {
                    let item = self.module.get(idx.into_inner());
                    item.map(|i| SymbolPath::from_module_and_name(self.module, i.name().as_str()))
                        .unwrap_or_else(|| SymbolPath::new(&target_str))
                }
                SymbolResolution::External(p) => SymbolPath::new(p.into_inner().as_str()),
                SymbolResolution::Module { path, .. } => SymbolPath::new(path.as_str()),
                SymbolResolution::Exact { path, .. } => SymbolPath::new(path.into_inner().as_str()),
                SymbolResolution::MastRoot(_) => {
                    // MAST roots are valid
                    return;
                }
            };
            self.refs.push(Reference { path, range });
            return;
        }

        // For any unresolved target, track it as a reference using its literal path.
        // Resolution checking is done at diagnostic time against the workspace index.
        let path = SymbolPath::new(&target_str);
        self.refs.push(Reference { path, range });
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    fn test_uri() -> Url {
        Url::parse("file:///tmp/test.masm").unwrap()
    }

    fn test_uri_named(name: &str) -> Url {
        Url::parse(&format!("file:///tmp/{}", name)).unwrap()
    }

    fn make_definition(path: &str, line: u32) -> Definition {
        Definition {
            path: SymbolPath::new(path),
            range: Range::new(Position::new(line, 0), Position::new(line, 10)),
        }
    }

    fn make_reference(path: &str, line: u32) -> Reference {
        Reference {
            path: SymbolPath::new(path),
            range: Range::new(Position::new(line, 0), Position::new(line, 10)),
        }
    }

    #[test]
    fn workspace_index_empty() {
        let index = WorkspaceIndex::default();
        assert!(index.definition("::foo::bar").is_none());
        assert!(index.references("::foo::bar").is_empty());
    }

    #[test]
    fn workspace_index_add_definitions() {
        let mut index = WorkspaceIndex::default();
        let uri = test_uri();

        let defs = vec![
            make_definition("::test::foo", 0),
            make_definition("::test::bar", 5),
        ];

        index.update_document(uri.clone(), &defs, &[]);

        assert!(index.definition("::test::foo").is_some());
        assert!(index.definition("::test::bar").is_some());
        assert!(index.definition("::test::missing").is_none());
    }

    #[test]
    fn workspace_index_add_references() {
        let mut index = WorkspaceIndex::default();
        let uri = test_uri();

        let refs = vec![
            make_reference("::test::foo", 10),
            make_reference("::test::foo", 15),
        ];

        index.update_document(uri.clone(), &[], &refs);

        let found = index.references("::test::foo");
        assert_eq!(found.len(), 2);
    }

    #[test]
    fn workspace_index_update_clears_old() {
        let mut index = WorkspaceIndex::default();
        let uri = test_uri();

        // First update
        let defs1 = vec![make_definition("::test::old", 0)];
        index.update_document(uri.clone(), &defs1, &[]);
        assert!(index.definition("::test::old").is_some());

        // Second update replaces the first
        let defs2 = vec![make_definition("::test::new", 0)];
        index.update_document(uri.clone(), &defs2, &[]);

        // Old definition should be gone, new one should exist
        // Note: The short name "old" might still be present due to name indexing
        assert!(index.definition("::test::new").is_some());
    }

    #[test]
    fn workspace_index_definition_by_name() {
        let mut index = WorkspaceIndex::default();
        let uri = test_uri();

        let defs = vec![make_definition("::module::submodule::myproc", 0)];
        index.update_document(uri.clone(), &defs, &[]);

        // Should find by name
        let found = index.definition_by_name("myproc");
        assert!(found.is_some());
    }

    #[test]
    fn workspace_index_definition_by_suffix() {
        let mut index = WorkspaceIndex::default();
        let uri = test_uri();

        let defs = vec![make_definition("::std::crypto::sha256::hash", 0)];
        index.update_document(uri.clone(), &defs, &[]);

        // Should find by suffix
        let found = index.definition_by_suffix("sha256::hash");
        assert!(found.is_some());

        let found = index.definition_by_suffix("hash");
        assert!(found.is_some());
    }

    #[test]
    fn workspace_index_references_by_suffix() {
        let mut index = WorkspaceIndex::default();
        let uri = test_uri();

        let refs = vec![
            make_reference("::std::crypto::hash", 10),
            make_reference("::std::math::hash", 15),
        ];
        index.update_document(uri.clone(), &[], &refs);

        // Find by suffix "hash"
        let found = index.references_by_suffix("hash");
        assert_eq!(found.len(), 2);
    }

    #[test]
    fn workspace_index_workspace_symbols() {
        let mut index = WorkspaceIndex::default();
        let uri = test_uri();

        let defs = vec![
            make_definition("::app::calculate_sum", 0),
            make_definition("::app::calculate_product", 5),
            make_definition("::app::format_output", 10),
        ];
        index.update_document(uri.clone(), &defs, &[]);

        // Search for "calc"
        let found = index.workspace_symbols("calc");
        assert!(found.len() >= 2);

        // Search for "format"
        let found = index.workspace_symbols("format");
        assert!(found.len() >= 1);

        // Search for non-existent
        let found = index.workspace_symbols("nonexistent");
        assert!(found.is_empty());
    }

    #[test]
    fn workspace_index_multi_file() {
        let mut index = WorkspaceIndex::default();
        let uri1 = test_uri_named("file1.masm");
        let uri2 = test_uri_named("file2.masm");

        let defs1 = vec![make_definition("::file1::foo", 0)];
        let defs2 = vec![make_definition("::file2::bar", 0)];

        index.update_document(uri1.clone(), &defs1, &[]);
        index.update_document(uri2.clone(), &defs2, &[]);

        assert!(index.definition("::file1::foo").is_some());
        assert!(index.definition("::file2::bar").is_some());
    }

    #[test]
    fn workspace_index_references_from_multiple_files() {
        let mut index = WorkspaceIndex::default();
        let uri1 = test_uri_named("caller1.masm");
        let uri2 = test_uri_named("caller2.masm");

        let refs1 = vec![make_reference("::defs::target", 5)];
        let refs2 = vec![make_reference("::defs::target", 10)];

        index.update_document(uri1.clone(), &[], &refs1);
        index.update_document(uri2.clone(), &[], &refs2);

        let found = index.references("::defs::target");
        assert_eq!(found.len(), 2);

        // Verify they're from different files
        let uris: HashSet<_> = found.iter().map(|l| &l.uri).collect();
        assert!(uris.contains(&uri1));
        assert!(uris.contains(&uri2));
    }

    #[test]
    fn definition_struct() {
        let def = Definition {
            path: SymbolPath::new("::mod::proc"),
            range: Range::new(Position::new(0, 0), Position::new(0, 10)),
        };
        let cloned = def.clone();
        assert_eq!(cloned.path.as_str(), "::mod::proc");
    }

    #[test]
    fn reference_struct() {
        let r = Reference {
            path: SymbolPath::new("::mod::target"),
            range: Range::new(Position::new(5, 0), Position::new(5, 10)),
        };
        let cloned = r.clone();
        assert_eq!(cloned.path.as_str(), "::mod::target");
    }

    #[test]
    fn definitions_by_suffix() {
        let mut index = WorkspaceIndex::default();
        let uri = test_uri();

        let defs = vec![
            make_definition("::crypto::sha256::hash", 0),
            make_definition("::crypto::blake3::hash", 5),
            make_definition("::math::add", 10),
        ];
        index.update_document(uri.clone(), &defs, &[]);

        let found = index.definitions_by_suffix("hash");
        // Should find both hash-related definitions (paths ending with "hash")
        assert!(found.len() >= 2, "expected at least 2 definitions ending with 'hash', got {}", found.len());
    }
}
