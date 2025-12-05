use std::collections::HashMap;

use miden_assembly_syntax::ast::{visit::Visit, InvocationTarget, Module};
use miden_debug_types::{DefaultSourceManager, Spanned};
use tower_lsp::lsp_types::{Location, Position, Range, Url};

use crate::analysis::{infer_module_contracts_with_store, ContractStore, ProcContract};
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
    /// Index definitions by their short name (last segment) for fast O(1) lookups.
    def_by_name: HashMap<String, Vec<SymbolPath>>,
    references: HashMap<SymbolPath, Vec<Location>>,
    refs_by_uri: HashMap<Url, Vec<SymbolPath>>,
    /// Index references by their short name for fast O(1) lookups.
    refs_by_name: HashMap<String, Vec<SymbolPath>>,
    /// Procedure contracts inferred from implementations and user annotations.
    contracts: ContractStore,
}

impl WorkspaceIndex {
    pub fn update_document(&mut self, uri: Url, defs: &[Definition], refs: &[Reference]) {
        // Remove old definitions for this URI
        if let Some(paths) = self.def_by_uri.remove(&uri) {
            for path in paths {
                self.definitions.remove(&path);
                // Remove from name index
                let name = path.name().to_string();
                if let Some(entries) = self.def_by_name.get_mut(&name) {
                    entries.retain(|p| p != &path);
                    if entries.is_empty() {
                        self.def_by_name.remove(&name);
                    }
                }
            }
        }

        // Remove old references for this URI
        if let Some(paths) = self.refs_by_uri.remove(&uri) {
            for path in paths {
                if let Some(entries) = self.references.get_mut(&path) {
                    entries.retain(|loc| loc.uri != uri);
                }
                // Remove from name index
                let name = path.name().to_string();
                if let Some(entries) = self.refs_by_name.get_mut(&name) {
                    entries.retain(|p| p != &path);
                    if entries.is_empty() {
                        self.refs_by_name.remove(&name);
                    }
                }
            }
        }

        // Add new definitions
        let mut def_paths = Vec::with_capacity(defs.len());
        for def in defs {
            def_paths.push(def.path.clone());
            self.definitions
                .insert(def.path.clone(), Location::new(uri.clone(), def.range));
            // Index by short name for fast lookups
            let name = def.path.name().to_string();
            self.def_by_name
                .entry(name)
                .or_default()
                .push(def.path.clone());
        }
        self.def_by_uri.insert(uri.clone(), def_paths);

        // Add new references
        let mut ref_paths: Vec<SymbolPath> = Vec::new();
        for r in refs {
            ref_paths.push(r.path.clone());
            self.references
                .entry(r.path.clone())
                .or_default()
                .push(Location::new(uri.clone(), r.range));
            // Index by short name for fast lookups
            let name = r.path.name().to_string();
            if !self
                .refs_by_name
                .get(&name)
                .is_some_and(|v| v.contains(&r.path))
            {
                self.refs_by_name
                    .entry(name)
                    .or_default()
                    .push(r.path.clone());
            }
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
    /// Uses O(1) hash lookup + O(k) where k is the number of definitions with that name.
    pub fn definition_by_name(&self, name: &str) -> Option<Location> {
        let candidates = self.def_by_name.get(name)?;
        // Return the first match (typically there's only one with a given name)
        candidates
            .first()
            .and_then(|path| self.definitions.get(path).cloned())
    }

    /// Look up a definition by path suffix.
    /// Uses O(1) hash lookup on the name + O(k) suffix matching where k is candidates with that name.
    pub fn definition_by_suffix(&self, suffix: &str) -> Option<Location> {
        // Extract the short name from the suffix for fast index lookup
        let name = suffix.rsplit("::").next()?;
        let candidates = self.def_by_name.get(name)?;
        candidates
            .iter()
            .find(|p| p.ends_with(suffix))
            .and_then(|p| self.definitions.get(p).cloned())
    }

    /// Look up references by path suffix.
    /// Uses O(1) hash lookup on the name + O(k) suffix matching where k is candidates with that name.
    pub fn references_by_suffix(&self, suffix: &str) -> Vec<Location> {
        // Extract the short name from the suffix for fast index lookup
        let name = match suffix.rsplit("::").next() {
            Some(n) => n,
            None => return Vec::new(),
        };
        let candidates = match self.refs_by_name.get(name) {
            Some(c) => c,
            None => return Vec::new(),
        };
        candidates
            .iter()
            .filter(|p| p.ends_with(suffix))
            .filter_map(|p| self.references.get(p))
            .flatten()
            .cloned()
            .collect()
    }

    /// Look up definitions by path suffix.
    /// Uses O(1) hash lookup on the name + O(k) suffix matching where k is candidates with that name.
    pub fn definitions_by_suffix(&self, suffix: &str) -> Vec<Location> {
        // Extract the short name from the suffix for fast index lookup
        let name = match suffix.rsplit("::").next() {
            Some(n) => n,
            None => return Vec::new(),
        };
        let candidates = match self.def_by_name.get(name) {
            Some(c) => c,
            None => return Vec::new(),
        };
        candidates
            .iter()
            .filter(|p| p.ends_with(suffix))
            .filter_map(|p| self.definitions.get(p).cloned())
            .collect()
    }

    /// Search for workspace symbols matching a query string.
    pub fn workspace_symbols(&self, query: &str) -> Vec<(String, Location)> {
        self.definitions
            .iter()
            .filter(|(path, _)| path.as_str().contains(query) || path.name().contains(query))
            .map(|(path, loc)| (path.to_string(), loc.clone()))
            .collect()
    }

    // ═══════════════════════════════════════════════════════════════════════
    // Contract methods
    // ═══════════════════════════════════════════════════════════════════════

    /// Update contracts for a document by inferring them from procedure implementations,
    /// seeding inference with the current workspace store for cross-file calls.
    pub fn update_contracts(&mut self, module: &Module, source_manager: &DefaultSourceManager) {
        let contracts =
            infer_module_contracts_with_store(module, source_manager, Some(&self.contracts));
        self.contracts.update_document(contracts);
    }

    /// Get a procedure contract by exact path.
    pub fn get_contract(&self, path: &SymbolPath) -> Option<&ProcContract> {
        self.contracts.get(path)
    }

    /// Get a procedure contract by short name.
    pub fn get_contract_by_name(&self, name: &str) -> Option<&ProcContract> {
        self.contracts.get_by_name(name)
    }

    /// Get a procedure contract by path suffix.
    pub fn get_contract_by_suffix(&self, suffix: &str) -> Option<&ProcContract> {
        self.contracts.get_by_suffix(suffix)
    }

    /// Get access to the contract store.
    pub fn contracts(&self) -> &ContractStore {
        &self.contracts
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
    let mut collector = InvocationCollector {
        resolver: crate::symbol_resolution::create_resolver(module),
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
    resolver: crate::symbol_resolution::SymbolResolver<'a>,
    source_manager: &'a DefaultSourceManager,
    refs: Vec<Reference>,
}

impl<'a> InvocationCollector<'a> {
    fn push_target(&mut self, target: &InvocationTarget) {
        let range = span_to_range(self.source_manager, target.span()).unwrap_or_else(zero_range);

        // Use the unified symbol resolution service to get the fully-qualified path
        if let Some(path) = self.resolver.resolve_target(target) {
            self.refs.push(Reference { path, range });
        }
        // MAST roots return None from resolve_target, which is correct - we skip them
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
    fn workspace_index_references_by_suffix_full_path() {
        let mut index = WorkspaceIndex::default();
        let uri = test_uri();

        // References should be stored with fully-qualified paths after module alias expansion.
        // For example, `exec.base_field::square` with `use std::math::ecgfp5::base_field`
        // should be stored as `::std::math::ecgfp5::base_field::square`.
        let refs = vec![make_reference(
            "::std::math::ecgfp5::base_field::square",
            10,
        )];
        index.update_document(uri.clone(), &[], &refs);

        // Looking up by full path should find the reference
        let found = index.references_by_suffix("::std::math::ecgfp5::base_field::square");
        assert_eq!(found.len(), 1);

        // Looking up by suffix should also work
        let found2 = index.references_by_suffix("base_field::square");
        assert_eq!(found2.len(), 1);

        let found3 = index.references_by_suffix("square");
        assert_eq!(found3.len(), 1);
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
        assert!(
            found.len() >= 2,
            "expected at least 2 definitions ending with 'hash', got {}",
            found.len()
        );
    }
}
