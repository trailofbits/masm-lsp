use std::collections::{BTreeMap, HashMap, HashSet};
use std::path::PathBuf;

use masm_decompiler::frontend::Program;
use tower_lsp::lsp_types::Url;

use crate::index::DocumentSymbols;
use crate::SymbolPath;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ProgramOrigin {
    OpenDocument,
    DiskTracked,
}

#[derive(Debug, Clone)]
pub(crate) struct TrackedProgram {
    pub(crate) uri: Url,
    pub(crate) source_path: PathBuf,
    pub(crate) symbols: DocumentSymbols,
    pub(crate) origin: ProgramOrigin,
}

impl TrackedProgram {
    pub(crate) fn new(
        uri: Url,
        source_path: PathBuf,
        symbols: DocumentSymbols,
        origin: ProgramOrigin,
    ) -> Self {
        Self {
            uri,
            source_path,
            symbols,
            origin,
        }
    }

    pub(crate) fn module_path(&self) -> SymbolPath {
        SymbolPath::new(self.symbols.module.path().to_string())
    }

    pub(crate) fn to_program(&self) -> Program {
        Program::from_parts(
            Box::new(self.symbols.module.as_ref().clone()),
            self.source_path.clone(),
            self.symbols.module.path().to_path_buf(),
        )
    }
}

#[derive(Debug, Clone, Default)]
struct TrackedEntry {
    is_open: bool,
    open: Option<TrackedProgram>,
    open_priority: u64,
    disk: Option<TrackedProgram>,
    disk_priority: u64,
}

impl TrackedEntry {
    fn candidate(&self) -> Option<&TrackedProgram> {
        if self.is_open {
            self.open.as_ref().or(self.disk.as_ref())
        } else {
            self.disk.as_ref()
        }
    }

    fn relevant_module_paths(&self) -> HashSet<SymbolPath> {
        let mut paths = HashSet::new();
        if let Some(program) = &self.open {
            paths.insert(program.module_path());
        }
        if let Some(program) = &self.disk {
            paths.insert(program.module_path());
        }
        paths
    }

    fn is_empty(&self) -> bool {
        !self.is_open && self.open.is_none() && self.disk.is_none()
    }
}

#[derive(Debug, Clone, Default)]
pub(crate) struct TrackedWorkspace {
    entries: HashMap<Url, TrackedEntry>,
    tracked_disk_uris: HashSet<Url>,
    next_open_priority: u64,
}

impl TrackedWorkspace {
    pub(crate) fn tracked_disk_uris(&self) -> HashSet<Url> {
        self.tracked_disk_uris.clone()
    }

    pub(crate) fn upsert_open_program(&mut self, program: TrackedProgram) -> HashSet<Url> {
        let uri = program.uri.clone();
        let mut relevant = self.relevant_module_paths_for_uri(&uri);
        relevant.insert(program.module_path());
        let before = self.active_winner_uris_for_modules(&relevant);

        let entry = self.entries.entry(uri.clone()).or_default();
        if entry.open.is_none() {
            entry.open_priority = self.next_open_priority;
            self.next_open_priority = self.next_open_priority.saturating_add(1);
        }
        entry.is_open = true;
        entry.open = Some(program);

        relevant.extend(self.relevant_module_paths_for_uri(&uri));
        self.changed_active_uris(relevant, before)
    }

    pub(crate) fn mark_open_parse_failed(&mut self, uri: &Url) -> HashSet<Url> {
        let relevant = self.relevant_module_paths_for_uri(uri);
        let before = self.active_winner_uris_for_modules(&relevant);

        let entry = self.entries.entry(uri.clone()).or_default();
        if entry.open.is_none() && entry.open_priority == 0 && self.next_open_priority == 0 {
            // keep zero as the first assigned priority; no-op branch documents intent
        }
        entry.is_open = true;

        self.changed_active_uris(relevant, before)
    }

    pub(crate) fn close_open_document(&mut self, uri: &Url) -> HashSet<Url> {
        let relevant = self.relevant_module_paths_for_uri(uri);
        let before = self.active_winner_uris_for_modules(&relevant);

        if let Some(entry) = self.entries.get_mut(uri) {
            entry.is_open = false;
            entry.open = None;
        }
        self.prune_if_empty(uri);

        self.changed_active_uris(relevant, before)
    }

    pub(crate) fn upsert_disk_program(
        &mut self,
        program: TrackedProgram,
        disk_priority: u64,
    ) -> HashSet<Url> {
        let uri = program.uri.clone();
        let mut relevant = self.relevant_module_paths_for_uri(&uri);
        relevant.insert(program.module_path());
        let before = self.active_winner_uris_for_modules(&relevant);

        let entry = self.entries.entry(uri.clone()).or_default();
        entry.disk = Some(program);
        entry.disk_priority = disk_priority;
        self.tracked_disk_uris.insert(uri.clone());

        relevant.extend(self.relevant_module_paths_for_uri(&uri));
        self.changed_active_uris(relevant, before)
    }

    pub(crate) fn remove_tracked_disk_program(&mut self, uri: &Url) -> HashSet<Url> {
        let relevant = self.relevant_module_paths_for_uri(uri);
        let before = self.active_winner_uris_for_modules(&relevant);

        if let Some(entry) = self.entries.get_mut(uri) {
            entry.disk = None;
            entry.disk_priority = 0;
        }
        self.tracked_disk_uris.remove(uri);
        self.prune_if_empty(uri);

        self.changed_active_uris(relevant, before)
    }

    pub(crate) fn active_program_for_uri(&self, uri: &Url) -> Option<TrackedProgram> {
        let candidate = self.entries.get(uri)?.candidate()?.clone();
        (self
            .active_uri_for_module(&candidate.module_path())
            .as_ref()
            == Some(uri))
        .then_some(candidate)
    }

    pub(crate) fn candidate_program_for_uri(&self, uri: &Url) -> Option<TrackedProgram> {
        self.entries.get(uri)?.candidate().cloned()
    }

    pub(crate) fn active_programs(&self) -> Vec<TrackedProgram> {
        let mut winners: BTreeMap<String, (ProgramPriority, TrackedProgram)> = BTreeMap::new();

        for (uri, entry) in &self.entries {
            let Some(candidate) = entry.candidate() else {
                continue;
            };
            let module_path = candidate.module_path();
            let key = module_path.as_str().to_string();
            let priority = self.priority_for_entry(entry, uri, candidate.origin);

            match winners.get(&key) {
                Some((existing, _)) if *existing <= priority => {}
                _ => {
                    winners.insert(key, (priority, candidate.clone()));
                }
            }
        }

        let mut programs: Vec<_> = winners.into_values().collect();
        programs.sort_by(|(left, _), (right, _)| left.cmp(right));
        programs.into_iter().map(|(_, program)| program).collect()
    }

    fn changed_active_uris(
        &self,
        relevant_modules: HashSet<SymbolPath>,
        before: HashSet<Url>,
    ) -> HashSet<Url> {
        let mut changed = before;
        changed.extend(self.active_winner_uris_for_modules(&relevant_modules));
        changed
    }

    fn relevant_module_paths_for_uri(&self, uri: &Url) -> HashSet<SymbolPath> {
        self.entries
            .get(uri)
            .map(TrackedEntry::relevant_module_paths)
            .unwrap_or_default()
    }

    fn active_winner_uris_for_modules(&self, modules: &HashSet<SymbolPath>) -> HashSet<Url> {
        modules
            .iter()
            .filter_map(|module| self.active_uri_for_module(module))
            .collect()
    }

    fn active_uri_for_module(&self, module_path: &SymbolPath) -> Option<Url> {
        let mut best: Option<(ProgramPriority, Url)> = None;

        for (uri, entry) in &self.entries {
            let Some(candidate) = entry.candidate() else {
                continue;
            };
            if candidate.module_path() != *module_path {
                continue;
            }

            let priority = self.priority_for_entry(entry, uri, candidate.origin);
            match &best {
                Some((existing, _)) if *existing <= priority => {}
                _ => best = Some((priority, uri.clone())),
            }
        }

        best.map(|(_, uri)| uri)
    }

    fn priority_for_entry(
        &self,
        entry: &TrackedEntry,
        uri: &Url,
        origin: ProgramOrigin,
    ) -> ProgramPriority {
        match origin {
            ProgramOrigin::OpenDocument => ProgramPriority {
                origin_rank: 0,
                origin_priority: entry.open_priority,
                uri: uri.as_str().to_string(),
            },
            ProgramOrigin::DiskTracked => ProgramPriority {
                origin_rank: 1,
                origin_priority: entry.disk_priority,
                uri: uri.as_str().to_string(),
            },
        }
    }

    fn prune_if_empty(&mut self, uri: &Url) {
        let should_remove = self.entries.get(uri).is_some_and(TrackedEntry::is_empty);
        if should_remove {
            self.entries.remove(uri);
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct ProgramPriority {
    origin_rank: u8,
    origin_priority: u64,
    uri: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::sync::Arc;

    use miden_debug_types::DefaultSourceManager;

    use crate::index::build_document_symbols;

    fn tracked_program(
        uri: &str,
        module_name: &str,
        proc_name: &str,
        origin: ProgramOrigin,
    ) -> TrackedProgram {
        let uri = Url::parse(uri).expect("valid uri");
        let sources = Arc::new(DefaultSourceManager::default());
        let source = format!("proc {proc_name}\n  nop\nend\n");
        let mut parser = miden_assembly_syntax::ModuleParser::new(
            miden_assembly_syntax::ast::ModuleKind::Library,
        );
        let module = parser
            .parse_str(
                miden_assembly_syntax::ast::Path::new(module_name),
                source,
                sources.clone(),
            )
            .expect("parse module");
        let symbols = build_document_symbols(module, sources);
        TrackedProgram::new(
            uri,
            PathBuf::from(format!("/tmp/{module_name}.masm")),
            symbols,
            origin,
        )
    }

    #[test]
    fn open_document_overrides_disk_program_same_uri() {
        let mut workspace = TrackedWorkspace::default();
        let uri = "file:///tmp/utils.masm";
        let disk = tracked_program(uri, "utils", "disk_proc", ProgramOrigin::DiskTracked);
        let open = tracked_program(uri, "utils", "open_proc", ProgramOrigin::OpenDocument);

        workspace.upsert_disk_program(disk, 0);
        workspace.upsert_open_program(open.clone());

        let active = workspace
            .active_program_for_uri(&Url::parse(uri).expect("valid uri"))
            .expect("active program");
        assert_eq!(active.origin, ProgramOrigin::OpenDocument);
        assert_eq!(
            active
                .symbols
                .module
                .procedures()
                .next()
                .unwrap()
                .name()
                .as_str(),
            "open_proc"
        );
    }

    #[test]
    fn close_document_restores_disk_program_when_fallback_exists() {
        let mut workspace = TrackedWorkspace::default();
        let uri = Url::parse("file:///tmp/utils.masm").expect("valid uri");
        workspace.upsert_disk_program(
            tracked_program(
                uri.as_str(),
                "utils",
                "disk_proc",
                ProgramOrigin::DiskTracked,
            ),
            0,
        );
        workspace.upsert_open_program(tracked_program(
            uri.as_str(),
            "utils",
            "open_proc",
            ProgramOrigin::OpenDocument,
        ));

        workspace.close_open_document(&uri);

        let active = workspace
            .active_program_for_uri(&uri)
            .expect("active program");
        assert_eq!(active.origin, ProgramOrigin::DiskTracked);
        assert_eq!(
            active
                .symbols
                .module
                .procedures()
                .next()
                .unwrap()
                .name()
                .as_str(),
            "disk_proc"
        );
    }

    #[test]
    fn parse_error_without_last_known_good_keeps_no_active_open_program() {
        let mut workspace = TrackedWorkspace::default();
        let uri = Url::parse("file:///tmp/new.masm").expect("valid uri");

        workspace.mark_open_parse_failed(&uri);

        assert!(workspace.active_program_for_uri(&uri).is_none());
        assert!(workspace.candidate_program_for_uri(&uri).is_none());
    }

    #[test]
    fn parse_error_uses_disk_candidate_when_no_open_program_exists() {
        let mut workspace = TrackedWorkspace::default();
        let uri = Url::parse("file:///tmp/utils.masm").expect("valid uri");
        workspace.upsert_disk_program(
            tracked_program(
                uri.as_str(),
                "utils",
                "disk_proc",
                ProgramOrigin::DiskTracked,
            ),
            0,
        );

        workspace.mark_open_parse_failed(&uri);

        let active = workspace
            .active_program_for_uri(&uri)
            .expect("active program");
        assert_eq!(active.origin, ProgramOrigin::DiskTracked);
    }
}
