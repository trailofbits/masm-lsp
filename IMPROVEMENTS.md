# LSP Implementation Improvements

This document outlines improvements to the masm-lsp codebase to enhance type safety, reduce string-based parsing, and improve testability.

---

## Progress

| # | Improvement | Status | Notes |
|---|-------------|--------|-------|
| 4 | Consolidate duplicated code | ✅ Done | `to_miden_uri` moved to util.rs |
| 2 | Typed symbol paths | ✅ Done | `SymbolPath` newtype created |
| 1 | Eliminate string fallbacks | ✅ Done | AST-based resolution improved |
| 7 | Consistent error handling | ✅ Done | `ResolutionError` type added |
| 6 | Compile-time instruction ref | ✅ Done | phf map generated at compile time |
| 5 | Parse-based module detection | ✅ Done | AST-based module kind detection |
| 8 | Service traits for testing | ✅ Done | DocumentService, WorkspaceService traits |
| 3 | Optimize workspace index | 🔲 Pending | Low priority until perf issue |

---

## 1. Eliminate String-Based Fallbacks ✅ COMPLETED

### Status: Done

### What Was Implemented

#### 1.1 AST-based invocation target lookup (src/resolution.rs)
- Added `find_invocation_at_offset()` function that traverses the AST using the `Visit` trait
- Added `InvocationFinder` struct that visits exec, call, syscall, and procref targets
- Added `resolve_invocation_target()` to resolve an invocation target to a `ResolvedSymbol`
- Added `position_to_offset()` to convert LSP positions to byte offsets

#### 1.2 Improved `resolve_symbol_at_position()`
- Now first tries AST-based invocation target lookup at the cursor position
- Falls back to token-based resolution only for non-invocation symbols (e.g., proc definitions)
- More robust than string scanning because it uses parsed AST structure

#### 1.3 Removed string-based fallbacks
- Removed `extract_path_hint()` from server.rs (character-scanning fallback)
- Removed `is_path_char()` helper from server.rs
- Removed `scan_invocations_for_unresolved()` from diagnostics.rs (regex-like line scanning)
- Removed `rewrite_syntax_errors()` from server.rs (string message matching)
- Simplified `goto_definition()` and `references()` to use only AST-based resolution

### Success Criteria Met
- ✅ No `line.find()`, `split_whitespace()`, or manual character scanning for parsing
- ✅ All symbol resolution for invocations goes through AST traversal
- ✅ Unresolved invocations detected via AST traversal in `InvocationCollector` (index.rs)

---

## 2. Introduce Typed Symbol Paths ✅ COMPLETED

### Status: Done

**Commit:** `1bc8840` - Add examples, SymbolPath type, and consolidate utilities

### What Was Implemented

- Created `src/symbol_path.rs` with `SymbolPath` newtype
- Methods: `new()`, `from_module_and_name()`, `as_str()`, `name()`, `module_path()`, `segments()`, `ends_with()`, `ends_with_path()`, `name_matches()`
- Implements `Display`, `From<String>`, `From<&str>`, `AsRef<str>`, `Hash`, `Eq`
- Unit tests for all methods
- Updated `Definition.path` and `Reference.path` to use `SymbolPath`
- Updated `ResolvedSymbol.path` to use `SymbolPath`
- Updated `WorkspaceIndex` to use `SymbolPath` keys

### Success Criteria Met
- ✅ No raw `String` used for symbol paths in public APIs
- ✅ Compile-time prevention of mixing paths with arbitrary strings
- ✅ Existing tests pass

---

## 3. Optimize WorkspaceIndex Lookups

### Problem
Suffix-based lookups (`definition_by_suffix`, `references_by_suffix`) perform O(n) linear scans.

### Affected Files
- `src/index.rs:53-157` (WorkspaceIndex)

### Implementation Steps

#### 3.1 Add suffix index (optional optimization)
```
Location: src/index.rs

struct WorkspaceIndex {
    definitions: HashMap<SymbolPath, Location>,
    def_by_uri: HashMap<Url, Vec<SymbolPath>>,

    // New: index by short name for fast lookup
    def_by_name: HashMap<String, Vec<SymbolPath>>,

    references: HashMap<SymbolPath, Vec<Location>>,
    refs_by_uri: HashMap<Url, Vec<SymbolPath>>,
}
```

#### 3.2 Update `update_document` to maintain index
```
When adding a definition with path "std::crypto::sha256::hash":
1. Add to definitions map
2. Add "hash" -> path mapping to def_by_name
3. Optionally add "sha256::hash" -> path for multi-segment lookups
```

#### 3.3 Rewrite suffix lookup
```
Before:
  pub fn definition_by_suffix(&self, suffix: &str) -> Option<Location> {
      self.definitions.iter().find_map(...)
  }

After:
  pub fn definition_by_suffix(&self, suffix: &str) -> Option<Location> {
      let name = suffix.rsplit("::").next()?;
      let candidates = self.def_by_name.get(name)?;
      candidates.iter()
          .find(|p| p.ends_with(suffix))
          .and_then(|p| self.definitions.get(p).cloned())
  }
```

### Success Criteria
- Lookup performance is O(1) average case for common queries
- Memory overhead is acceptable (benchmark with stdlib loaded)
- All existing tests pass

---

## 4. Consolidate Duplicated Code ✅ COMPLETED

### Status: Done

**Commit:** `1bc8840` - Add examples, SymbolPath type, and consolidate utilities

### What Was Implemented

- Moved `to_miden_uri()` to `src/util.rs`
- Removed duplicate definitions from `server.rs` and `diagnostics.rs`
- Replaced `build_item_path()` with `SymbolPath::from_module_and_name()`
- Removed duplicate `build_item_path` from `index.rs` and `resolution.rs`

### Success Criteria Met
- ✅ No function defined in more than one place
- ✅ Clear module responsibilities

---

## 5. Parse-Based Module Kind Detection ✅ COMPLETED

### Status: Done

### What Was Implemented

#### 5.1 AST-based module kind detection (src/server.rs)
- Added `determine_module_kind_from_ast()` function that uses `module.has_entrypoint()` to detect Executable modules
- Modules with `begin..end` blocks are detected as `ModuleKind::Executable`
- Modules without entrypoints are detected as `ModuleKind::Library`

#### 5.2 Improved parse_module flow (src/server.rs)
- Now tries `ModuleKind::Library` first (most permissive for many constructs)
- On successful parse, uses `determine_module_kind_from_ast()` to detect the correct kind
- If Library parsing fails, checks for `begin` in source and retries as `Executable`
- Uses `module.set_kind()` to update the module to the detected kind

#### 5.3 Removed text-based heuristics (src/util.rs)
- Removed `guess_module_kinds()` function entirely
- Removed `push_if_missing()` helper function
- Module kind is now determined from AST, not filename patterns or text scanning

#### 5.4 Helper function for retry (src/server.rs)
- Added `try_parse_with_kind()` helper for cleaner retry logic
- Handles the case where Library parsing fails with syntax errors for `begin` blocks

### Success Criteria Met
- ✅ Module kind determined from parsed AST using `has_entrypoint()`
- ✅ Removed filename-based kernel heuristics
- ✅ Removed text-based "begin" scanning for initial parse attempts
- ✅ Fallback to text check only when Library parse fails (for syntax error recovery)
- ✅ All existing tests pass

### Note on Kernel Detection
Kernel modules are not auto-detected since they require explicit namespace configuration during parsing. The `#kernel` namespace is a special case that must be specified in parse options.

---

## 6. Compile-Time Instruction Reference ✅ COMPLETED

### Status: Done

### What Was Implemented

#### 6.1 Created structured data file (data/instructions.toml)
- TOML format with ~150 instruction definitions
- Simple key-value pairs: `instruction_name = "description"`
- Supports both base instructions and variants (e.g., `add` and `"add.b"`)

#### 6.2 Build script generation (build.rs)
- Uses `phf_codegen` to generate a compile-time perfect hash map
- Parses `data/instructions.toml` at build time
- Generates `INSTRUCTION_MAP: phf::Map<&'static str, &'static str>`
- Automatically rebuilds when instruction data changes

#### 6.3 Updated inlay_hints.rs
- Added `include!(concat!(env!("OUT_DIR"), "/instruction_map.rs"))`
- `render_note()` now uses `INSTRUCTION_MAP.get()` for O(1) lookups
- Falls back to base instruction name if variant not found

#### 6.4 Removed runtime parsing
Deleted from `inlay_hints.rs`:
- `instruction_note_map()`
- `parse_instruction_reference()`
- `first_sentence()`
- `clean_instruction_name()`
- `clean_html()`

#### 6.5 Dependencies added to Cargo.toml
```toml
[build-dependencies]
phf_codegen = "0.11"
toml = "0.8"

[dependencies]
phf = "0.11"
```

### Success Criteria Met
- ✅ No Markdown parsing at runtime
- ✅ Startup time reduced (data compiled into binary)
- ✅ Data validated at compile time
- ✅ All tests pass

---

## 7. Consistent Error Handling ✅ COMPLETED

### Status: Done

### What Was Implemented

#### 7.1 ResolutionError type (src/resolution.rs)
- Created `ResolutionError` enum with variants:
  - `SourceNotFound(Url)` - when source file isn't loaded
  - `NoTokenAtPosition { line, column }` - when no token at cursor position
  - `InvalidPosition { line, column }` - when position is out of bounds
  - `SymbolResolution(LocalSymbolResolutionError)` - wrapper for upstream errors
  - `SymbolNotFound(String)` - when symbol can't be resolved

#### 7.2 Updated function signatures
- `resolve_symbol_at_position()` now returns `Result<ResolvedSymbol, ResolutionError>` (was `Result<Option<...>, ...>`)
- `resolve_invocation_target()` now returns `Result<ResolvedSymbol, ResolutionError>` (was `Result<Option<...>, ...>`)

#### 7.3 Error handling in handlers
- `goto_definition()` and `references()` now log resolution errors at debug level
- Errors are converted to appropriate LSP responses (None or error)

#### 7.4 Token extraction improvements
- Added `strip_instruction_prefix()` to handle `exec.f` → `f` token extraction
- Added `.` to identifier characters for proper instruction path extraction

### Success Criteria Met
- ✅ Resolution functions return `Result<T, ResolutionError>`
- ✅ Error causes are traceable via logs (debug level)
- ✅ All tests pass

---

### Original Problem
Mixed use of `Option<T>` and `Result<T, E>` obscures error causes.

### Original Affected Files
- `src/resolution.rs` (returns `Result<Option<...>, ...>`)
- `src/util.rs` (returns `Option`)
- `src/diagnostics.rs` (returns `Option`)

### Original Implementation Steps

#### 7.1 Define resolution error type
```
Location: src/resolution.rs (or new src/error.rs)

#[derive(Debug, thiserror::Error)]
pub enum ResolutionError {
    #[error("source file not found: {0}")]
    SourceNotFound(Url),

    #[error("no token at position {line}:{column}")]
    NoTokenAtPosition { line: u32, column: u32 },

    #[error("symbol resolution failed: {0}")]
    SymbolResolution(#[from] LocalSymbolResolutionError),

    #[error("symbol not found: {0}")]
    SymbolNotFound(String),
}
```

#### 7.2 Update function signatures
```
Before:
  pub fn extract_token_at_position(...) -> Option<String>
  pub fn resolve_symbol_at_position(...) -> Result<Option<ResolvedSymbol>, ...>

After:
  pub fn extract_token_at_position(...) -> Result<String, ResolutionError>
  pub fn resolve_symbol_at_position(...) -> Result<ResolvedSymbol, ResolutionError>
```

#### 7.3 Propagate errors with context
```
Location: src/server.rs

Before:
  let Some(token) = extract_token_at_position(&source, pos) else {
      return Ok(None);
  };

After:
  let token = extract_token_at_position(&source, pos)
      .map_err(|e| tracing::debug!("token extraction: {e}"))
      .ok()?;

Or propagate to caller for logging/diagnostics
```

#### 7.4 Add error logging
```
Location: src/server.rs

In goto_definition, references, etc.:
- Log resolution errors at debug level
- Convert to appropriate LSP response (None or error)
```

### Success Criteria
- All resolution functions return `Result<T, ResolutionError>`
- Error causes are traceable via logs
- No silent `None` returns without explanation

---

## 8. Extract Service Trait for Testing ✅ COMPLETED

### Status: Done

### What Was Implemented

#### 8.1 Created service traits (src/service.rs)
- `DocumentService` trait with:
  - `get_document_symbols(&self, uri: &Url) -> Option<DocumentSymbols>`
  - `resolve_at_position(&self, uri: &Url, position: Position) -> Result<ResolvedSymbol, ResolutionError>`
- `WorkspaceService` trait with:
  - `find_definition(&self, path: &SymbolPath) -> Option<Location>`
  - `find_references(&self, path: &SymbolPath) -> Vec<Location>`
  - `search_symbols(&self, query: &str) -> Vec<(String, Location)>`

#### 8.2 Implemented traits for Backend (src/server.rs)
- `DocumentService` implementation uses `get_or_parse_document()` and `resolve_symbol_at_position()`
- `WorkspaceService` placeholder implementation (LSP handlers use async patterns directly)
- Added `WorkspaceIndexWrapper` for synchronous workspace access via traits

#### 8.3 Created mock implementations (src/service.rs::mocks)
- `MockDocumentService` with builder pattern:
  - `with_symbols(uri, symbols)` - add document symbols
  - `with_resolution(uri, line, column, result)` - add resolution results
- `MockWorkspaceService` with builder pattern:
  - `with_definition(path, location)` - add symbol definitions
  - `with_references(path, locations)` - add symbol references
  - Supports exact match, suffix match, and name match lookups

#### 8.4 Extracted core logic into testable functions
- `handle_goto_definition<D, W>()` - core goto definition logic
- `handle_find_references<D, W>()` - core find references logic
- `handle_workspace_symbols<W>()` - core workspace symbol search

#### 8.5 Added comprehensive unit tests
- `test_goto_definition_finds_exact_match` - exact path lookup
- `test_goto_definition_returns_none_when_symbol_not_found` - error handling
- `test_find_references_returns_all_references` - basic reference lookup
- `test_find_references_includes_declaration` - include_declaration flag
- `test_find_references_returns_none_when_empty` - empty results handling
- `test_workspace_symbols_filters_by_query` - symbol search filtering
- `test_mock_workspace_finds_by_suffix` - suffix and name matching

### Success Criteria Met
- ✅ Core logic testable without LSP transport
- ✅ Mock implementations available for unit tests
- ✅ Integration tests still use full Backend
- ✅ All existing tests pass (29 tests total)

---

## Implementation Priority

| Priority | Improvement | Effort | Impact | Status |
|----------|-------------|--------|--------|--------|
| 1 | Consolidate duplicated code (#4) | Low | Medium | ✅ Done |
| 2 | Typed symbol paths (#2) | Medium | High | ✅ Done |
| 3 | Eliminate string fallbacks (#1) | Medium | High | ✅ Done |
| 4 | Consistent error handling (#7) | Medium | Medium | ✅ Done |
| 5 | Compile-time instruction ref (#6) | Low | Low | ✅ Done |
| 6 | Parse-based module detection (#5) | Medium | Medium | ✅ Done |
| 7 | Service traits for testing (#8) | Medium | Medium | ✅ Done |
| 8 | Optimize workspace index (#3) | Low | Low | 🔲 **Next** |

### Recommended Next Step: #3 - Optimize Workspace Index

**Rationale:**
- Low effort task that improves lookup performance
- Currently O(n) suffix lookups could be optimized to O(1) average case
- Only necessary if performance issues are observed with large codebases
- All other improvements have been completed

---

## Phase 2: Structural Improvements (New)

These improvements focus on reducing complexity in `server.rs` and improving robustness.

### 9. Replace Panics with Result Types

**Status:** ✅ Done
**Effort:** Small
**Risk:** Low

Replace `unwrap_or_else(|| panic!(...))` patterns with proper error handling.

**Locations to fix:**
- `src/resolution.rs:79-81` in `resolve_symbol_at_position`:
  ```rust
  // Current:
  let item = module.get(span.into_inner()).unwrap_or_else(|| {
      panic!("invalid item index {:?}", span.into_inner());
  });

  // Proposed:
  let item = module
      .get(span.into_inner())
      .ok_or_else(|| ResolutionError::InvalidItemIndex(span.into_inner()))?;
  ```

- `src/resolution.rs:246-248` in `resolve_invocation_target` (same pattern)

**New error variant:**
```rust
#[error("invalid item index: {0:?}")]
InvalidItemIndex(miden_assembly_syntax::ast::ItemIndex),
```

---

### 10. Add ServerConfig Builder Pattern

**Status:** ✅ Done
**Effort:** Small
**Risk:** Low

**Current usage (src/lib.rs):**
```rust
let mut cfg = ServerConfig::default();
cfg.inlay_hint_tabs = 2;
cfg.library_paths = paths;
```

**Proposed:**
```rust
let cfg = ServerConfig::builder()
    .inlay_hint_tabs(2)
    .library_paths(paths)
    .instruction_hovers_enabled(true)
    .build();
```

**Implementation in src/lib.rs:**
```rust
#[derive(Default)]
pub struct ServerConfigBuilder {
    inlay_hint_tabs: Option<usize>,
    library_paths: Option<Vec<LibraryPath>>,
    instruction_hovers_enabled: Option<bool>,
}

impl ServerConfigBuilder {
    pub fn inlay_hint_tabs(mut self, tabs: usize) -> Self {
        self.inlay_hint_tabs = Some(tabs);
        self
    }
    // ... other methods ...

    pub fn build(self) -> ServerConfig {
        ServerConfig {
            inlay_hint_tabs: self.inlay_hint_tabs.unwrap_or(2),
            library_paths: self.library_paths.unwrap_or_else(default_library_paths),
            instruction_hovers_enabled: self.instruction_hovers_enabled.unwrap_or(false),
        }
    }
}

impl ServerConfig {
    pub fn builder() -> ServerConfigBuilder {
        ServerConfigBuilder::default()
    }
}
```

---

### 11. Extract Module Path Resolution

**Status:** ✅ Done
**Effort:** Medium
**Risk:** Low

Extract module path resolution logic from `server.rs:285-297` and related functions into a dedicated module.

**Files affected:**
- `src/server.rs` → new `src/module_path.rs`

**Functions to extract:**
- `module_path_from_uri()` (async method on Backend)
- `build_path_from_root()` (server.rs:846-870)
- `module_path_from_uri_fallback()` (server.rs:835-844)

**Proposed new module `src/module_path.rs`:**
```rust
//! Module path resolution from file URIs.

pub struct ModulePathResolver<'a> {
    library_paths: &'a [LibraryPath],
}

impl<'a> ModulePathResolver<'a> {
    pub fn new(library_paths: &'a [LibraryPath]) -> Self {
        Self { library_paths }
    }

    pub fn resolve(&self, uri: &Url) -> Option<miden_assembly_syntax::ast::PathBuf> {
        // ... implementation ...
    }
}
```

**Benefits:**
- Clearer responsibility boundaries
- Easier to test path resolution in isolation
- Reduces `server.rs` complexity by ~50 lines

---

### 12. Extract DocumentCache from Backend

**Status:** 🔲 Pending
**Effort:** Medium
**Risk:** Medium

Group document-related state into a dedicated type.

**Current Backend structure (src/server.rs:45-52):**
```rust
pub struct Backend<C> {
    client: C,
    sources: Arc<DefaultSourceManager>,
    documents: Arc<RwLock<HashMap<Url, DocumentState>>>,  // Group
    symbols: Arc<RwLock<HashMap<Url, DocumentSymbols>>>,  // these
    workspace: Arc<RwLock<WorkspaceIndex>>,
    config: Arc<RwLock<ServerConfig>>,
}
```

**Proposed:**
```rust
#[derive(Clone, Default)]
pub struct DocumentCache {
    state: Arc<RwLock<HashMap<Url, DocumentState>>>,
    symbols: Arc<RwLock<HashMap<Url, DocumentSymbols>>>,
}

impl DocumentCache {
    pub async fn get_state(&self, uri: &Url) -> Option<DocumentState> { ... }
    pub async fn set_state(&self, uri: Url, state: DocumentState) { ... }
    pub async fn remove(&self, uri: &Url) { ... }
    pub async fn get_symbols(&self, uri: &Url) -> Option<DocumentSymbols> { ... }
    pub async fn set_symbols(&self, uri: Url, symbols: DocumentSymbols) { ... }
}

pub struct Backend<C> {
    client: C,
    sources: Arc<DefaultSourceManager>,
    documents: DocumentCache,  // Consolidated
    workspace: Arc<RwLock<WorkspaceIndex>>,
    config: Arc<RwLock<ServerConfig>>,
}
```

**Benefits:**
- Encapsulates document management logic
- Reduces lock coordination complexity
- Enables potential batched updates in future

---

### 13. Split server.rs into Focused Modules

**Status:** 🔲 Pending
**Effort:** Large
**Risk:** Medium
**Dependencies:** #11, #12

Split `server.rs` (1500+ lines) into focused modules.

**Proposed structure:**
```
src/
├── server/
│   ├── mod.rs           # Re-exports and Backend struct definition
│   ├── handlers.rs      # LSP method implementations (goto_definition, hover, etc.)
│   ├── lifecycle.rs     # did_open, did_change, did_close handlers
│   ├── parsing.rs       # parse_module, parse_and_index, module kind detection
│   └── config.rs        # Configuration extraction from JSON settings
└── server.rs            # (deleted, replaced by server/mod.rs)
```

**Migration approach:**
1. Create `src/server/` directory
2. Move helper functions first (low risk): `extract_tab_count`, `extract_library_paths`, `determine_module_kind_from_ast`, `is_on_use_statement`, `extract_procedure_signature`, `extract_doc_comment`
3. Move lifecycle handlers: `handle_open`, `handle_change`, `handle_close`, `set_document_version`, `publish_diagnostics`
4. Move parsing logic: `parse_module`, `try_parse_with_kind`, `parse_and_index`, `get_or_parse_document`
5. Move LSP handlers: `goto_definition`, `hover`, `references`, `symbol`, `inlay_hint`
6. Update imports in `lib.rs`

---

## Phase 3: Enhanced Testing (New)

### 14. Add Property-Based Tests for SymbolPath

**Status:** ✅ Done
**Effort:** Small
**Risk:** None (additive)

Add `proptest` tests for `SymbolPath` parsing and manipulation.

**Add to Cargo.toml:**
```toml
[dev-dependencies]
proptest = "1.4"
```

**Add to src/symbol_path.rs:**
```rust
#[cfg(test)]
mod proptests {
    use proptest::prelude::*;
    use super::SymbolPath;

    proptest! {
        #[test]
        fn name_is_last_segment(path in "[a-z]+(::[a-z]+)*") {
            let sp = SymbolPath::new(&format!("::{}", path));
            let expected_name = path.rsplit("::").next().unwrap();
            prop_assert_eq!(sp.name(), expected_name);
        }

        #[test]
        fn roundtrip_display(path in "::[a-z]+(::[a-z]+)*") {
            let sp = SymbolPath::new(&path);
            prop_assert_eq!(sp.to_string(), path);
        }

        #[test]
        fn ends_with_self(path in "::[a-z]+(::[a-z]+)*") {
            let sp = SymbolPath::new(&path);
            prop_assert!(sp.ends_with(&path));
        }
    }
}
```

---

### 15. Add Concurrent Access Tests

**Status:** 🔲 Pending
**Effort:** Medium
**Risk:** None (additive)

Test that concurrent document updates don't cause data races or deadlocks.

**New file: tests/concurrent_test.rs**
```rust
use masm_lsp::server::Backend;
use tokio::task::JoinSet;
use tower_lsp::lsp_types::Url;

mod common;
use common::harness::RecordingClient;

#[tokio::test]
async fn concurrent_document_updates_no_panic() {
    let client = RecordingClient::new();
    let backend = std::sync::Arc::new(Backend::new(client));

    let mut set = JoinSet::new();

    for i in 0..10 {
        let backend = backend.clone();
        set.spawn(async move {
            let uri = Url::parse(&format!("file:///tmp/doc{}.masm", i)).unwrap();
            for v in 1..=50 {
                // Rapid open/change cycles
                let text = format!("proc f{}\n  nop\nend\n", v);
                // ... simulate operations ...
            }
        });
    }

    while let Some(result) = set.join_next().await {
        result.expect("task should not panic");
    }
}
```

---

## Updated Implementation Priority

| Priority | Improvement | Effort | Impact | Status |
|----------|-------------|--------|--------|--------|
| 1-7 | (Previous improvements) | - | - | ✅ Done |
| 8 | Optimize workspace index (#3) | Low | Low | 🔲 Pending |
| 9 | Replace panics with Result (#9) | Small | Medium | ✅ Done |
| 10 | ServerConfig builder (#10) | Small | Low | ✅ Done |
| 11 | Extract module path resolution (#11) | Medium | Medium | ✅ Done |
| 12 | Extract DocumentCache (#12) | Medium | Medium | 🔲 Pending |
| 13 | Split server.rs (#13) | Large | High | 🔲 Pending |
| 14 | Property tests for SymbolPath (#14) | Small | Low | ✅ Done |
| 15 | Concurrent access tests (#15) | Medium | Medium | 🔲 Pending |

### Recommended Implementation Order

**Quick wins (1-2 hours each):**
1. #9 - Replace panics with Result (improves robustness)
2. #10 - ServerConfig builder (improves ergonomics)
3. #14 - Property tests (improves test coverage)

**Moderate refactoring (half day each):**
4. #11 - Extract module path resolution
5. #12 - Extract DocumentCache
6. #15 - Concurrent access tests

**Large refactoring (1-2 days):**
7. #13 - Split server.rs (depends on #11, #12)
