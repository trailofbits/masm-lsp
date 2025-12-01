# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an LSP (Language Server Protocol) server for Miden Assembly (MASM), a stack-based language for the Miden VM zero-knowledge virtual machine. The server provides IDE features like go-to-definition, hover, references, diagnostics, and decompilation hints.

## Build and Test Commands

```bash
cargo build              # Build the project
cargo test               # Run all tests
cargo test <test_name>   # Run a single test
cargo check              # Fast type checking
cargo bench              # Run benchmarks (decompilation)
```

## Architecture

### Layer Structure

1. **LSP Transport** (`server/mod.rs`): Implements `tower_lsp::LanguageServer` trait, handles JSON-RPC communication
2. **Service Layer** (`service.rs`): Abstracts document/workspace operations via traits (`DocumentService`, `WorkspaceService`) for testability
3. **Index** (`index.rs`): `WorkspaceIndex` maintains definitions, references, and contracts across all files; `DocumentSymbols` holds per-file parsed data
4. **Analysis** (`analysis/`): Static analysis subsystem with taint tracking, stack simulation, and contract inference

### Key Modules

- **`symbol_resolution.rs`**: Resolves symbol names to qualified `SymbolPath` using module context (imports, aliases)
- **`cursor_resolution.rs`**: Finds what symbol the cursor is on via AST traversal, returns `ResolvedSymbol`
- **`decompiler/`**: Converts instructions to pseudocode by tracking a symbolic stack with named variables
  - `ToPseudocode` trait on `Instruction` for conversion
  - `DecompilerState` tracks symbolic stack state
  - `collector.rs` visits AST to generate hints for all procedures

### Analysis Subsystem (`analysis/`)

- **Contract Inference** (`contracts/`): Infers procedure stack effects and validation behavior using topological ordering of call graph
- **Taint Tracking** (`types/taint.rs`): Tracks untrusted values (advice stack, Merkle store) through execution
- **Checkers** (`checkers/`): Individual security checks (u32 validation, merkle depth, procedure calls)
- **Abstract Interpretation** (`abstract_interpretation.rs`): Framework for loop analysis and decompilation

### Symbol Resolution Flow

1. `cursor_resolution::resolve_symbol_at_position` finds the symbol under cursor
2. `symbol_resolution::SymbolResolver::resolve_target` converts to qualified path using module imports
3. `WorkspaceIndex::find_definition` looks up the definition location

### Inlay Hints

Two modes controlled by `InlayHintType`:

- **Decompilation**: Shows pseudocode like `v_0 = a_0 + a_1` (default)
- **Description**: Shows instruction descriptions via `ToDescription` trait

---

## Reusable Traits and Abstractions

**IMPORTANT**: Before implementing new functionality, check if an existing trait or abstraction can be reused. Do NOT reimplement functionality that already exists.

### Core Traits (Must Reuse)

| Trait | Location | Purpose | When to Use |
|-------|----------|---------|-------------|
| `ToPseudocode` | `decompiler/pseudocode.rs` | Converts instructions to pseudocode | Any pseudocode/decompilation output |
| `ToDescription` | `descriptions.rs` | Dynamic instruction descriptions | Hover info, documentation |
| `StackLike` | `analysis/stack_ops.rs` | Unified stack manipulation | Any stack-based analysis |
| `Checker` | `analysis/checker.rs` | Security check abstraction | New security/validation checks |
| `DocumentService` | `service.rs` | Document operations | Testing, document handling |
| `WorkspaceService` | `service.rs` | Workspace operations | Testing, workspace queries |
| `PublishDiagnostics` | `client.rs` | Diagnostic publishing | Testing with mock clients |

### Key Utilities (Must Reuse)

| Utility | Location | Purpose |
|---------|----------|---------|
| `symbol_resolution::resolve_target()` | `symbol_resolution.rs` | **Single source of truth** for symbol resolution |
| `stack_ops::static_effect()` | `analysis/stack_ops.rs` | O(1) instruction effect lookup (pops, pushes) |
| `diagnostics::span_to_range()` | `diagnostics.rs` | Convert byte spans to LSP ranges |
| `diagnostics::normalize_message()` | `diagnostics.rs` | Standardize diagnostic messages |
| `SymbolPath` | `symbol_path.rs` | Qualified symbol paths (e.g., `::std::math::u64::add`) |

### Adding New Checkers

New security checks should implement the `Checker` trait:

```rust
impl Checker for MyChecker {
    fn check(&self, instruction: &Instruction, state: &AnalysisState, ctx: &CheckerContext) -> Vec<AnalysisFinding> {
        // Return findings with severity, message, and span
    }
}
```

Register new checkers in `analysis/analyzer.rs`.

### Adding New Stack Operations

If you need stack manipulation, implement `StackLike` or use existing implementations (`DecompilerState`, `AbstractState`). Do NOT write custom stack logic.

---

## Testing Infrastructure

### Test Harness (`tests/common/`)

**Always use the existing test infrastructure** for integration tests:

```rust
use crate::common::harness::TestHarness;
use crate::common::fixtures;

#[tokio::test]
async fn test_my_feature() {
    let harness = TestHarness::new().await;
    harness.open_doc("file.masm", fixtures::SIMPLE_EXECUTABLE).await;

    // Use harness methods for LSP operations
    let result = harness.goto_definition("file.masm", line, col).await;
    assert!(result.is_some());
}
```

### Key Test Utilities

| Utility | Purpose |
|---------|---------|
| `TestHarness::new()` | Create test environment with mock client |
| `TestHarness::with_instruction_hovers()` | Enable instruction hovers for hover tests |
| `harness.open_doc(name, content)` | Open inline test document |
| `harness.open_fixture(name)` | Open file from `tests/fixtures/` |
| `harness.take_published()` | Get published diagnostics |
| `fixtures::find_position(content, marker)` | Find cursor position in test code |

### Test Fixtures (`tests/common/fixtures.rs`)

Common MASM code snippets for testing:
- `SIMPLE_EXECUTABLE`: Basic executable with procedure calls
- `WITH_PROCREF`: Tests `procref` instruction
- `MULTI_CALL`: Multiple procedure calls

Add new reusable fixtures here, do NOT duplicate test code inline.

### Mock Services (`service.rs`)

For unit testing without full LSP:

```rust
let mock_doc = MockDocumentService::new();
let mock_ws = MockWorkspaceService::new();
// Test service-layer logic in isolation
```

### Test Organization

| Test File | Coverage |
|-----------|----------|
| `resolution_test.rs` | Symbol resolution (local, global, stdlib) |
| `hover_test.rs` | Hover information |
| `diagnostics_test.rs` | Error reporting |
| `concurrent_test.rs` | Race conditions, stress tests |
| `stdlib_test.rs` | Standard library integration |
| `examples_test.rs` | Real-world examples |

---

## Code Conventions

- Trait-based conversion: `ToPseudocode` for decompilation, `ToDescription` for descriptions
- Use `SymbolPath` for qualified procedure paths (e.g., `std::math::u64::add`)
- Span-to-range conversion goes through `diagnostics::span_to_range`
- AST traversal uses `miden_assembly_syntax::ast::visit::Visit` trait

### Configuration

Use the builder pattern for `ServerConfig`:

```rust
ServerConfig::builder()
    .inlay_hint_type(InlayHintType::Decompilation)
    .taint_analysis_enabled(true)
    .build()
```

### Error Handling

- Use `thiserror` for custom error types
- Prefer `Result` over panics
- Attach diagnostic context for user-facing errors

### Diagnostic Sources

Three diagnostic sources (use consistently):
- `masm-lsp/syntax`: Parse errors, unresolved references
- `masm-lsp/analysis`: Taint analysis, uninitialized locals
- `masm-lsp/decompilation`: Decompilation failures

---

## Anti-Patterns to Avoid

1. **Do NOT** reimplement symbol resolution—use `symbol_resolution::resolve_target()`
2. **Do NOT** write custom stack manipulation—implement or use `StackLike`
3. **Do NOT** duplicate test fixtures—add to `tests/common/fixtures.rs`
4. **Do NOT** add LSP handlers without service-layer tests
5. **Do NOT** bypass the index for definition/reference lookups
6. **Do NOT** hardcode instruction effects—use `stack_ops::static_effect()`
7. **Do NOT** create new diagnostic sources without documenting them

---

## Module Boundaries

### What Goes Where

| New Feature | Location |
|-------------|----------|
| LSP protocol handlers | `server/mod.rs` |
| Business logic | `service.rs` (via traits) |
| Symbol lookups | `index.rs` → `WorkspaceIndex` |
| New security checks | `analysis/checkers/` + register in `analyzer.rs` |
| New instruction descriptions | `descriptions.rs` → `ToDescription` |
| Pseudocode generation | `decompiler/pseudocode.rs` → `ToPseudocode` |
| Stack effect metadata | `analysis/stack_ops.rs` |

### Dependency Direction

```
server/ → service.rs → index.rs → symbol_resolution.rs
                    ↘             ↘
                 analysis/    cursor_resolution.rs
```

Analysis modules depend on `types/` and `checker.rs`, not vice versa.

---

## External Dependencies

- `miden-assembly-syntax`: Parser and AST types from Miden VM
- `miden-debug-types`: Source management (`DefaultSourceManager`, `SourceSpan`)
- `tower-lsp`: LSP protocol implementation
- `tokio`: Async runtime
- `tracing`: Logging (use `debug!`, `info!`, `error!`)
