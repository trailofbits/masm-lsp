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

## Code Conventions

- Trait-based conversion: `ToPseudocode` for decompilation, `ToDescription` for descriptions
- Use `SymbolPath` for qualified procedure paths (e.g., `std::math::u64::add`)
- Span-to-range conversion goes through `diagnostics::span_to_range`
- AST traversal uses `miden_assembly_syntax::ast::visit::Visit` trait

## External Dependencies

- `miden-assembly-syntax`: Parser and AST types from Miden VM
- `miden-debug-types`: Source management (`DefaultSourceManager`, `SourceSpan`)
- `tower-lsp`: LSP protocol implementation
