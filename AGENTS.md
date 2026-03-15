# MASM LSP Server

## Project Overview

This is an LSP (Language Server Protocol) server for Miden Assembly (MASM), a
stack-based language for the Miden VM zero-knowledge virtual machine. The server
provides IDE features like go-to-definition, hover, references, diagnostics,
instruction descriptions, and decompilation hints.

The LSP depends on Miden crates from the `0xMiden/miden-vm` repository for
underlying language definitions, and the MASM decompiler from the
`trailofbits/masm-decompiler` repository. The only downstream client is the
`vscode-masm` LSP client extension for VSCode, which is implemented in the
`trailofbits/vscode-masm` repository.

## High-level Guidelines

- Do not write code before stating assumptions.
- Ask for clarifications if the given task is under-specified
- Do not claim correctness you haven't verified.
- Do not handle only the happy path.
- Always state under what conditions the implementation works.
- Do not commit or push code unless explicitly asked to.

## Development guidelines

- Prefer Rust idioms and best practices. Implement standard traits for types
  (e.g. `From`, `TryFrom`, and `Display`) rather than custom methods or functions.
- Maintain modularity and separation of concerns. Avoid large functions, types,
  and modules that do too many things.
- Keep the main `lib.rs` and `main.rs` minimal. Implement functionality in their own
  submodules.
- Keep files small (ideally less than 1000 lines), and refactor long functions and
  methods to ensure they remain readable.
- Write unit tests for all new functionality. Use property-based testing with
  `proptest` where applicable. Take care to design tests that will detect any
  potential issues and fail if the implementation is incorrect.
- Use documentation comments (`///`) for _all_ types and functions. Documentation
  should be brief and to the point.
- Errors should be explicit and informative, and must indicate what pattern was
  unsupported and the location (typically an instruction span) where it occurred.
- Diagnostics should be descriptive but not overly long. They must contain a source.
  The following sources are used to indicate at which level the failure occurred.
  - `masm-lsp/syntax`: Used for parsing and syntax errors
  - `masm-lsp/analysis`: Used for warnings from static-analysis passes
  - `masm-lsp/decompilation`: User for decompilation failures

## Testing

- Tests fixtures are defined in `tests/fixtures` and test harnesses are defined in
  `tests/common`.
- Whenever the user points out a bug in the LSP server, make sure you have
  enough context to understand what the correct behavior should be. Bugs are
  usually surfaced in the downstream LSP client. Make sure that the bug is really
  in the LSP server before planning and implementing a fix. Bugs in the LSP client
  should not be fixed in the server and vice versa.
- If a bug in the LSP server has been confirmed, write a minimal test case
  checking for that behavior. This test should fail (since the bug is not fixed
  yet). When this test is in place, review the implementation to find the
  underlying root cause of the bug. Ensure that you understand the component where
  the underlying issue is, as well as how this component interacts with other
  parts of the application. Then, describe your findings to the user, together
  with a suggested fix. Remember that sometimes, a bug may indicate a systemic
  issue that requires a larger rewrite. Finally, When the issue is fixed, take
  care to remove any stale code that results from the fix. Keep the test as a
  regression test, ensuring that the bug is not reintroduced in the future.

## Build and Test Commands

```bash
cargo build                 # Build the project
cargo test                  # Run all tests
cargo test <test_name>      # Run a single test
cargo check                 # Fast type checking
cargo doc -p <package_name> # Package documentation
```
