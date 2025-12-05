# masm-analysis

- Purpose: shared static analysis crate for MASM (contracts inference, stack/taint analysis, stack-effect formatting, symbol resolution).
- Upstream deps: `miden-assembly-syntax`, `miden-debug-types`, `masm-instructions`, `tower-lsp` (for Range/Diagnostic types).
- Downstream users: `masm-lsp`, `masm-decompiler`, and any tools needing MASM analysis utilities.
- Main exports: `analysis::*` (analyzer/checkers/contracts), `stack_effect`, `symbol_path`, `symbol_resolution`, `descriptions`, `instruction_docs`, `diagnostics` helpers.
