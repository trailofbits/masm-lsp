# masm-decompiler

- Purpose: SSA-based MASM decompiler producing pseudocode hints and diagnostics.
- Upstream deps: `masm-analysis`, `masm-instructions`, `miden-assembly-syntax`, `miden-debug-types`, `tower-lsp`.
- Downstream users: `masm-lsp` (to render pseudocode inlay hints/diagnostics).
- Main exports: `collect_decompilation_hints`, SSA helpers/types (`DecompilerState`, `PseudocodeTemplate`, `SsaContext`, `VarKind`), and `SOURCE_DECOMPILATION`.
