# MASM LSP Server

An LSP server for the [Miden assembly language](https://0xmiden.github.io/miden-vm/user_docs/assembly/main.html) (MASM).

This language server can be run against any IDE that supports the language server protocol. It is installed automatically by the [VS Code extension for MASM](https://github.com/trailofbits/vscode-masm), but can also be used with other editors such as Neovim, Emacs, or Sublime Text.

## Crate snapshot

- Purpose: LSP server and CLI entry point for MASM (hover, diagnostics, inlay hints, decompilation, analysis).
- Upstream deps: `miden-assembly-syntax`, `miden-debug-types`, `masm-instructions`, `tower-lsp`, `tokio`.
- Downstream users: Editor clients via the LSP protocol.
- Key types/APIs: `ServerConfig`, `InlayHintType`, handlers in `src/server`, diagnostic helpers in `src/diagnostics`, decompiler/analysis facades in `src/decompiler` and `src/analysis`.

## References

- [Miden VM Documentation](https://0xmiden.github.io/miden-vm/)
- [Miden VM GitHub Repository](https://github.com/0xMiden/miden-vm)
- [Miden Assembly Language Reference](https://0xmiden.github.io/miden-vm/user_docs/assembly/main.html)
- [Miden stdlib](https://github.com/0xMiden/miden-vm/tree/main/stdlib)
