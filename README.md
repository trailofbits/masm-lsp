# MASM LSP Server

An LSP server for the [Miden assembly language](https://0xmiden.github.io/miden-vm/user_docs/assembly/main.html) (MASM).

## References

- [Miden VM Documentation](https://0xmiden.github.io/miden-vm/)
- [Miden VM GitHub Repository](https://github.com/0xMiden/miden-vm)
- [Miden Assembly Language Reference](https://0xmiden.github.io/miden-vm/user_docs/assembly/main.html)
- [Miden stdlib](https://github.com/0xMiden/miden-vm/tree/main/stdlib)
- [Tree-sitter grammar](https://github.com/0xMiden/tree-sitter-masm)

## Running in VS Code

1. Build the server:
   ```bash
   cargo build --release
   ```
   The binary will be at `target/release/masm-lsp`.

2. Install a VS Code client:
   - Install the “Custom Language Server” or “LSP Inspector” extension (or another extension that lets you configure an external LSP).
   - Configure the server command to point to `masm-lsp` (absolute path), with no extra args.
   - Set file associations for MASM:
     ```json
     "files.associations": {
       "*.masm": "masm"
     }
     ```
   - In the extension’s settings, map language id `masm` to the `masm-lsp` command.

3. Optional: add syntax highlighting via the Tree-sitter MASM grammar or a MASM syntax extension; the LSP provides diagnostics/defs/refs regardless.

4. Reload VS Code. Open a MASM file and confirm that diagnostics and go-to-definition work. You can check the LSP client logs in VS Code’s “Output” panel for the configured client.
