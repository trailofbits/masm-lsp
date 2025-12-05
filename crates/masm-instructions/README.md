# masm-instructions

- Purpose: shared instruction metadata/semantics crate; build script only generates instruction info, semantics are frozen in source.
- Upstream deps: `phf`, `phf_codegen`, `toml` (build-time), `miden-assembly-syntax` (Instruction enum).
- Downstream users: `masm-lsp` (hovers, docs), analysis crates consuming instruction shapes.
- Main exports: `InstructionInfo`, `INSTRUCTION_MAP`, `semantics_of`, `InstructionEffect`.
