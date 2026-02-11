# masm-instructions

This crate provides instruction metadata for MASM, generated from
`data/instruction_reference.toml` at build time.

## What it provides

- `ToDescription` and `ToStackEffect` traits for `miden_assembly_syntax::ast::Instruction`
- Template substitution for `{n}`, `{nth}`, and `{n+k}` placeholders

## Regenerating the TOML

```bash
uv run --project crates/masm-instructions \
  crates/masm-instructions/scripts/convert_instruction_reference.py \
  -i crates/masm-instructions/docs/instruction_reference.md \
  -o crates/masm-instructions/data/instruction_reference.toml
```

Edits to the TOML file automatically trigger rebuilds via `build.rs`.
