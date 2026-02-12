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
