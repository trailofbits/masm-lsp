# Masm-LSP Analysis Documentation

This document describes the diagnostic analyses performed by the Masm-LSP.

## Overview

The LSP performs static analysis using:
- **Taint tracking**: Values from untrusted sources (advice stack, Merkle store) are tracked
- **Symbolic stack simulation**: Stack state is maintained abstractly through execution
- **Control flow analysis**: Branch merging with lattice-based state joins

---

## Diagnostics

### 1. Unresolved References

**Source**: `masm-lsp/syntax`
**Severity**: Error

**What it detects**: Procedure calls that cannot be resolved in the workspace.

**Analysis**: Attempts resolution by full path, suffix match, then name-only lookup.

**Why useful**: Catches typos and missing imports before execution.

**Unhandled edge cases**:
- External dependencies not in workspace are reported as errors
- MAST root invocations cannot be resolved

---

### 2. Unvalidated Advice in u32 Operations

**Source**: `masm-lsp/analysis`
**Severity**: Warning

**What it detects**: Values from the advice stack used in u32 operations without prior `u32assert`/`u32assert2`/`u32assertw`.

**Analysis**: Tracks value origin and validation state. Checks top 2 stack positions at each u32 operation (excluding `u32assert*`, `u32test*`, `u32split`, `u32cast`).

**Why useful**: u32 operations have undefined behavior if inputs exceed 0xFFFFFFFF. Malicious provers can inject invalid values via advice.

**Unhandled edge cases**:
- Only checks top 2 stack positions; deep stack unvalidated values are missed
- Custom validation patterns (e.g., range checks via comparison) not recognized
- Values validated then moved deep in stack lose tracking

---

### 3. Unvalidated Merkle Depth

**Source**: `masm-lsp/analysis`
**Severity**: Warning

**What it detects**: Unvalidated depth parameter in `mtree_get`, `mtree_set`, `mtree_verify`.

**Analysis**: Checks top-of-stack for unvalidated untrusted values at Merkle operations.

**Why useful**: Merkle depth must be <= 64. Invalid depth from advice causes runtime errors.

**Unhandled edge cases**:
- Same limitations as u32 validation tracking
- Depth validated via comparison (`lt.64`) not recognized as validation

---

### 4. Unvalidated Advice in Procedure Calls

**Source**: `masm-lsp/analysis`
**Severity**: Warning

**What it detects**: Passing unvalidated advice values to procedures that require validated u32 inputs.

**Analysis**:
1. Infers procedure contracts (validates inputs? uses u32 ops?)
2. At call sites, checks top 4 stack positions for unvalidated advice
3. Warns if callee requires u32 but doesn't validate, or if callee is unknown

**Why useful**: Prevents propagation of unvalidated values across procedure boundaries.

**Unhandled edge cases**:
- MAST root calls cannot be analyzed
- External procedures have unknown contracts (conservative warning)
- Only checks top 4 stack positions
- Cross-module contract inference not supported

---

### 5. Branch Stack Mismatch

**Source**: `masm-lsp/analysis`
**Severity**: Warning

**What it detects**: If/else blocks where the `then` and `else` branches have different net stack effects.

**Analysis**: For each `if.true` block:
1. Snapshots state before the branch
2. Analyzes both branches independently
3. Compares the net stack effect (pushes minus pops) of each branch
4. Emits error if effects differ

**Why useful**: Miden assembly requires both branches to have identical stack effects. Mismatched branches cause assembly errors.

**Unhandled edge cases**:
- Unknown procedure calls within branches clear stack tracking
- Analysis may fail silently if procedure effects are unknown

---

### 6. Uninitialized Local Variables

**Source**: `masm-lsp/analysis`
**Severity**: Warning

**What it detects**:
- Reading from locals never written (`loc.load` on uninitialized local)
- Reading from locals written only on some code paths
- Taking address of uninitialized locals (`locaddr`)

**Analysis**: Control flow analysis with three-state lattice:
- **Uninitialized**: Never written
- **MaybeInitialized**: Written on some paths (e.g., one branch of if/else)
- **Initialized**: Written on all paths

State merging rules:
- `if/else`: Must be written in both branches to be Initialized
- `while`: Body may not execute, so initializations become MaybeInitialized
- `repeat(n)`: If n > 0, body always executes at least once

**Why useful**: Reading uninitialized locals returns undefined values, leading to unpredictable behavior.

**Unhandled edge cases**:
- `locaddr` conservatively warns even if address is only written to
- Memory aliasing not tracked (pointer passed to procedure that writes)
- Loop iteration counts not analyzed (while loop always treated as zero-or-more)

---

## Architecture

```
Document
    │
    ├─► Syntax Diagnostics (parsing errors, unresolved refs)
    │
    └─► Module Analysis
            │
            ├─► Contract Inference (per-procedure behavior)
            │
            ├─► Stack Simulation Analysis
            │       ├─► U32 Validation Checker
            │       ├─► Merkle Depth Checker
            │       ├─► Procedure Call Checker
            │       └─► Branch Stack Mismatch Checker
            │
            └─► Locals Analysis
                    └─► Uninitialized Variable Detection
```

Checkers run before stack effects are applied, receiving the current symbolic state at each instruction.
