# Miden Assembly Decompiler - Critical Review and Refactoring Plan

This document contains a comprehensive critical review of the decompiler implementation,
identifying bugs, code duplication, and areas for improvement.

---

## 1. Soundness Issues

### 1.1. `dupw` implementation inconsistency between modules

**Location**: `decompiler.rs:1217-1240` vs `stack_ops.rs:761-767`

**Problem**: The decompiler's `dupw_pseudocode` duplicates elements then pushes new variables,
resulting in 8 elements added instead of 4.

**Status**: [ ] Fixed

---

### 1.2. `AbstractState::movdn` potential off-by-one

**Location**: `abstract_interp.rs:526-536`

**Problem**: The implementation doesn't use `StackLike` trait and has subtle index calculation
that may differ from Miden VM semantics.

**Status**: [ ] Fixed

---

### 1.3. While loop condition semantics

**Location**: `decompiler.rs:1720-1821`

**Problem**: The `+1` adjustment in net effect calculation assumes the loop body always pushes
exactly one condition element. Unusual control flow could break this assumption.

**Status**: [ ] Reviewed

---

## 2. Bugs to Fix

### 2.1. `peek_name` missing underscore (HIGH PRIORITY)

**Location**: `decompiler.rs:145-153`

**Problem**: Generates `a0`, `a1` instead of `a_0`, `a_1` for dynamically discovered inputs.

```rust
// Current (broken):
format!("a{}", self.next_input_id + inputs_below)

// Should be:
format!("a_{}", self.next_input_id + inputs_below)
```

**Status**: [ ] Fixed

---

### 2.2. `SwapDw` implementation incorrect (HIGH PRIORITY)

**Location**: `decompiler.rs:411-420`

**Problem**: `SwapDw` should swap positions 0-7 with 8-15. Current implementation:
- First loop swaps 0-3 with 8-11
- Second loop swaps 4-7 with 8-11 (wrong!)

Positions 12-15 are never touched.

```rust
// Current (broken):
for i in 0..4 { state.swap(i, i + 8); }
for i in 4..8 { state.swap(i, i + 4); }  // This is wrong!

// Should be:
for i in 0..8 { state.swap(i, i + 8); }
```

**Status**: [ ] Fixed

---

### 2.3. `MTreeGet` stack effect mismatch (HIGH PRIORITY)

**Location**: `stack_effects.rs:58-66` vs `stack_ops.rs:240`

**Problem**: Inconsistent stack effects:
- `stack_ops.rs`: `StaticEffect::new(6, 4)` - pops 6, pushes 4
- `stack_effects.rs`: Only pops 2 elements (depth, index)

**Status**: [ ] Fixed

---

### 2.4. Abstract interpretation fallback handler

**Location**: `abstract_interp.rs:966-972`

**Problem**: Unhandled instructions push single `Top` regardless of actual stack effect.

```rust
// Current (broken):
_ => {
    state.push(SymbolicExpr::Top);
    None
}

// Should use static_effect to determine correct push/pop count
```

**Status**: [ ] Fixed

---

### 2.5. Procedure call contract lookup fragile

**Location**: `decompiler.rs:1458-1466`

**Problem**: Using `get_by_suffix` for path lookup could match wrong procedures if multiple
modules have procedures with the same name.

**Status**: [ ] Fixed

---

### 2.6. Loop counter aliasing in `apply_counter_indexing`

**Location**: `decompiler.rs:1476-1546`

**Problem**: Simple string replacement could incorrectly transform variable names that
happen to contain `a_` followed by digits (e.g., `data_a_0` becomes `data_a_i`).

**Status**: [ ] Fixed

---

## 3. Code Duplication

### 3.1. Three separate instruction dispatch tables

The code has **three** large instruction match statements:
1. `decompiler.rs:308-1203` - ~900 lines for pseudocode generation
2. `stack_ops.rs:47-308` - ~260 lines for static effects
3. `stack_effects.rs:16-1500+` - ~1500 lines for security analysis

**Status**: [ ] Documented (acceptable duplication for different concerns)

---

### 3.2. `AbstractState` doesn't use `StackLike` trait (MEDIUM PRIORITY)

**Location**: `abstract_interp.rs`

**Problem**: `AbstractState` has manual implementations of `movup`, `movdn`, `swap`, `dup`
that don't use the unified `StackLike` trait, risking inconsistencies.

**Status**: [ ] Fixed

---

## 4. File Organization

### 4.1. Split `decompiler.rs` (2651 lines)

Split into:
- `decompiler/state.rs` - `DecompilerState`, `NamedValue`, `SavedStackState`
- `decompiler/pseudocode.rs` - `generate_pseudocode` and helper functions
- `decompiler/collector.rs` - `DecompilationCollector` and visitor logic
- `decompiler/mod.rs` - re-exports

**Status**: [ ] Done

---

### 4.2. Split `stack_effects.rs` (1500+ lines)

Split by instruction category:
- `stack_effects/mod.rs` - main `apply_effect` dispatch and re-exports
- `stack_effects/arithmetic.rs` - arithmetic and comparison operations
- `stack_effects/memory.rs` - memory and local operations
- `stack_effects/crypto.rs` - cryptographic operations
- `stack_effects/u32_ops.rs` - u32 operations

**Status**: [ ] Done

---

## 5. Minor Issues

### 5.1. Remove dead code annotations

**Location**: `decompiler.rs:1553-1558`

`ProcedureDecompilation` struct and `current_proc` field have `#[allow(dead_code)]`.
These should either be used or removed.

**Status**: [ ] Fixed

---

## Implementation Order

| Priority | Task | Status |
|----------|------|--------|
| 1 | Fix `peek_name` underscore bug | [ ] |
| 2 | Fix `SwapDw` implementation | [ ] |
| 3 | Fix `MTreeGet` stack effects | [ ] |
| 4 | Split `decompiler.rs` into modules | [ ] |
| 5 | Split `stack_effects.rs` into modules | [ ] |
| 6 | Make `AbstractState` use `StackLike` trait | [ ] |
| 7 | Fix abstract interpretation fallback | [ ] |
| 8 | Fix procedure call contract lookup | [ ] |
| 9 | Fix loop counter aliasing | [ ] |
| 10 | Fix `dupw` implementation | [ ] |
| 11 | Remove dead code annotations | [ ] |

---

## Success Criteria

1. All identified bugs are fixed
2. All existing tests pass
3. `decompiler.rs` is split into focused modules (<500 lines each)
4. `stack_effects.rs` is split into focused modules (<400 lines each)
5. `AbstractState` uses `StackLike` trait consistently
6. No `#[allow(dead_code)]` on unused code
