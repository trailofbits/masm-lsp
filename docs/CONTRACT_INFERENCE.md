# Contract Inference

This document describes the contract inference system used by the Miden Assembly LSP
to automatically infer procedure contracts from their implementations.

## Overview

Contract inference analyzes procedure implementations to determine:
- **Stack effects**: How many inputs a procedure consumes and outputs it produces
- **Validation behavior**: Whether a procedure validates its inputs (e.g., u32 range checks)
- **Operation flags**: Whether a procedure uses u32 ops, reads advice, or uses Merkle ops
- **Input/output signatures**: Detailed tracking of how inputs flow through to outputs

This information enables workspace-wide inter-procedural analysis for detecting issues
like unvalidated advice values or missing range checks.

## Algorithm

### Topological Ordering

Contract inference uses **topological ordering** to analyze procedures efficiently.
By processing procedures in dependency order (callees before callers), each procedure
is analyzed exactly once with full knowledge of its callees' contracts.

```
┌─────────────────────────────────────────────────────────────────┐
│ Call Graph (call_graph.rs)                                      │
│   - Tracks which procedures call which                          │
│   - Computes topological order with SCC detection               │
└─────────────────────────────────────────────────────────────────┘
                              │
                              │ Provides ordering
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│ Contract Inference (inference.rs)                               │
│   - Analyzes procedures in topological order                    │
│   - Each procedure analyzed exactly once                        │
│   - Uses AbstractState for symbolic stack tracking              │
└─────────────────────────────────────────────────────────────────┘
                              │
                              │ Produces
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│ ContractStore                                                   │
│   - Maps procedure paths to ProcContract                        │
│   - Indexed by name for fast lookups                            │
└─────────────────────────────────────────────────────────────────┘
```

### Strongly Connected Components (SCCs)

Mutually recursive procedures form cycles in the call graph. These are detected using
**Tarjan's algorithm** for finding strongly connected components:

```rust
pub enum TopologicalNode {
    /// A single procedure with no cycles
    Single(SymbolPath),

    /// A strongly connected component of mutually recursive procedures
    Cycle(Vec<SymbolPath>),
}
```

For cycles, the inference performs two passes:
1. First pass: Analyze all procedures in the cycle, recording effects
2. Second pass: Re-analyze with knowledge from the first pass

This allows effects to propagate through mutual recursion.

## Key Data Structures

### ProcContract

The main output type representing a procedure's inferred contract:

```rust
pub struct ProcContract {
    /// Fully qualified procedure path
    pub path: SymbolPath,

    /// Stack effect (inputs consumed, outputs produced)
    pub stack_effect: StackEffect,

    /// Whether the procedure validates its inputs
    pub validates: ValidationBehavior,

    /// Whether the procedure uses u32 operations
    pub uses_u32_ops: bool,

    /// Whether the procedure reads from advice provider
    pub reads_advice: bool,

    /// Whether the procedure uses Merkle operations
    pub uses_merkle_ops: bool,

    /// Detailed input/output signature
    pub signature: Option<ProcSignature>,

    /// Source location of the procedure definition
    pub definition_range: Option<Range>,
}
```

### StackEffect

Represents how a procedure affects the stack:

```rust
pub enum StackEffect {
    /// Fully known stack effect
    Known { inputs: usize, outputs: usize },

    /// Known inputs but unknown outputs (due to dynamic calls, etc.)
    KnownInputs { inputs: usize },

    /// Completely unknown effect
    Unknown,
}
```

### ValidationBehavior

Tracks whether a procedure validates its inputs:

```rust
pub enum ValidationBehavior {
    /// Procedure validates inputs (has u32assert at start)
    ValidatesU32,

    /// No validation detected
    None,
}
```

### AbstractState

The inference uses `AbstractState` from the abstract interpretation framework
for symbolic stack tracking. Each stack element is represented as a `SymbolicExpr`:

```rust
pub enum SymbolicExpr {
    /// An input parameter at a fixed position
    Input(usize),

    /// A constant literal value
    Constant(u64),

    /// Binary operation on two expressions
    BinaryOp { op: BinaryOpKind, left: Box<SymbolicExpr>, right: Box<SymbolicExpr> },

    /// A value loaded from memory
    MemoryLoad { address: Box<SymbolicExpr> },

    /// A value from the advice provider (untrusted)
    Advice,

    /// Unknown value (top element of lattice)
    Top,
}
```

This allows tracking:
- Which outputs come directly from inputs (passthrough)
- Which outputs are memory reads from input addresses
- Which inputs are used as memory addresses

## Analysis Process

### 1. Build Call Graph

```rust
let call_graph = CallGraph::from_module(module);
```

The call graph is built by visiting each procedure and collecting its call targets
(exec, call, syscall instructions).

### 2. Compute Topological Order

```rust
let topo_order = call_graph.topological_order();
```

Tarjan's algorithm produces SCCs in reverse topological order (leaves first),
which is exactly what we need - procedures with no dependencies are analyzed first.

### 3. Analyze Each Procedure

For each node in topological order:

**Single procedures:**
```rust
TopologicalNode::Single(path) => {
    let contract = infer_procedure_contract_with_store(proc, contracts, ...);
    working_store.update_document(vec![contract]);
}
```

**Cycles (mutual recursion):**
```rust
TopologicalNode::Cycle(paths) => {
    // First pass - collect initial effects
    for path in &paths {
        let contract = infer_procedure_contract_with_store(...);
        working_store.update_document(vec![contract]);
    }
    // Second pass - refine with knowledge of cycle
    for path in &paths {
        let contract = infer_procedure_contract_with_store(...);
        working_store.update_document(vec![contract]);
    }
}
```

### 4. Signature Propagation

After all procedures are analyzed, a second pass propagates input signatures
through call chains:

```rust
for (path, proc) in module.procedures() {
    if let Some(sig) = propagate_input_signatures(proc, &working_store) {
        // Update contract with propagated signature
    }
}
```

This ensures that if procedure A passes an input to procedure B which uses it
as a memory address, A's signature reflects that the input is an address.

## SignatureAnalyzer

The `SignatureAnalyzer` is the core visitor that walks a procedure's AST to infer
its contract. It tracks:

- **Validation state**: Whether we've seen validation instructions at the start
- **Operation flags**: u32 ops, advice reads, Merkle ops
- **Abstract state**: Symbolic stack via `AbstractState`
- **Input usage**: How each input is used (value, address, etc.)
- **Bounds tracking**: Constant bounds for loop analysis

Key methods:

```rust
impl SignatureAnalyzer {
    /// Analyze a single instruction
    fn analyze_instruction(&mut self, inst: &Instruction);

    /// Handle procedure calls with transitive effects
    fn handle_procedure_call(&mut self, target: &InvocationTarget);

    /// Build the final stack effect
    fn stack_effect(&self) -> StackEffect;

    /// Build detailed input/output signature
    fn build_signature(&self) -> Option<ProcSignature>;
}
```

## Input/Output Signatures

The `ProcSignature` type provides detailed tracking of procedure inputs and outputs:

```rust
pub struct ProcSignature {
    pub inputs: Vec<InputKind>,
    pub outputs: Vec<OutputKind>,
}

pub enum InputKind {
    Value,           // Generic value
    InputAddress,    // Used as address for memory read
    OutputAddress,   // Used as address for memory write
    InputOutputAddress, // Used for both read and write
}

pub enum OutputKind {
    Computed,        // Result of computation
    InputPassthrough { input_pos: usize },  // Direct passthrough of input
    MemoryRead { from_input: Option<usize> }, // Memory read, optionally from input address
}
```

This enables precise tracking of data flow through procedures.

## Control Flow Handling

### If/Else Branches

Both branches are analyzed to determine if they have matching stack effects:

```rust
Op::If { then_blk, else_blk, .. } => {
    let then_effect = compute_block_effect(then_blk);
    let else_effect = compute_block_effect(else_blk);

    if then_net == else_net {
        // Apply the common effect
        apply_pop_push(inputs, outputs);
    } else {
        // Mismatched branches - mark as unknown
        unknown_effect = true;
    }
}
```

### While Loops

While loops are analyzed to infer bounds when possible:

```rust
Op::While { body, .. } => {
    if let Some(bound) = infer_while_bound(body, contracts) {
        // Known iterations - multiply effect
        apply_pop_push(bound * body_pops, bound * body_pushes);
    } else {
        // Unknown iterations
        unknown_effect = true;
    }
}
```

### Repeat Loops

Repeat loops have known iteration counts:

```rust
Op::Repeat { count, body, .. } => {
    let iterations = count.value();
    let (body_in, body_out) = compute_block_effect(body);
    apply_pop_push(iterations * body_in, iterations * body_out);
}
```

## Usage

### Basic Usage

```rust
use masm_lsp::analysis::contracts::{infer_module_contracts, ContractStore};

// Infer contracts for a module
let contracts = infer_module_contracts(&module, &source_manager);

// Store contracts
let mut store = ContractStore::new();
store.update_document(contracts);

// Query contracts
if let Some(contract) = store.get_by_name("my_proc") {
    println!("Stack effect: {:?}", contract.stack_effect);
}
```

### With Existing Contracts

```rust
// Infer with knowledge of existing contracts (e.g., stdlib)
let contracts = infer_module_contracts_with_store(
    &module,
    &source_manager,
    Some(&existing_store),
);
```

## Performance

The topological ordering approach provides O(n) analysis where n is the number
of procedures, compared to O(n²) worst case for fix-point iteration. Each procedure
is analyzed exactly once (except for SCCs which get two passes).

## Limitations

1. **Dynamic calls**: `dyn_exec` and `dyn_call` have unknown effects
2. **MAST roots**: Calls to MAST roots have unknown effects
3. **Complex loops**: While loops without recognizable bound patterns
4. **External calls**: Calls to procedures not in the workspace use stdlib contracts or Unknown

## File Structure

```
src/analysis/
├── contracts/
│   ├── mod.rs          # Module exports
│   ├── inference.rs    # Main inference logic, SignatureAnalyzer
│   ├── store.rs        # ContractStore for workspace-wide storage
│   ├── types.rs        # ProcContract, StackEffect, etc.
│   └── signature.rs    # Procedure signature parsing
├── call_graph.rs       # Call graph and topological ordering
├── abstract_interpretation.rs  # AbstractState, SymbolicExpr
└── mod.rs              # Analysis module exports
```
