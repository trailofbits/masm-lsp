# Improved Stack Tracking for MASM Decompilation

This document describes the abstract interpretation framework for improving stack location tracking in the MASM decompiler.

## Problem Statement

The current decompiler has difficulty correctly tracking stack positions through loops. Consider this example:

```masm
pub proc add
    repeat.5
        movup.5
        add
        movdn.4
    end
end
```

The current decompiler produces:

```
pub proc add(a_0, a_1, a_2, a_3, a_4, a_5, a_6)
    for i in 0...5:
        v_0 = a_i + a_(i+5)
    end
```

This output has two problems:

1. **Incorrect input count**: The function actually takes 10 inputs, not 7
2. **Missing output representation**: The decompiled output doesn't show where results are stored

The expected output should be:

```
pub proc add(a_0, a_1, a_2, a_3, a_4, a_5, a_6, a_7, a_8, a_9) -> (r_0, r_1, r_2, r_3, r_4)
    for i in 0..5:
        r_i = a_i + a_(i+5)
    end
```

### Root Cause Analysis

The current approach:

1. Executes **one iteration** of the loop body
2. Measures the net stack effect
3. Post-hoc applies counter indexing (`a_N` → `a_(N+i*effect)`)

This fails because:

- **Input discovery pollutes effect measurement**: The first iteration discovers inputs dynamically, so the "net effect" includes both initial setup and per-iteration behavior
- **Simple linear indexing can't capture complex patterns**: Stack permutations (movup/movdn) aren't captured by simple counter indexing
- **Single-iteration simulation** can't distinguish between loop invariants and per-iteration changes

## Solution: Abstract Interpretation

Abstract interpretation is a principled framework for program analysis that provides:

1. **Soundness by construction**: Correct abstract domains and transfer functions guarantee correct analysis
2. **Guaranteed termination**: Widening operators ensure loops converge
3. **Precision recovery**: Narrowing can improve results after widening
4. **Compositionality**: Nested structures are handled naturally

### Key Concepts

#### Abstract Domain (Lattice)

Instead of tracking concrete values, we track **symbolic expressions** that represent the provenance and computation of each stack value:

```rust
enum SymbolicExpr {
    /// An input parameter: a_0, a_1, etc.
    Input(usize),

    /// A parametric input depending on loop counter: a_(base + i*stride)
    ParametricInput { base: i32, stride: i32, loop_depth: usize },

    /// A constant literal value
    Constant(u64),

    /// Binary operation: left op right
    BinaryOp { op: BinaryOpKind, left: Box<SymbolicExpr>, right: Box<SymbolicExpr> },

    /// Unknown value (top element ⊤)
    Top,
}
```

The lattice ordering is:

```
                    ⊤ (Top - unknown)
                   /|\
                  / | \
    Parametric(..) ... Parametric(..)
                  \ | /
    Computed(..)  Input(n)  Computed(..)
```

#### Transfer Functions

Each instruction has a **transfer function** that transforms the abstract state:

```rust
// Example: add instruction
fn transfer_add(state: &mut AbstractState) {
    let b = state.pop();  // Symbolic expression for top
    let a = state.pop();  // Symbolic expression for second
    let result = SymbolicExpr::BinaryOp {
        op: BinaryOpKind::Add,
        left: Box::new(a),
        right: Box::new(b),
    };
    state.push(result);
}
```

Stack manipulation instructions (movup, movdn, swap) simply permute the symbolic stack without creating new expressions.

#### Fixed-Point Computation for Loops

For loops, we compute the **least fixed point** of the loop body's transfer function:

```
┌─────────────────────────────────────────────────────────────┐
│                    LOOP ANALYSIS PHASES                      │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  Phase 1: Input Discovery                                    │
│  ─────────────────────────                                   │
│  • Start with empty state                                    │
│  • Execute loop body once                                    │
│  • Track how many inputs were accessed                       │
│                                                              │
│  Phase 2: Stable Effect Calculation                          │
│  ───────────────────────────────────                         │
│  • Start with discovered inputs                              │
│  • Execute loop body                                         │
│  • Calculate true net effect per iteration                   │
│                                                              │
│  Phase 3: Consistency Verification                           │
│  ────────────────────────────────                            │
│  • Execute loop body again                                   │
│  • Verify net effect is consistent                           │
│  • Detect patterns in stack transformation                   │
│                                                              │
│  Phase 4: Total Input Calculation                            │
│  ─────────────────────────────────                           │
│  • For consuming loops: inputs + (iterations-1) * |effect|   │
│  • For producing loops: discovered inputs are sufficient     │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

### Implementation

The implementation is in `src/analysis/abstract_interp.rs` and provides:

#### Core Types

```rust
/// Abstract state representing the symbolic stack
pub struct AbstractState {
    stack: Vec<SymbolicExpr>,
    discovered_inputs: usize,
    // ...
}

/// Result of analyzing a loop body
pub struct LoopAnalysis {
    pub min_inputs_required: usize,
    pub net_effect_per_iteration: i32,
    pub total_inputs_for_loop: Option<usize>,
    pub is_consistent: bool,
    // ...
}

/// Result of pre-analyzing a procedure
pub struct ProcedureAnalysis {
    pub total_inputs_required: usize,
    pub has_dynamic_stack: bool,
    // ...
}
```

#### Key Functions

```rust
/// Analyze a repeat loop to determine stack requirements
pub fn analyze_repeat_loop(body: &Block, iteration_count: usize) -> LoopAnalysis;

/// Analyze a while loop (unknown iteration count)
pub fn analyze_while_loop(body: &Block) -> LoopAnalysis;

/// Pre-analyze an entire procedure to discover total inputs
pub fn pre_analyze_procedure(body: &Block) -> ProcedureAnalysis;
```

### Example: Tracing Through the Problem

Let's trace the abstract interpretation through our example:

```masm
repeat.5
    movup.5
    add
    movdn.4
end
```

#### Phase 1: Input Discovery

Starting with empty state:

```
Initial:  []
movup.5:  Discovers a_0..a_5, then moves a_5 to top
          [a_4, a_3, a_2, a_1, a_0, a_5]
add:      Pops a_5 and a_0, pushes (a_0 + a_5)
          [a_4, a_3, a_2, a_1, (a_0 + a_5)]
movdn.4:  Moves result to position 4
          [a_4, a_3, a_2, a_1, (a_0 + a_5)]  -- reordered
```

Discovered inputs: 6

#### Phase 2: Stable Effect Calculation

Starting with 6 inputs [a_5, a_4, a_3, a_2, a_1, a_0]:

```
Initial:  [a_5, a_4, a_3, a_2, a_1, a_0]  depth=6
movup.5:  [a_4, a_3, a_2, a_1, a_0, a_5]
add:      [a_4, a_3, a_2, a_1, (a_0 + a_5)]  depth=5
movdn.4:  [a_1, a_2, a_3, a_4, (a_0 + a_5)]  depth=5
```

Net effect: 5 - 6 = **-1** (consuming one element per iteration)

#### Phase 3: Total Input Calculation

For 5 iterations with net effect -1:
- Total inputs = 6 + (5-1) * 1 = **10** inputs

This matches the expected behavior!

### Integration with Decompiler

To integrate with the existing decompiler:

1. **Pre-analyze procedures** before decompilation to determine total inputs
2. **Initialize with correct input count** from the start
3. **Use symbolic expressions** for parametric output generation

```rust
// In decompiler.rs visit_procedure:
fn visit_procedure(&mut self, proc: &Procedure) {
    // NEW: Pre-analyze to discover total inputs
    let analysis = pre_analyze_procedure(proc.body());

    let input_count = analysis.total_inputs_required
        .max(explicit_signature_inputs)
        .max(contract_inputs);

    // Initialize with correct input count
    self.state = Some(DecompilerState::new(input_count));

    // Continue with decompilation...
}
```

### Handling Different Loop Types

#### Bounded Loops (`repeat.N`)

For bounded loops, we can precisely calculate:
- Total inputs needed for all iterations
- The exact access pattern

```rust
let analysis = analyze_repeat_loop(body, count);
if let Some(total) = analysis.total_inputs_for_loop {
    state.ensure_depth(total);
}
```

#### Unbounded Loops (`while`)

For while loops, we use conservative analysis:
- Determine the per-iteration effect
- Mark as dynamic if non-zero effect
- Use widening if needed for termination

```rust
let analysis = analyze_while_loop(body);
if analysis.net_effect_per_iteration != 0 {
    // Mark as having dynamic stack content
    state.mark_dynamic_stack();
}
```

### Widening and Narrowing

For unbounded loops or complex patterns, widening ensures termination:

```rust
fn widen(prev: &AbstractState, next: &AbstractState) -> AbstractState {
    // Join corresponding positions
    for (p, n) in prev.stack.iter().zip(next.stack.iter()) {
        match (p, n) {
            // Same value: keep it
            (a, b) if a == b => a.clone(),

            // Different inputs: generalize to parametric
            (Input(i), Input(j)) => ParametricInput {
                base: *i,
                stride: (*j - *i),
                loop_depth: current_loop,
            },

            // Can't find pattern: go to Top
            _ => Top,
        }
    }
}
```

Narrowing can recover precision after widening stabilizes:

```rust
fn narrow(widened: &AbstractState, body: &Block) -> AbstractState {
    loop {
        let after_body = transfer_block(body, &current);
        current = meet(&current, &after_body);
        if !changed { break; }
    }
}
```

### Limitations and Future Work

#### Current Limitations

1. **Complex nested loops**: Multiple nested loops with interacting effects may lose precision
2. **Data-dependent control flow**: Branches that depend on runtime values can't be precisely analyzed
3. **Dynamic calls**: Procedure calls with unknown stack effects cause tracking to fail

#### Future Improvements

1. **Relational abstract domains**: Track relationships between stack positions
2. **Path-sensitive analysis**: Different abstract states for different control flow paths
3. **Interprocedural analysis**: Analyze called procedures to determine their stack effects

### References

- [Abstract Interpretation - Wikipedia](https://en.wikipedia.org/wiki/Abstract_interpretation)
- [Static Single Assignment for Decompilation](https://yurichev.com/mirrors/vanEmmerik_ssa.pdf) - Michael Van Emmerik's thesis
- [Ghidra Decompiler Documentation](https://www.lockshaw.io/static/ghidra/decompiler/doc/index.html)
- [Harvard CS252r: Widening and Narrowing](https://groups.seas.harvard.edu/courses/cs252/2011sp/slides/Lec12-AbstractInt-2.pdf)
- [Bruno Blanchet: Introduction to Abstract Interpretation](https://bblanche.gitlabpages.inria.fr/absint.pdf)

### Module Structure

```
src/analysis/
├── mod.rs                 # Module exports
├── abstract_interp.rs     # Abstract interpretation framework
│   ├── SymbolicExpr       # Abstract domain for stack values
│   ├── AbstractState      # Symbolic stack state
│   ├── transfer_*         # Transfer functions for instructions
│   ├── analyze_*_loop     # Loop analysis with fixed-point
│   └── pre_analyze_*      # Pre-analysis for input discovery
├── types.rs               # Existing analysis types
├── stack_effects.rs       # Existing stack effect handling
└── ...
```

### Testing

The module includes unit tests for:

- Symbolic expression formatting
- Abstract state operations (push, pop, dup, swap, movup, movdn)
- Parametric expression lifting
- Expression joining at control flow merge points

Run tests with:

```bash
cargo test abstract_interp
```

### Conclusion

Abstract interpretation provides a principled foundation for stack tracking that:

1. **Correctly handles bounded loops** by computing fixed points
2. **Discovers all required inputs** before decompilation
3. **Generates accurate parametric expressions** for loop patterns
4. **Gracefully degrades** for complex or dynamic patterns

The implementation is modular and can be incrementally integrated with the existing decompiler to improve output quality.
