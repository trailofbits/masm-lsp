//! Shared stack operations and instruction effect metadata.
//!
//! This module provides:
//! - A unified `StackLike` trait for stack manipulation operations
//! - Static stack effect tables for fast-path analysis
//! - Simple arithmetic stack analysis that doesn't require symbolic execution

use miden_assembly_syntax::ast::{Block, Immediate, Instruction, InvocationTarget, Op};

use super::contracts::{ContractStore, StackEffect};

// ═══════════════════════════════════════════════════════════════════════════════
// Static Stack Effects
// ═══════════════════════════════════════════════════════════════════════════════

/// Static stack effect: (pops, pushes) for an instruction.
///
/// This represents the number of elements removed from and added to the stack
/// by a single instruction execution.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct StaticEffect {
    /// Number of elements popped from the stack
    pub pops: usize,
    /// Number of elements pushed to the stack
    pub pushes: usize,
}

impl StaticEffect {
    /// Create a new static effect.
    pub const fn new(pops: usize, pushes: usize) -> Self {
        Self { pops, pushes }
    }

    /// Net change in stack depth.
    pub const fn net(&self) -> i32 {
        self.pushes as i32 - self.pops as i32
    }

    /// Check if this is a stack-neutral operation.
    pub const fn is_neutral(&self) -> bool {
        self.pops == self.pushes
    }
}

/// Get the static stack effect for an instruction.
///
/// Returns `None` for instructions with dynamic effects (procedure calls,
/// dynamic dispatch, etc.) that cannot be determined statically.
pub fn static_effect(inst: &Instruction) -> Option<StaticEffect> {
    use Instruction::*;

    Some(match inst {
        // ─────────────────────────────────────────────────────────────────────
        // Stack manipulation
        // ─────────────────────────────────────────────────────────────────────
        Drop => StaticEffect::new(1, 0),
        DropW => StaticEffect::new(4, 0),

        // Dup: pop 0, push 1 (copy from position n)
        Dup0 | Dup1 | Dup2 | Dup3 | Dup4 | Dup5 | Dup6 | Dup7 | Dup8 | Dup9 | Dup10 | Dup11
        | Dup12 | Dup13 | Dup14 | Dup15 => StaticEffect::new(0, 1),

        // DupW: pop 0, push 4 (copy word from position n)
        DupW0 | DupW1 | DupW2 | DupW3 => StaticEffect::new(0, 4),

        // Swap: pop 0, push 0 (reorder only)
        Swap1 | Swap2 | Swap3 | Swap4 | Swap5 | Swap6 | Swap7 | Swap8 | Swap9 | Swap10
        | Swap11 | Swap12 | Swap13 | Swap14 | Swap15 => StaticEffect::new(0, 0),

        // SwapW: swap words (reorder only)
        SwapW1 | SwapW2 | SwapW3 | SwapDw => StaticEffect::new(0, 0),

        // MovUp/MovDn: reorder only
        MovUp2 | MovUp3 | MovUp4 | MovUp5 | MovUp6 | MovUp7 | MovUp8 | MovUp9 | MovUp10
        | MovUp11 | MovUp12 | MovUp13 | MovUp14 | MovUp15 => StaticEffect::new(0, 0),

        MovDn2 | MovDn3 | MovDn4 | MovDn5 | MovDn6 | MovDn7 | MovDn8 | MovDn9 | MovDn10
        | MovDn11 | MovDn12 | MovDn13 | MovDn14 | MovDn15 => StaticEffect::new(0, 0),

        // MovUpW/MovDnW: reorder words
        MovUpW2 | MovUpW3 => StaticEffect::new(0, 0),
        MovDnW2 | MovDnW3 => StaticEffect::new(0, 0),

        // ─────────────────────────────────────────────────────────────────────
        // Arithmetic operations
        // ─────────────────────────────────────────────────────────────────────

        // Binary: pop 2, push 1
        Add | Sub | Mul | Div | And | Or | Xor => StaticEffect::new(2, 1),

        // Binary with immediate: pop 1, push 1
        AddImm(_) | SubImm(_) | MulImm(_) | DivImm(_) | ExpImm(_) => StaticEffect::new(1, 1),

        // Unary: pop 1, push 1
        Neg | Inv | Incr | Not | IsOdd | Pow2 | ILog2 => StaticEffect::new(1, 1),

        // Exp: pop 2, push 1
        Exp => StaticEffect::new(2, 1),
        ExpBitLength(_) => StaticEffect::new(2, 1),

        // ─────────────────────────────────────────────────────────────────────
        // Comparison operations
        // ─────────────────────────────────────────────────────────────────────

        // Binary comparisons: pop 2, push 1
        Eq | Neq | Lt | Lte | Gt | Gte => StaticEffect::new(2, 1),

        // Comparisons with immediate: pop 1, push 1
        EqImm(_) | NeqImm(_) => StaticEffect::new(1, 1),

        // Word equality: pop 8 (two words), push 1
        Eqw => StaticEffect::new(8, 1),

        // ─────────────────────────────────────────────────────────────────────
        // u32 operations
        // ─────────────────────────────────────────────────────────────────────

        // Assertions (no stack change, may trap)
        U32Assert | U32Assert2 | U32AssertW
        | U32AssertWithError(_) | U32Assert2WithError(_) | U32AssertWWithError(_) => {
            StaticEffect::new(0, 0)
        }

        // Test: push bool without consuming value
        U32Test => StaticEffect::new(0, 1),
        U32TestW => StaticEffect::new(0, 1),

        // Wrapping arithmetic: pop 2, push 1
        U32WrappingAdd | U32WrappingSub | U32WrappingMul => StaticEffect::new(2, 1),
        U32WrappingAddImm(_) | U32WrappingSubImm(_) | U32WrappingMulImm(_) => {
            StaticEffect::new(1, 1)
        }

        // Overflowing arithmetic: pop 2, push 2 (result + overflow flag)
        U32OverflowingAdd | U32OverflowingSub | U32OverflowingMul => StaticEffect::new(2, 2),
        U32OverflowingAddImm(_) | U32OverflowingSubImm(_) | U32OverflowingMulImm(_) => {
            StaticEffect::new(1, 2)
        }

        // OverflowingAdd3/MAdd: pop 3, push 2
        U32OverflowingAdd3 | U32OverflowingMadd => StaticEffect::new(3, 2),

        // WrappingAdd3/MAdd: pop 3, push 1
        U32WrappingAdd3 | U32WrappingMadd => StaticEffect::new(3, 1),

        // Division: DivMod returns both quotient and remainder, Div/Mod return one value
        U32DivMod => StaticEffect::new(2, 2),
        U32Div | U32Mod => StaticEffect::new(2, 1),
        U32DivModImm(_) => StaticEffect::new(1, 2),
        U32DivImm(_) | U32ModImm(_) => StaticEffect::new(1, 1),

        // Split: pop 1, push 2 (high, low)
        U32Split => StaticEffect::new(1, 2),

        // Cast: pop 1, push 1
        U32Cast => StaticEffect::new(1, 1),

        // Bitwise: pop 2, push 1
        U32And | U32Or | U32Xor => StaticEffect::new(2, 1),

        // Bitwise unary: pop 1, push 1
        U32Not => StaticEffect::new(1, 1),

        // Shifts: pop 2, push 1
        U32Shl | U32Shr | U32Rotl | U32Rotr => StaticEffect::new(2, 1),
        U32ShlImm(_) | U32ShrImm(_) | U32RotlImm(_) | U32RotrImm(_) => StaticEffect::new(1, 1),

        // Popcount/leading/trailing: pop 1, push 1
        U32Popcnt | U32Clz | U32Ctz | U32Clo | U32Cto => StaticEffect::new(1, 1),

        // u32 comparisons: pop 2, push 1
        U32Lt | U32Lte | U32Gt | U32Gte | U32Min | U32Max => StaticEffect::new(2, 1),

        // ─────────────────────────────────────────────────────────────────────
        // Push operations
        // ─────────────────────────────────────────────────────────────────────

        Push(_) => StaticEffect::new(0, 1),
        PushFeltList(values) => StaticEffect::new(0, values.len()),
        PadW => StaticEffect::new(0, 4),
        PushSlice(_, range) => StaticEffect::new(0, range.len()),

        // ─────────────────────────────────────────────────────────────────────
        // Memory operations
        // ─────────────────────────────────────────────────────────────────────

        // Single element load: pop 1 (addr), push 1 (value)
        MemLoad => StaticEffect::new(1, 1),
        MemLoadImm(_) => StaticEffect::new(0, 1),

        // Single element store: pop 2 (addr, value), push 0
        MemStore => StaticEffect::new(2, 0),
        MemStoreImm(_) => StaticEffect::new(1, 0),

        // Word load: [A, a, ...] -> [V, ...] where A is word to overwrite, a is addr, V is loaded
        // Pop 5 (4 word elements + 1 addr), push 4 (loaded word)
        MemLoadWBe | MemLoadWLe => StaticEffect::new(5, 4),
        // Immediate: [A, ...] -> [V, ...] - pop 4 (word to overwrite), push 4 (loaded word)
        MemLoadWBeImm(_) | MemLoadWLeImm(_) => StaticEffect::new(4, 4),

        // Word store: pop 5 (addr + word), push 0
        MemStoreWBe | MemStoreWLe => StaticEffect::new(5, 0),
        MemStoreWBeImm(_) | MemStoreWLeImm(_) => StaticEffect::new(4, 0),

        // Memstream: pop 13 (addr + 12 state), push 13
        MemStream => StaticEffect::new(13, 13),

        // ─────────────────────────────────────────────────────────────────────
        // Local memory operations
        // ─────────────────────────────────────────────────────────────────────

        LocLoad(_) => StaticEffect::new(0, 1),
        LocStore(_) => StaticEffect::new(1, 0),
        LocLoadWBe(_) | LocLoadWLe(_) => StaticEffect::new(0, 4),
        LocStoreWBe(_) | LocStoreWLe(_) => StaticEffect::new(4, 0),
        Locaddr(_) => StaticEffect::new(0, 1),

        // ─────────────────────────────────────────────────────────────────────
        // Advice operations
        // ─────────────────────────────────────────────────────────────────────

        AdvPush(n) => {
            let count = match n {
                Immediate::Value(v) => v.into_inner() as usize,
                _ => return None, // Unknown count
            };
            StaticEffect::new(0, count)
        }
        AdvLoadW => StaticEffect::new(4, 4), // Pop address word, push value word
        AdvPipe => StaticEffect::new(8, 8),  // Transform hasher state

        // ─────────────────────────────────────────────────────────────────────
        // Cryptographic operations
        // ─────────────────────────────────────────────────────────────────────

        Hash => StaticEffect::new(4, 4),
        HMerge => StaticEffect::new(8, 4),
        HPerm => StaticEffect::new(12, 12),

        // ─────────────────────────────────────────────────────────────────────
        // Merkle tree operations
        // ─────────────────────────────────────────────────────────────────────

        MTreeGet => StaticEffect::new(6, 4),  // Pop (depth, index, root), push value
        MTreeSet => StaticEffect::new(10, 4), // Pop (depth, index, old_root, value), push new_root
        MTreeMerge => StaticEffect::new(8, 4),
        MTreeVerify | MTreeVerifyWithError(_) => StaticEffect::new(0, 0),

        // ─────────────────────────────────────────────────────────────────────
        // Extension field operations (ext2)
        // ─────────────────────────────────────────────────────────────────────

        // Binary ext2: pop 4, push 2
        Ext2Add | Ext2Sub | Ext2Mul | Ext2Div => StaticEffect::new(4, 2),

        // Unary ext2: pop 2, push 2
        Ext2Neg | Ext2Inv => StaticEffect::new(2, 2),

        // ─────────────────────────────────────────────────────────────────────
        // Assertions
        // ─────────────────────────────────────────────────────────────────────

        Assert | AssertWithError(_) => StaticEffect::new(1, 0),
        AssertEq | AssertEqWithError(_) => StaticEffect::new(2, 0),
        Assertz | AssertzWithError(_) => StaticEffect::new(1, 0),
        AssertEqw | AssertEqwWithError(_) => StaticEffect::new(8, 0),

        // ─────────────────────────────────────────────────────────────────────
        // Conditional operations
        // ─────────────────────────────────────────────────────────────────────

        // CSwap: pop condition + 2 values, push 2 values (possibly swapped)
        CSwap => StaticEffect::new(3, 2),
        // CSwapW: pop condition + 2 words (8 elements), push 2 words (possibly swapped)
        CSwapW => StaticEffect::new(9, 8),
        // CDrop: pop condition + 2 values, push 1 (selected value)
        CDrop => StaticEffect::new(3, 1),
        // CDropW: pop condition + 2 words (8 elements), push 1 word (selected)
        CDropW => StaticEffect::new(9, 4),

        // ─────────────────────────────────────────────────────────────────────
        // Other operations
        // ─────────────────────────────────────────────────────────────────────

        Sdepth => StaticEffect::new(0, 1),
        Clk => StaticEffect::new(0, 1),
        Caller => StaticEffect::new(0, 4),
        ProcRef(_) => StaticEffect::new(0, 4), // Push procedure hash

        // Reverse operations (reorder only)
        Reversew => StaticEffect::new(0, 0),
        Reversedw => StaticEffect::new(0, 0),

        // No-ops
        Nop | Breakpoint | Debug(_) | Emit | EmitImm(_) | Trace(_) | SysEvent(_) => {
            StaticEffect::new(0, 0)
        }

        // ─────────────────────────────────────────────────────────────────────
        // Procedure calls - DYNAMIC (unknown effects)
        // ─────────────────────────────────────────────────────────────────────

        Exec(_) | Call(_) | SysCall(_) => return None,
        DynExec | DynCall => return None,

        // ─────────────────────────────────────────────────────────────────────
        // Complex STARK operations - unknown effects
        // ─────────────────────────────────────────────────────────────────────

        FriExt2Fold4 | HornerBase | HornerExt | EvalCircuit | LogPrecompile => return None,
    })
}

// ═══════════════════════════════════════════════════════════════════════════════
// Simple Stack Analysis (Fast Path)
// ═══════════════════════════════════════════════════════════════════════════════

/// Result of simple stack analysis.
#[derive(Clone, Debug)]
pub enum StackEffectResult {
    /// Stack effect is fully known.
    Known {
        /// Number of inputs required
        inputs: usize,
        /// Number of outputs produced (after consuming inputs)
        outputs: usize,
    },
    /// Stack effect depends on runtime values or unknown procedures.
    Dynamic {
        /// Minimum inputs discovered before failure
        min_inputs: usize,
        /// Reason analysis failed
        reason: String,
    },
}

/// Lightweight stack analysis using arithmetic only.
///
/// Does not track symbolic values, just depths. This is much faster than
/// full abstract interpretation and sufficient for determining stack effects
/// of most code.
///
/// When a `ContractStore` is provided, procedure calls can be resolved to their
/// stack effects, allowing the analysis to continue past call sites instead of
/// falling back to `Dynamic`.
#[derive(Clone, Debug)]
pub struct SimpleStackAnalysis<'a> {
    /// Current stack depth relative to procedure entry.
    depth: i32,
    /// Minimum depth reached (negative = inputs discovered).
    min_depth: i32,
    /// Maximum depth reached.
    max_depth: i32,
    /// Whether analysis is still valid.
    valid: bool,
    /// Reason for invalidity.
    invalid_reason: Option<String>,
    /// Optional contract store for resolving procedure calls.
    contract_store: Option<&'a ContractStore>,
}

impl Default for SimpleStackAnalysis<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> SimpleStackAnalysis<'a> {
    /// Create a new analysis state.
    pub fn new() -> Self {
        Self {
            depth: 0,
            min_depth: 0,
            max_depth: 0,
            valid: true,
            invalid_reason: None,
            contract_store: None,
        }
    }

    /// Create analysis with a contract store for resolving procedure calls.
    pub fn with_contracts(contract_store: &'a ContractStore) -> Self {
        Self {
            depth: 0,
            min_depth: 0,
            max_depth: 0,
            valid: true,
            invalid_reason: None,
            contract_store: Some(contract_store),
        }
    }

    /// Create analysis starting with a given number of inputs on stack.
    pub fn with_inputs(input_count: usize) -> Self {
        Self {
            depth: input_count as i32,
            min_depth: 0,
            max_depth: input_count as i32,
            valid: true,
            invalid_reason: None,
            contract_store: None,
        }
    }

    /// Create analysis with both inputs and a contract store.
    pub fn with_inputs_and_contracts(input_count: usize, contract_store: &'a ContractStore) -> Self {
        Self {
            depth: input_count as i32,
            min_depth: 0,
            max_depth: input_count as i32,
            valid: true,
            invalid_reason: None,
            contract_store: Some(contract_store),
        }
    }

    /// Check if analysis is still valid.
    pub fn is_valid(&self) -> bool {
        self.valid
    }

    /// Get the reason for invalidity.
    pub fn invalid_reason(&self) -> Option<&str> {
        self.invalid_reason.as_deref()
    }

    /// Get the current stack depth.
    pub fn depth(&self) -> i32 {
        self.depth
    }

    /// Get the number of inputs required.
    pub fn inputs_required(&self) -> usize {
        (-self.min_depth).max(0) as usize
    }

    /// Get the net stack effect.
    pub fn net_effect(&self) -> i32 {
        self.depth
    }

    /// Mark analysis as invalid.
    fn invalidate(&mut self, reason: &str) {
        self.valid = false;
        self.invalid_reason = Some(reason.to_string());
    }

    /// Apply an instruction's effect.
    pub fn apply_instruction(&mut self, inst: &Instruction) -> bool {
        if !self.valid {
            return false;
        }

        match static_effect(inst) {
            Some(effect) => {
                self.depth -= effect.pops as i32;
                self.min_depth = self.min_depth.min(self.depth);
                self.depth += effect.pushes as i32;
                self.max_depth = self.max_depth.max(self.depth);
                true
            }
            None => {
                // Try to resolve procedure calls via contract store
                if self.try_apply_procedure_call(inst) {
                    return true;
                }
                // Dynamic effect - invalidate
                self.invalidate(&format!("dynamic effect: {:?}", inst));
                false
            }
        }
    }

    /// Try to apply a procedure call's stack effect from the contract store.
    ///
    /// Returns `true` if the call was resolved and the effect was applied,
    /// `false` if the call couldn't be resolved.
    fn try_apply_procedure_call(&mut self, inst: &Instruction) -> bool {
        let contract_store = match self.contract_store {
            Some(store) => store,
            None => return false,
        };

        let target = match inst {
            Instruction::Exec(t) | Instruction::Call(t) | Instruction::SysCall(t) => t,
            _ => return false,
        };

        let target_name = match target {
            InvocationTarget::Symbol(ident) => ident.as_str(),
            InvocationTarget::Path(path) => path.inner().as_str(),
            InvocationTarget::MastRoot(_) => return false, // Can't resolve MAST roots
        };

        // Look up the contract by suffix (handles both short names and full paths)
        let contract = match contract_store.get_by_suffix(target_name) {
            Some(c) => c,
            None => return false, // Contract not found
        };

        match &contract.stack_effect {
            StackEffect::Known { inputs, outputs } => {
                self.depth -= *inputs as i32;
                self.min_depth = self.min_depth.min(self.depth);
                self.depth += *outputs as i32;
                self.max_depth = self.max_depth.max(self.depth);
                true
            }
            StackEffect::KnownInputs { inputs } => {
                // We know inputs but not outputs - can't continue simple analysis
                self.depth -= *inputs as i32;
                self.min_depth = self.min_depth.min(self.depth);
                // Mark as invalid since we don't know outputs
                self.invalidate(&format!(
                    "procedure '{}' has unknown output count",
                    target_name
                ));
                false
            }
            StackEffect::Unknown => {
                self.invalidate(&format!(
                    "procedure '{}' has unknown stack effect",
                    target_name
                ));
                false
            }
        }
    }

    /// Analyze a block, returns net effect if successful.
    pub fn analyze_block(&mut self, block: &Block) -> Option<i32> {
        let start_depth = self.depth;

        for op in block.iter() {
            if !self.analyze_op(op) {
                return None;
            }
        }

        Some(self.depth - start_depth)
    }

    /// Analyze a single operation.
    fn analyze_op(&mut self, op: &Op) -> bool {
        if !self.valid {
            return false;
        }

        match op {
            Op::Inst(inst) => self.apply_instruction(inst.inner()),
            Op::If {
                then_blk, else_blk, ..
            } => self.analyze_if_else(then_blk, else_blk),
            Op::While { body, .. } => self.analyze_while(body),
            Op::Repeat { count, body, .. } => self.analyze_repeat(*count, body),
        }
    }

    /// Analyze an if/else.
    fn analyze_if_else(&mut self, then_blk: &Block, else_blk: &Block) -> bool {
        // Condition is consumed
        self.depth -= 1;
        self.min_depth = self.min_depth.min(self.depth);

        let entry_depth = self.depth;

        // Analyze then branch
        let mut then_state = self.clone();
        let then_effect = match then_state.analyze_block(then_blk) {
            Some(e) => e,
            None => {
                self.invalidate(
                    then_state
                        .invalid_reason()
                        .unwrap_or("then branch failed"),
                );
                return false;
            }
        };

        // Analyze else branch
        let mut else_state = self.clone();
        let else_effect = if else_blk.is_empty() {
            0
        } else {
            match else_state.analyze_block(else_blk) {
                Some(e) => e,
                None => {
                    self.invalidate(
                        else_state
                            .invalid_reason()
                            .unwrap_or("else branch failed"),
                    );
                    return false;
                }
            }
        };

        // Check branch consistency
        if then_effect != else_effect {
            self.invalidate(&format!(
                "branch mismatch: then={}, else={}",
                then_effect, else_effect
            ));
            return false;
        }

        // Update state
        self.depth = entry_depth + then_effect;
        self.min_depth = self.min_depth.min(then_state.min_depth).min(else_state.min_depth);
        self.max_depth = self.max_depth.max(then_state.max_depth).max(else_state.max_depth);

        true
    }

    /// Analyze a while loop.
    ///
    /// While loops require abstract interpretation for precise analysis,
    /// but we can detect simple cases.
    fn analyze_while(&mut self, body: &Block) -> bool {
        // Condition is consumed at entry
        self.depth -= 1;
        self.min_depth = self.min_depth.min(self.depth);

        // Analyze one iteration to find the body's effect
        let mut body_state = self.clone();
        let body_effect = match body_state.analyze_block(body) {
            Some(e) => e,
            None => {
                self.invalidate(
                    body_state
                        .invalid_reason()
                        .unwrap_or("while body failed"),
                );
                return false;
            }
        };

        // While body must push a new condition
        // So if body_effect == 1, the loop is stack-neutral (pushes condition to replace consumed one)
        // If body_effect != 1, the loop has variable stack effect based on iterations
        if body_effect != 1 {
            self.invalidate("while loop with non-neutral stack effect requires abstract interpretation");
            return false;
        }

        // Stack-neutral while loop: after loop, condition was consumed but not replaced
        // (loop exited when condition was false)
        // So net effect is -1 (condition consumed, no final replacement)
        // But actually: the body pushes 1, which becomes the next condition
        // When loop exits, that condition is NOT consumed again... this is complex

        // For now, be conservative: require abstract interpretation for while loops
        self.invalidate("while loop requires abstract interpretation");
        false
    }

    /// Analyze a repeat loop.
    fn analyze_repeat(&mut self, count: u32, body: &Block) -> bool {
        if count == 0 {
            return true; // No iterations
        }

        // Analyze one iteration to find the body's effect
        let mut body_state = self.clone();
        let body_effect = match body_state.analyze_block(body) {
            Some(e) => e,
            None => {
                self.invalidate(
                    body_state
                        .invalid_reason()
                        .unwrap_or("repeat body failed"),
                );
                return false;
            }
        };

        // Calculate total effect
        let total_effect = body_effect * count as i32;

        // Calculate minimum depth reached during the loop
        // This is tricky: we need to track the minimum across all iterations
        if body_effect < 0 {
            // Consuming loop: minimum depth decreases each iteration
            // After k iterations, we're at: start + k * body_effect
            // Minimum during iteration k is: start + k * body_effect + body_state.min_depth - self.depth

            // The absolute minimum is reached at the end of the last iteration's minimum point
            let body_min_relative = body_state.min_depth - self.depth; // min relative to iteration start
            let final_min = self.depth + (count - 1) as i32 * body_effect + body_min_relative;
            self.min_depth = self.min_depth.min(final_min);
        } else {
            // Neutral or producing loop: minimum is in first iteration
            self.min_depth = self.min_depth.min(body_state.min_depth);
        }

        // Update max depth similarly
        if body_effect > 0 {
            let body_max_relative = body_state.max_depth - self.depth;
            let final_max = self.depth + (count - 1) as i32 * body_effect + body_max_relative;
            self.max_depth = self.max_depth.max(final_max);
        } else {
            self.max_depth = self.max_depth.max(body_state.max_depth);
        }

        self.depth += total_effect;
        true
    }
}

/// Analyze a procedure's stack effect using the fast path.
///
/// Falls back to returning `Dynamic` if the procedure contains constructs
/// that require abstract interpretation (while loops, procedure calls).
pub fn analyze_procedure_simple(body: &Block) -> StackEffectResult {
    analyze_procedure_simple_with_contracts(body, None)
}

/// Analyze a procedure's stack effect using the fast path with contract resolution.
///
/// When a `ContractStore` is provided, procedure calls can be resolved to their
/// known stack effects, allowing the analysis to succeed for more procedures.
///
/// Falls back to returning `Dynamic` if the procedure contains constructs
/// that require abstract interpretation (while loops, unresolved procedure calls).
pub fn analyze_procedure_simple_with_contracts(
    body: &Block,
    contract_store: Option<&ContractStore>,
) -> StackEffectResult {
    let mut analysis = match contract_store {
        Some(store) => SimpleStackAnalysis::with_contracts(store),
        None => SimpleStackAnalysis::new(),
    };

    match analysis.analyze_block(body) {
        Some(net_effect) => {
            let inputs = analysis.inputs_required();
            let outputs = (inputs as i32 + net_effect) as usize;
            StackEffectResult::Known { inputs, outputs }
        }
        None => StackEffectResult::Dynamic {
            min_inputs: analysis.inputs_required(),
            reason: analysis
                .invalid_reason()
                .unwrap_or("unknown")
                .to_string(),
        },
    }
}

/// Analyze a procedure's stack effect using tiered analysis.
///
/// This function implements the two-tier analysis strategy:
/// 1. First tries the fast arithmetic analysis (SimpleStackAnalysis)
/// 2. Falls back to full abstract interpretation if needed
///
/// Use this function when you need the most accurate analysis possible.
pub fn analyze_procedure_tiered(body: &Block) -> StackEffectResult {
    analyze_procedure_tiered_with_contracts(body, None)
}

/// Analyze a procedure's stack effect using tiered analysis with contract resolution.
///
/// When a `ContractStore` is provided, procedure calls can be resolved to their
/// known stack effects during the fast path.
///
/// This function implements the two-tier analysis strategy:
/// 1. First tries the fast arithmetic analysis (SimpleStackAnalysis)
/// 2. Falls back to full abstract interpretation if needed
///
/// Use this function when you need the most accurate analysis possible.
pub fn analyze_procedure_tiered_with_contracts(
    body: &Block,
    contract_store: Option<&ContractStore>,
) -> StackEffectResult {
    use super::abstract_interpretation::pre_analyze_procedure;

    // Tier 1: Try fast path
    let mut simple = match contract_store {
        Some(store) => SimpleStackAnalysis::with_contracts(store),
        None => SimpleStackAnalysis::new(),
    };
    if let Some(net_effect) = simple.analyze_block(body) {
        let inputs = simple.inputs_required();
        let outputs = (inputs as i32 + net_effect) as usize;
        return StackEffectResult::Known { inputs, outputs };
    }

    // Tier 2: Fall back to abstract interpretation
    let ai_result = pre_analyze_procedure(body);

    if ai_result.has_dynamic_stack {
        StackEffectResult::Dynamic {
            min_inputs: ai_result.total_inputs_required,
            reason: ai_result
                .failure_reason
                .unwrap_or_else(|| "dynamic stack effect".to_string()),
        }
    } else {
        // AI succeeded - we know the inputs, but outputs depend on specific execution
        // For now, report as dynamic since we don't track the final stack depth in AI
        StackEffectResult::Dynamic {
            min_inputs: ai_result.total_inputs_required,
            reason: "complex control flow analyzed via abstract interpretation".to_string(),
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// StackLike Trait
// ═══════════════════════════════════════════════════════════════════════════════

/// Trait for types that behave like a stack.
///
/// This provides a unified interface for stack manipulation operations,
/// allowing code reuse between different stack implementations (abstract
/// interpretation, decompiler state, etc.).
pub trait StackLike {
    /// The type of elements stored in the stack.
    type Element: Clone;

    /// Get the current stack depth.
    fn depth(&self) -> usize;

    /// Check if the stack is empty.
    fn is_empty(&self) -> bool {
        self.depth() == 0
    }

    /// Push an element onto the stack.
    fn push(&mut self, elem: Self::Element);

    /// Pop an element from the stack.
    ///
    /// If the stack is empty, implementations should either return a default
    /// value or "discover" a new input.
    fn pop(&mut self) -> Self::Element;

    /// Peek at the element at position n (0 = top).
    fn peek(&self, n: usize) -> Option<&Self::Element>;

    /// Ensure the stack has at least `needed` elements.
    ///
    /// Implementations should expand the stack with appropriate default/input
    /// values if necessary.
    fn ensure_depth(&mut self, needed: usize);

    /// Duplicate the element at position n onto the top.
    fn dup(&mut self, n: usize) {
        self.ensure_depth(n + 1);
        if let Some(elem) = self.peek(n).cloned() {
            self.push(elem);
        }
    }

    /// Swap elements at positions a and b (0 = top).
    fn swap(&mut self, a: usize, b: usize);

    /// Move the element at position n to the top.
    fn movup(&mut self, n: usize);

    /// Move the top element to position n.
    ///
    /// After `movdn(n)`: `[a, b, c, d, ...]` becomes `[b, c, d, ..., a, ...]`
    /// where `a` is now at position n.
    fn movdn(&mut self, n: usize);

    // ─────────────────────────────────────────────────────────────────────────
    // Word operations (4-element groups)
    // ─────────────────────────────────────────────────────────────────────────

    /// Swap two words (4-element groups) at word positions a and b.
    fn swapw(&mut self, word_a: usize, word_b: usize) {
        for i in 0..4 {
            self.swap(word_a * 4 + i, word_b * 4 + i);
        }
    }

    /// Move the word at position n to the top.
    fn movupw(&mut self, word_pos: usize) {
        for _ in 0..4 {
            self.movup(word_pos * 4);
        }
    }

    /// Move the top word to position n.
    fn movdnw(&mut self, word_pos: usize) {
        for _ in 0..4 {
            self.movdn(word_pos * 4);
        }
    }

    /// Duplicate a word (4 elements) at word position onto the top.
    fn dupw(&mut self, word_idx: usize) {
        let base = word_idx * 4;
        // Dup in reverse order to maintain correct stack order
        for i in (0..4).rev() {
            self.dup(base + i);
        }
    }

    /// Reverse the top 4 elements (word).
    fn reversew(&mut self) {
        self.swap(0, 3);
        self.swap(1, 2);
    }

    /// Reverse the top 8 elements (double word).
    fn reversedw(&mut self) {
        for i in 0..4 {
            self.swap(i, 7 - i);
        }
    }

    /// Push N copies of the given element onto the stack.
    fn push_n(&mut self, n: usize, elem: Self::Element)
    where
        Self::Element: Clone,
    {
        for _ in 0..n {
            self.push(elem.clone());
        }
    }

    /// Push N elements of the default value onto the stack.
    fn push_defaults(&mut self, n: usize)
    where
        Self::Element: Default,
    {
        for _ in 0..n {
            self.push(Self::Element::default());
        }
    }

    /// Pop N elements from the stack.
    fn pop_n(&mut self, n: usize) {
        for _ in 0..n {
            self.pop();
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// Stack Manipulation Helper
// ═══════════════════════════════════════════════════════════════════════════════

/// Result of applying a stack manipulation instruction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StackManipResult {
    /// The instruction was a pure stack manipulation and was applied.
    Applied,
    /// The instruction is not a pure stack manipulation (needs special handling).
    NotStackManip,
}

/// Apply pure stack manipulation instructions to a StackLike implementation.
///
/// This handles all dup, swap, movup, movdn, drop instructions and their word variants.
/// Returns `StackManipResult::Applied` if the instruction was handled, or
/// `StackManipResult::NotStackManip` if the instruction requires special handling.
///
/// This function is useful for stack tracking analysis where only the provenance
/// of values matters, not their contents.
pub fn apply_stack_manipulation<S: StackLike>(stack: &mut S, inst: &Instruction) -> StackManipResult
where
    S::Element: Default,
{
    use Instruction::*;
    match inst {
        // Drop operations
        Drop => {
            stack.pop();
            StackManipResult::Applied
        }
        DropW => {
            stack.pop_n(4);
            StackManipResult::Applied
        }

        // Dup operations
        Dup0 => { stack.dup(0); StackManipResult::Applied }
        Dup1 => { stack.dup(1); StackManipResult::Applied }
        Dup2 => { stack.dup(2); StackManipResult::Applied }
        Dup3 => { stack.dup(3); StackManipResult::Applied }
        Dup4 => { stack.dup(4); StackManipResult::Applied }
        Dup5 => { stack.dup(5); StackManipResult::Applied }
        Dup6 => { stack.dup(6); StackManipResult::Applied }
        Dup7 => { stack.dup(7); StackManipResult::Applied }
        Dup8 => { stack.dup(8); StackManipResult::Applied }
        Dup9 => { stack.dup(9); StackManipResult::Applied }
        Dup10 => { stack.dup(10); StackManipResult::Applied }
        Dup11 => { stack.dup(11); StackManipResult::Applied }
        Dup12 => { stack.dup(12); StackManipResult::Applied }
        Dup13 => { stack.dup(13); StackManipResult::Applied }
        Dup14 => { stack.dup(14); StackManipResult::Applied }
        Dup15 => { stack.dup(15); StackManipResult::Applied }

        // DupW operations
        DupW0 => { stack.dupw(0); StackManipResult::Applied }
        DupW1 => { stack.dupw(1); StackManipResult::Applied }
        DupW2 => { stack.dupw(2); StackManipResult::Applied }
        DupW3 => { stack.dupw(3); StackManipResult::Applied }

        // Swap operations
        Swap1 => { stack.swap(0, 1); StackManipResult::Applied }
        Swap2 => { stack.swap(0, 2); StackManipResult::Applied }
        Swap3 => { stack.swap(0, 3); StackManipResult::Applied }
        Swap4 => { stack.swap(0, 4); StackManipResult::Applied }
        Swap5 => { stack.swap(0, 5); StackManipResult::Applied }
        Swap6 => { stack.swap(0, 6); StackManipResult::Applied }
        Swap7 => { stack.swap(0, 7); StackManipResult::Applied }
        Swap8 => { stack.swap(0, 8); StackManipResult::Applied }
        Swap9 => { stack.swap(0, 9); StackManipResult::Applied }
        Swap10 => { stack.swap(0, 10); StackManipResult::Applied }
        Swap11 => { stack.swap(0, 11); StackManipResult::Applied }
        Swap12 => { stack.swap(0, 12); StackManipResult::Applied }
        Swap13 => { stack.swap(0, 13); StackManipResult::Applied }
        Swap14 => { stack.swap(0, 14); StackManipResult::Applied }
        Swap15 => { stack.swap(0, 15); StackManipResult::Applied }

        // SwapW operations
        SwapW1 => { stack.swapw(0, 1); StackManipResult::Applied }
        SwapW2 => { stack.swapw(0, 2); StackManipResult::Applied }
        SwapW3 => { stack.swapw(0, 3); StackManipResult::Applied }

        // SwapDW
        SwapDw => {
            // Swap double words: [0..8] with [8..16]
            for i in 0..8 {
                stack.swap(i, 8 + i);
            }
            StackManipResult::Applied
        }

        // MovUp operations
        MovUp2 => { stack.movup(2); StackManipResult::Applied }
        MovUp3 => { stack.movup(3); StackManipResult::Applied }
        MovUp4 => { stack.movup(4); StackManipResult::Applied }
        MovUp5 => { stack.movup(5); StackManipResult::Applied }
        MovUp6 => { stack.movup(6); StackManipResult::Applied }
        MovUp7 => { stack.movup(7); StackManipResult::Applied }
        MovUp8 => { stack.movup(8); StackManipResult::Applied }
        MovUp9 => { stack.movup(9); StackManipResult::Applied }
        MovUp10 => { stack.movup(10); StackManipResult::Applied }
        MovUp11 => { stack.movup(11); StackManipResult::Applied }
        MovUp12 => { stack.movup(12); StackManipResult::Applied }
        MovUp13 => { stack.movup(13); StackManipResult::Applied }
        MovUp14 => { stack.movup(14); StackManipResult::Applied }
        MovUp15 => { stack.movup(15); StackManipResult::Applied }

        // MovUpW operations
        MovUpW2 => { stack.movupw(2); StackManipResult::Applied }
        MovUpW3 => { stack.movupw(3); StackManipResult::Applied }

        // MovDn operations
        MovDn2 => { stack.movdn(2); StackManipResult::Applied }
        MovDn3 => { stack.movdn(3); StackManipResult::Applied }
        MovDn4 => { stack.movdn(4); StackManipResult::Applied }
        MovDn5 => { stack.movdn(5); StackManipResult::Applied }
        MovDn6 => { stack.movdn(6); StackManipResult::Applied }
        MovDn7 => { stack.movdn(7); StackManipResult::Applied }
        MovDn8 => { stack.movdn(8); StackManipResult::Applied }
        MovDn9 => { stack.movdn(9); StackManipResult::Applied }
        MovDn10 => { stack.movdn(10); StackManipResult::Applied }
        MovDn11 => { stack.movdn(11); StackManipResult::Applied }
        MovDn12 => { stack.movdn(12); StackManipResult::Applied }
        MovDn13 => { stack.movdn(13); StackManipResult::Applied }
        MovDn14 => { stack.movdn(14); StackManipResult::Applied }
        MovDn15 => { stack.movdn(15); StackManipResult::Applied }

        // MovDnW operations
        MovDnW2 => { stack.movdnw(2); StackManipResult::Applied }
        MovDnW3 => { stack.movdnw(3); StackManipResult::Applied }

        // Not a pure stack manipulation (handled separately)
        _ => StackManipResult::NotStackManip,
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// Tests
// ═══════════════════════════════════════════════════════════════════════════════

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_static_effect_arithmetic() {
        assert_eq!(static_effect(&Instruction::Add), Some(StaticEffect::new(2, 1)));
        assert_eq!(static_effect(&Instruction::Sub), Some(StaticEffect::new(2, 1)));
        assert_eq!(static_effect(&Instruction::Neg), Some(StaticEffect::new(1, 1)));
    }

    #[test]
    fn test_static_effect_stack_ops() {
        assert_eq!(static_effect(&Instruction::Drop), Some(StaticEffect::new(1, 0)));
        assert_eq!(static_effect(&Instruction::DropW), Some(StaticEffect::new(4, 0)));
        assert_eq!(static_effect(&Instruction::Dup0), Some(StaticEffect::new(0, 1)));
        assert_eq!(static_effect(&Instruction::Swap1), Some(StaticEffect::new(0, 0)));
    }

    #[test]
    fn test_static_effect_dynamic() {
        assert_eq!(static_effect(&Instruction::DynExec), None);
        assert_eq!(static_effect(&Instruction::DynCall), None);
    }

    #[test]
    fn test_net_effect() {
        assert_eq!(StaticEffect::new(2, 1).net(), -1);
        assert_eq!(StaticEffect::new(1, 2).net(), 1);
        assert_eq!(StaticEffect::new(2, 2).net(), 0);
    }

    #[test]
    fn test_simple_analysis_basic() {
        // Start with some inputs on the stack
        let mut analysis = SimpleStackAnalysis::with_inputs(2);
        assert_eq!(analysis.depth(), 2);

        // Dup: depth 2 -> 3
        assert!(analysis.apply_instruction(&Instruction::Dup0));
        assert_eq!(analysis.depth(), 3);

        // Add: depth 3 -> 2
        assert!(analysis.apply_instruction(&Instruction::Add));
        assert_eq!(analysis.depth(), 2);

        // Drop: depth 2 -> 1
        assert!(analysis.apply_instruction(&Instruction::Drop));
        assert_eq!(analysis.depth(), 1);
    }

    #[test]
    fn test_simple_analysis_inputs() {
        let mut analysis = SimpleStackAnalysis::new();

        // Pop without push: discovers input
        assert!(analysis.apply_instruction(&Instruction::Drop));
        assert_eq!(analysis.depth(), -1);
        assert_eq!(analysis.inputs_required(), 1);

        // Another pop: discovers another input
        assert!(analysis.apply_instruction(&Instruction::Drop));
        assert_eq!(analysis.depth(), -2);
        assert_eq!(analysis.inputs_required(), 2);
    }
}
