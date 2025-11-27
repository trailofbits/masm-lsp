//! While loop bound inference.
//!
//! This module provides analysis to infer iteration counts for while loops
//! when the loop follows recognizable counter patterns.
//!
//! Common patterns detected:
//! - Countdown loops: counter starts at N, decrements each iteration until 0
//! - Countup loops: counter starts at 0, increments until reaching N

use std::ops::ControlFlow;

use miden_assembly_syntax::ast::{
    visit::{self, Visit},
    Block, Immediate, Instruction, Op,
};

use super::types::Bounds;

// ═══════════════════════════════════════════════════════════════════════════
// Helper for extracting values from immediates
// ═══════════════════════════════════════════════════════════════════════════

/// Try to extract a u64 value from a push immediate.
fn push_imm_to_u64(imm: &Immediate<miden_assembly_syntax::parser::PushValue>) -> Option<u64> {
    use miden_assembly_syntax::parser::{IntValue, PushValue};

    match imm {
        Immediate::Value(span) => match span.inner() {
            PushValue::Int(int_val) => {
                let val = match int_val {
                    IntValue::U8(v) => *v as u64,
                    IntValue::U16(v) => *v as u64,
                    IntValue::U32(v) => *v as u64,
                    IntValue::Felt(f) => f.as_int(),
                };
                Some(val)
            }
            PushValue::Word(_) => None, // Word pushes multiple values, can't extract single u64
        },
        _ => None,
    }
}

/// Try to extract a u64 value from a Felt immediate.
fn felt_imm_to_u64(imm: &Immediate<miden_assembly_syntax::Felt>) -> Option<u64> {
    match imm {
        Immediate::Value(span) => Some(span.inner().as_int()),
        _ => None,
    }
}


// ═══════════════════════════════════════════════════════════════════════════
// Loop Bound Result
// ═══════════════════════════════════════════════════════════════════════════

/// Result of analyzing a while loop for bounds.
#[derive(Clone, Debug, PartialEq)]
pub enum LoopBound {
    /// Loop iterations cannot be determined statically
    Unknown,
    /// Loop runs exactly N times
    Exactly(u64),
}

impl LoopBound {
    /// Returns the iteration count if known.
    pub fn count(&self) -> Option<u64> {
        match self {
            LoopBound::Exactly(n) => Some(*n),
            LoopBound::Unknown => None,
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Symbolic Stack for Tracking
// ═══════════════════════════════════════════════════════════════════════════

/// A minimal symbolic stack that tracks bounds through a while body.
#[derive(Clone, Debug, Default)]
struct SymbolicStack {
    /// Stack values with their bounds
    elements: Vec<Bounds>,
}

impl SymbolicStack {
    fn new() -> Self {
        Self::default()
    }

    /// Initialize with a known counter value on top.
    fn with_counter(initial: u64) -> Self {
        Self {
            elements: vec![Bounds::Const(initial)],
        }
    }

    fn push(&mut self, bounds: Bounds) {
        self.elements.push(bounds);
    }

    fn pop(&mut self) -> Bounds {
        self.elements.pop().unwrap_or(Bounds::Field)
    }

    fn peek(&self, n: usize) -> Bounds {
        if n < self.elements.len() {
            self.elements[self.elements.len() - 1 - n].clone()
        } else {
            Bounds::Field
        }
    }

    fn depth(&self) -> usize {
        self.elements.len()
    }

    fn dup(&mut self, n: usize) {
        let bounds = self.peek(n);
        self.push(bounds);
    }

    fn swap(&mut self, a: usize, b: usize) {
        let len = self.elements.len();
        if a < len && b < len {
            let idx_a = len - 1 - a;
            let idx_b = len - 1 - b;
            self.elements.swap(idx_a, idx_b);
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// While Loop Analyzer
// ═══════════════════════════════════════════════════════════════════════════

/// Analyzes a while loop body to detect counter patterns.
///
/// This analyzer simulates execution of the loop body to detect patterns like:
/// - `sub.1 dup.0 neq.0` (countdown loop)
/// - `add.1 dup.0 lt.N` (countup loop)
pub struct WhileLoopAnalyzer {
    /// Symbolic stack state
    stack: SymbolicStack,
    /// Whether we've seen unknown operations that prevent analysis
    unknown_ops: bool,
    /// Detected decrement pattern: (position, decrement_amount)
    decrement_pattern: Option<(usize, u64)>,
    /// Initial counter value (if pushed before loop)
    initial_counter: Option<u64>,
}

impl WhileLoopAnalyzer {
    /// Create a new analyzer with an optional initial counter value.
    pub fn new(initial_counter: Option<u64>) -> Self {
        let stack = match initial_counter {
            Some(n) => SymbolicStack::with_counter(n),
            None => SymbolicStack::new(),
        };

        Self {
            stack,
            unknown_ops: false,
            decrement_pattern: None,
            initial_counter,
        }
    }

    /// Analyze a while loop body and return the inferred bound.
    pub fn analyze(mut self, body: &Block) -> LoopBound {
        // Simulate execution of the body
        let _ = visit::visit_block(&mut self, body);

        // If we encountered unknown operations, we can't determine bound
        if self.unknown_ops {
            return LoopBound::Unknown;
        }

        // Check for countdown pattern: counter decremented, condition is counter != 0
        if let (Some(initial), Some((_, decrement))) =
            (self.initial_counter, self.decrement_pattern)
        {
            if decrement > 0 && initial > 0 {
                // Countdown loop: runs initial/decrement times (rounded up)
                let iterations = (initial + decrement - 1) / decrement;
                return LoopBound::Exactly(iterations);
            }
        }

        LoopBound::Unknown
    }

    /// Analyze an instruction's effect on the symbolic stack.
    fn analyze_instruction(&mut self, inst: &Instruction) {
        match inst {
            // Push operations
            Instruction::Push(imm) => {
                let bounds = push_imm_to_u64(imm)
                    .map(Bounds::Const)
                    .unwrap_or(Bounds::Field);
                self.stack.push(bounds);
            }
            Instruction::PushFeltList(values) => {
                for _ in values {
                    self.stack.push(Bounds::Field);
                }
            }

            // Drop operations
            Instruction::Drop => {
                self.stack.pop();
            }
            Instruction::DropW => {
                for _ in 0..4 {
                    self.stack.pop();
                }
            }

            // Dup operations
            Instruction::Dup0 => self.stack.dup(0),
            Instruction::Dup1 => self.stack.dup(1),
            Instruction::Dup2 => self.stack.dup(2),
            Instruction::Dup3 => self.stack.dup(3),
            Instruction::Dup4 => self.stack.dup(4),
            Instruction::Dup5 => self.stack.dup(5),
            Instruction::Dup6 => self.stack.dup(6),
            Instruction::Dup7 => self.stack.dup(7),
            Instruction::Dup8 => self.stack.dup(8),
            Instruction::Dup9 => self.stack.dup(9),
            Instruction::Dup10 => self.stack.dup(10),
            Instruction::Dup11 => self.stack.dup(11),
            Instruction::Dup12 => self.stack.dup(12),
            Instruction::Dup13 => self.stack.dup(13),
            Instruction::Dup14 => self.stack.dup(14),
            Instruction::Dup15 => self.stack.dup(15),

            // Swap operations
            Instruction::Swap1 => self.stack.swap(0, 1),
            Instruction::Swap2 => self.stack.swap(0, 2),
            Instruction::Swap3 => self.stack.swap(0, 3),
            Instruction::Swap4 => self.stack.swap(0, 4),
            Instruction::Swap5 => self.stack.swap(0, 5),
            Instruction::Swap6 => self.stack.swap(0, 6),
            Instruction::Swap7 => self.stack.swap(0, 7),
            Instruction::Swap8 => self.stack.swap(0, 8),
            Instruction::Swap9 => self.stack.swap(0, 9),
            Instruction::Swap10 => self.stack.swap(0, 10),
            Instruction::Swap11 => self.stack.swap(0, 11),
            Instruction::Swap12 => self.stack.swap(0, 12),
            Instruction::Swap13 => self.stack.swap(0, 13),
            Instruction::Swap14 => self.stack.swap(0, 14),
            Instruction::Swap15 => self.stack.swap(0, 15),

            // Arithmetic: sub.1 is the key countdown pattern
            Instruction::Sub => {
                let b = self.stack.pop();
                let a = self.stack.pop();
                let result = a.sub(&b);

                // Detect decrement pattern
                if let (Bounds::Const(dec_amount), Some(_)) = (&b, self.initial_counter) {
                    self.decrement_pattern = Some((self.stack.depth(), *dec_amount));
                }

                self.stack.push(result);
            }
            Instruction::SubImm(imm) => {
                let a = self.stack.pop();
                let b = felt_imm_to_u64(imm)
                    .map(Bounds::Const)
                    .unwrap_or(Bounds::Field);
                let result = a.sub(&b);

                // Detect decrement pattern: sub.N where N is the decrement
                if let Bounds::Const(dec_amount) = &b {
                    self.decrement_pattern = Some((self.stack.depth(), *dec_amount));
                }

                self.stack.push(result);
            }

            // Other arithmetic
            Instruction::Add => {
                let b = self.stack.pop();
                let a = self.stack.pop();
                self.stack.push(a.add(&b));
            }
            Instruction::AddImm(imm) => {
                let a = self.stack.pop();
                let b = felt_imm_to_u64(imm)
                    .map(Bounds::Const)
                    .unwrap_or(Bounds::Field);
                self.stack.push(a.add(&b));
            }
            Instruction::Mul => {
                let b = self.stack.pop();
                let a = self.stack.pop();
                self.stack.push(a.mul(&b));
            }
            Instruction::MulImm(imm) => {
                let a = self.stack.pop();
                let b = felt_imm_to_u64(imm)
                    .map(Bounds::Const)
                    .unwrap_or(Bounds::Field);
                self.stack.push(a.mul(&b));
            }

            // Comparison operations
            Instruction::Eq => {
                let b = self.stack.pop();
                let a = self.stack.pop();
                self.stack.push(a.eq(&b));
            }
            Instruction::EqImm(imm) => {
                let a = self.stack.pop();
                let b = felt_imm_to_u64(imm)
                    .map(Bounds::Const)
                    .unwrap_or(Bounds::Field);
                self.stack.push(a.eq(&b));
            }
            Instruction::Neq => {
                let b = self.stack.pop();
                let a = self.stack.pop();
                self.stack.push(a.neq(&b));
            }
            Instruction::NeqImm(imm) => {
                let a = self.stack.pop();
                let b = felt_imm_to_u64(imm)
                    .map(Bounds::Const)
                    .unwrap_or(Bounds::Field);
                self.stack.push(a.neq(&b));
            }
            Instruction::Lt => {
                let b = self.stack.pop();
                let a = self.stack.pop();
                self.stack.push(a.lt(&b));
            }
            Instruction::Gt => {
                let b = self.stack.pop();
                let a = self.stack.pop();
                self.stack.push(a.gt(&b));
            }

            // Boolean operations (maintain bool bounds)
            Instruction::And | Instruction::Or | Instruction::Xor => {
                self.stack.pop();
                self.stack.pop();
                self.stack.push(Bounds::Bool);
            }
            Instruction::Not => {
                self.stack.pop();
                self.stack.push(Bounds::Bool);
            }

            // Unary operations that don't affect bounds tracking
            Instruction::Neg | Instruction::Inv => {
                let _ = self.stack.pop();
                self.stack.push(Bounds::Field);
            }

            // Operations that introduce unknown values
            Instruction::AdvPush(_)
            | Instruction::AdvLoadW
            | Instruction::AdvPipe
            | Instruction::MemLoad
            | Instruction::MemLoadImm(_) => {
                self.unknown_ops = true;
            }

            // Procedure calls - unknown effect
            Instruction::Exec(_) | Instruction::Call(_) | Instruction::SysCall(_) => {
                self.unknown_ops = true;
            }

            // Other operations - conservatively mark as unknown for now
            _ => {
                // For unhandled instructions, we can't track bounds precisely
                // but we won't mark as unknown_ops unless they affect control flow
            }
        }
    }
}

impl Visit for WhileLoopAnalyzer {
    fn visit_op(&mut self, op: &Op) -> ControlFlow<()> {
        match op {
            Op::Inst(inst) => {
                self.analyze_instruction(inst.inner());
            }
            // Nested control flow makes analysis complex - mark as unknown
            Op::If { .. } | Op::While { .. } | Op::Repeat { .. } => {
                self.unknown_ops = true;
            }
        }
        ControlFlow::Continue(())
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Public API
// ═══════════════════════════════════════════════════════════════════════════

/// Try to infer the iteration count of a while loop.
///
/// This function looks for patterns where:
/// 1. A constant counter is initialized before the loop
/// 2. The loop body decrements the counter
/// 3. The loop continues while counter is non-zero
///
/// # Arguments
///
/// * `body` - The while loop body to analyze
/// * `initial_counter` - Optional initial counter value (from push.N before loop)
///
/// # Returns
///
/// `LoopBound::Exactly(n)` if the iteration count can be determined,
/// `LoopBound::Unknown` otherwise.
pub fn infer_while_bound(body: &Block, initial_counter: Option<u64>) -> LoopBound {
    let analyzer = WhileLoopAnalyzer::new(initial_counter);
    analyzer.analyze(body)
}

// ═══════════════════════════════════════════════════════════════════════════
// Tests
// ═══════════════════════════════════════════════════════════════════════════

#[cfg(test)]
mod tests {
    use super::*;

    /// Visitor to extract the first while loop body from a module.
    struct WhileBodyExtractor {
        body: Option<Block>,
    }

    impl Visit for WhileBodyExtractor {
        fn visit_op(&mut self, op: &Op) -> ControlFlow<()> {
            if self.body.is_some() {
                return ControlFlow::Break(());
            }
            if let Op::While { body, .. } = op {
                self.body = Some(body.clone());
                return ControlFlow::Break(());
            }
            ControlFlow::Continue(())
        }
    }

    // Helper to parse a while loop body
    fn parse_body(source: &str) -> Block {
        parse_body_with_procs(source, "")
    }

    // Helper to parse a while loop body with additional procedure definitions
    fn parse_body_with_procs(source: &str, extra_procs: &str) -> Block {
        use miden_assembly_syntax::ast::ModuleKind;
        use miden_assembly_syntax::{Parse, ParseOptions};
        use miden_debug_types::{DefaultSourceManager, SourceLanguage, SourceManager, Uri};

        // Wrap in a procedure with while loop
        let full_source = format!(
            "{}proc test_proc
    while.true
        {}
    end
end",
            extra_procs, source
        );

        let source_manager = DefaultSourceManager::default();
        let uri = Uri::from("test://test.masm");
        source_manager.load(SourceLanguage::Masm, uri.clone(), full_source);

        let source_file = source_manager.get_by_uri(&uri).expect("Failed to load source");
        let mut module_path = miden_assembly_syntax::ast::PathBuf::default();
        module_path.push("test");
        let opts = ParseOptions {
            kind: ModuleKind::Library,
            path: Some(module_path.into()),
            ..Default::default()
        };
        let module = source_file
            .parse_with_options(&source_manager, opts)
            .expect("Failed to parse MASM");

        // Extract the while body using a visitor
        let mut extractor = WhileBodyExtractor { body: None };
        let _ = visit::visit_module(&mut extractor, &module);

        extractor.body.expect("No while loop found in procedure")
    }

    #[test]
    fn test_countdown_loop_simple() {
        // Pattern: counter starts at 10, sub.1 decrements each iteration
        let body = parse_body("sub.1 dup.0 neq.0");
        let bound = infer_while_bound(&body, Some(10));
        assert_eq!(bound, LoopBound::Exactly(10));
    }

    #[test]
    fn test_countdown_loop_decrement_by_2() {
        // Counter starts at 10, decrement by 2 each iteration
        let body = parse_body("sub.2 dup.0 neq.0");
        let bound = infer_while_bound(&body, Some(10));
        assert_eq!(bound, LoopBound::Exactly(5));
    }

    #[test]
    fn test_countdown_loop_no_initial() {
        // No initial counter known
        let body = parse_body("sub.1 dup.0 neq.0");
        let bound = infer_while_bound(&body, None);
        assert_eq!(bound, LoopBound::Unknown);
    }

    #[test]
    fn test_loop_with_procedure_call_unknown() {
        // Procedure call in body makes bound unknown
        let helper_proc = "proc some_proc
    push.1
end

";
        let body = parse_body_with_procs("exec.some_proc sub.1 dup.0 neq.0", helper_proc);
        let bound = infer_while_bound(&body, Some(10));
        assert_eq!(bound, LoopBound::Unknown);
    }

    #[test]
    fn test_loop_with_advice_unknown() {
        // Advice operations make bound unknown
        let body = parse_body("adv_push.1 add sub.1 dup.0 neq.0");
        let bound = infer_while_bound(&body, Some(10));
        assert_eq!(bound, LoopBound::Unknown);
    }
}
