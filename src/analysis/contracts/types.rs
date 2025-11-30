//! Contract types for procedure analysis.
//!
//! This module contains the core types for describing procedure behavior:
//! - `ValidationBehavior`: Whether a procedure validates its inputs
//! - `StackEffect`: How a procedure affects the stack
//! - `InputKind`: How a procedure uses each input (value vs address)
//! - `InputSignature`: Detailed information about procedure inputs
//! - `OutputKind`: What a procedure output represents (computed, passthrough, etc.)
//! - `ProcSignature`: Complete input/output signature with provenance
//! - `ProcContract`: Complete contract describing procedure behavior

use tower_lsp::lsp_types::Range;

use crate::symbol_path::SymbolPath;

// ═══════════════════════════════════════════════════════════════════════════
// Input Kind
// ═══════════════════════════════════════════════════════════════════════════

/// Describes how a procedure uses a stack input.
///
/// This is used to track whether inputs are regular values or memory addresses,
/// and if addresses, whether they're read from (input) or written to (output).
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum InputKind {
    /// A regular value consumed from the stack
    #[default]
    Value,
    /// A memory address that will be read from (input parameter)
    InputAddress,
    /// A memory address that will be written to (output parameter)
    OutputAddress,
    /// A memory address that will be both read and written
    InOutAddress,
}

impl InputKind {
    /// Merge two InputKind values (e.g., from different code paths or operations).
    ///
    /// This implements a lattice where:
    /// - Value is the bottom (no address usage)
    /// - InputAddress and OutputAddress are middle
    /// - InOutAddress is the top (both read and write)
    pub fn merge(self, other: InputKind) -> InputKind {
        use InputKind::*;
        match (self, other) {
            // Same kind stays the same
            (Value, Value) => Value,
            (InputAddress, InputAddress) => InputAddress,
            (OutputAddress, OutputAddress) => OutputAddress,
            (InOutAddress, _) | (_, InOutAddress) => InOutAddress,
            // Input + Output = InOut
            (InputAddress, OutputAddress) | (OutputAddress, InputAddress) => InOutAddress,
            // Address usage dominates Value (conservative)
            (InputAddress, Value) | (Value, InputAddress) => InputAddress,
            (OutputAddress, Value) | (Value, OutputAddress) => OutputAddress,
        }
    }

    /// Returns true if this input is used as an output address (will be written to).
    pub fn is_output(&self) -> bool {
        matches!(self, InputKind::OutputAddress | InputKind::InOutAddress)
    }

    /// Returns true if this input is used as an input address (will be read from).
    pub fn is_input_address(&self) -> bool {
        matches!(self, InputKind::InputAddress | InputKind::InOutAddress)
    }

    /// Returns true if this input is used as any kind of address.
    pub fn is_address(&self) -> bool {
        !matches!(self, InputKind::Value)
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Input Signature
// ═══════════════════════════════════════════════════════════════════════════

/// Detailed information about procedure input arguments.
///
/// This tracks how each input to a procedure is used, allowing for:
/// - Suppressing false positives in uninitialized locals analysis
/// - Displaying more informative procedure signatures
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct InputSignature {
    /// Kinds of each input (index 0 = deepest consumed, last = top of stack when called)
    pub kinds: Vec<InputKind>,
}

impl InputSignature {
    /// Create a new input signature with the given number of inputs, all as Value.
    pub fn new(num_inputs: usize) -> Self {
        Self {
            kinds: vec![InputKind::Value; num_inputs],
        }
    }

    /// Get the kind of input at the given position.
    pub fn get(&self, position: usize) -> Option<InputKind> {
        self.kinds.get(position).copied()
    }

    /// Set the kind of input at the given position.
    pub fn set(&mut self, position: usize, kind: InputKind) {
        if position < self.kinds.len() {
            self.kinds[position] = self.kinds[position].merge(kind);
        }
    }

    /// Returns true if the input at the given position is an output address.
    pub fn is_output_address(&self, position: usize) -> bool {
        self.kinds.get(position).map_or(false, |k| k.is_output())
    }

    /// Returns the number of inputs.
    pub fn len(&self) -> usize {
        self.kinds.len()
    }

    /// Returns true if there are no inputs.
    pub fn is_empty(&self) -> bool {
        self.kinds.is_empty()
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Output Kind
// ═══════════════════════════════════════════════════════════════════════════

/// Describes what a procedure output represents.
///
/// This tracks the provenance of each output, allowing callers to understand
/// what values will be on the stack after a procedure returns.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum OutputKind {
    /// A new value computed by the procedure
    #[default]
    Computed,
    /// Passthrough of input at given position (input flows directly to output)
    InputPassthrough { input_pos: usize },
    /// Value loaded from memory (input address was read)
    MemoryRead { from_input: Option<usize> },
    /// Address that was written to (for tracking output parameters)
    WrittenAddress { input_pos: usize },
}

impl OutputKind {
    /// Returns the input position if this output is a passthrough.
    pub fn passthrough_input(&self) -> Option<usize> {
        match self {
            OutputKind::InputPassthrough { input_pos } => Some(*input_pos),
            _ => None,
        }
    }

    /// Returns true if this output represents a value loaded from memory.
    pub fn is_memory_read(&self) -> bool {
        matches!(self, OutputKind::MemoryRead { .. })
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Procedure Signature
// ═══════════════════════════════════════════════════════════════════════════

/// Complete procedure signature with input and output semantics.
///
/// This combines input kinds (how inputs are used) with output kinds
/// (what outputs represent), providing full information for inter-procedural
/// analysis and user-facing documentation.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ProcSignature {
    /// How each input is used (index 0 = first/deepest consumed)
    pub inputs: Vec<InputKind>,
    /// What each output represents (index 0 = first/deepest produced)
    pub outputs: Vec<OutputKind>,
}

impl ProcSignature {
    /// Create a new signature with the given input and output counts.
    pub fn new(num_inputs: usize, num_outputs: usize) -> Self {
        Self {
            inputs: vec![InputKind::Value; num_inputs],
            outputs: vec![OutputKind::Computed; num_outputs],
        }
    }

    /// Create a signature from an existing InputSignature.
    pub fn from_input_signature(sig: InputSignature, num_outputs: usize) -> Self {
        Self {
            inputs: sig.kinds,
            outputs: vec![OutputKind::Computed; num_outputs],
        }
    }

    /// Get the kind of input at the given position.
    pub fn get_input(&self, position: usize) -> Option<InputKind> {
        self.inputs.get(position).copied()
    }

    /// Set the kind of input at the given position (merges with existing).
    pub fn set_input(&mut self, position: usize, kind: InputKind) {
        if position < self.inputs.len() {
            self.inputs[position] = self.inputs[position].merge(kind);
        }
    }

    /// Get the kind of output at the given position.
    pub fn get_output(&self, position: usize) -> Option<OutputKind> {
        self.outputs.get(position).copied()
    }

    /// Set the kind of output at the given position.
    pub fn set_output(&mut self, position: usize, kind: OutputKind) {
        if position < self.outputs.len() {
            self.outputs[position] = kind;
        }
    }

    /// Returns true if the input at the given position is an output address.
    pub fn is_output_address(&self, position: usize) -> bool {
        self.inputs.get(position).map_or(false, |k| k.is_output())
    }

    /// Returns true if the input at the given position is an input address.
    pub fn is_input_address(&self, position: usize) -> bool {
        self.inputs.get(position).map_or(false, |k| k.is_input_address())
    }

    /// Get the number of inputs.
    pub fn num_inputs(&self) -> usize {
        self.inputs.len()
    }

    /// Get the number of outputs.
    pub fn num_outputs(&self) -> usize {
        self.outputs.len()
    }

    /// Convert to an InputSignature (for backwards compatibility).
    pub fn to_input_signature(&self) -> InputSignature {
        InputSignature {
            kinds: self.inputs.clone(),
        }
    }

    /// Format the signature for display in hover tooltips.
    ///
    /// Returns a human-readable signature like:
    /// - `(in a: felt, in b: felt) -> (r: felt)` for simple value procedures
    /// - `(in src: *felt, out dst: *felt) -> ()` for procedures with address arguments
    ///
    /// Input annotations:
    /// - `in`: input value or address read from
    /// - `out`: address written to
    /// - `inout`: address both read and written
    ///
    /// Types:
    /// - `felt`: field element value
    /// - `*felt`: pointer/address to memory
    pub fn format_for_display(&self, proc_name: &str) -> String {
        let inputs: Vec<String> = self
            .inputs
            .iter()
            .enumerate()
            .map(|(i, kind)| {
                let (dir, ty) = match kind {
                    InputKind::Value => ("in", "felt"),
                    InputKind::InputAddress => ("in", "*felt"),
                    InputKind::OutputAddress => ("out", "*felt"),
                    InputKind::InOutAddress => ("inout", "*felt"),
                };
                format!("{dir} a_{i}: {ty}")
            })
            .collect();

        let outputs: Vec<String> = self
            .outputs
            .iter()
            .enumerate()
            .map(|(i, kind)| {
                let ty = match kind {
                    OutputKind::Computed => "felt",
                    OutputKind::InputPassthrough { input_pos } => {
                        // Passthrough preserves the input type
                        match self.inputs.get(*input_pos) {
                            Some(InputKind::Value) => "felt",
                            Some(_) => "*felt",
                            None => "felt",
                        }
                    }
                    OutputKind::MemoryRead { .. } => "felt",
                    OutputKind::WrittenAddress { .. } => "*felt",
                };
                format!("r_{i}: {ty}")
            })
            .collect();

        format!("{proc_name}({}) -> ({})", inputs.join(", "), outputs.join(", "))
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Validation Behavior
// ═══════════════════════════════════════════════════════════════════════════

/// Describes whether a procedure validates its inputs.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub enum ValidationBehavior {
    /// Procedure does not validate inputs
    #[default]
    None,
    /// Procedure validates inputs (has u32assert* at start)
    ValidatesU32,
}

// ═══════════════════════════════════════════════════════════════════════════
// Stack Effect
// ═══════════════════════════════════════════════════════════════════════════

/// Stack effect of a procedure - how it changes the stack depth.
///
/// # Convergence
///
/// Stack effect inference uses worklist-based iteration that converges based on
/// **propagation termination**, not lattice monotonicity:
///
/// 1. A procedure's effect can only change if a callee's effect changes
/// 2. When a callee stabilizes, its callers can stabilize
/// 3. Finite procedures + finite effects = guaranteed termination
///
/// This is different from traditional dataflow analysis where monotonicity
/// guarantees convergence. Here, convergence comes from the bounded propagation
/// of information through the call graph.
///
/// For valid Miden code, all paths through a procedure have the same stack
/// effect (enforced by the assembler), so effects don't "oscillate" - they
/// simply refine as callee information becomes available.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub enum StackEffect {
    /// Stack effect is completely unknown
    #[default]
    Unknown,
    /// Inputs are known but outputs are unknown (e.g., due to while loops with unknown iteration count)
    KnownInputs { inputs: usize },
    /// Known stack effect: (inputs_consumed, outputs_produced)
    /// Net change = outputs_produced - inputs_consumed
    Known { inputs: usize, outputs: usize },
}

impl StackEffect {
    /// Get the number of inputs if known.
    pub fn inputs(&self) -> Option<usize> {
        match self {
            StackEffect::Unknown => None,
            StackEffect::KnownInputs { inputs } => Some(*inputs),
            StackEffect::Known { inputs, .. } => Some(*inputs),
        }
    }

    /// Get the number of outputs if known.
    pub fn outputs(&self) -> Option<usize> {
        match self {
            StackEffect::Unknown | StackEffect::KnownInputs { .. } => None,
            StackEffect::Known { outputs, .. } => Some(*outputs),
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Procedure Contract
// ═══════════════════════════════════════════════════════════════════════════

/// Contract describing expected behavior of a procedure.
#[derive(Clone, Debug)]
pub struct ProcContract {
    /// Full path of the procedure
    pub path: SymbolPath,
    /// Whether the procedure validates its inputs
    pub validates: ValidationBehavior,
    /// Whether the procedure uses u32 operations (implies u32 input requirements)
    pub uses_u32_ops: bool,
    /// Whether the procedure reads from advice stack
    pub reads_advice: bool,
    /// Whether the procedure performs Merkle operations
    pub uses_merkle_ops: bool,
    /// Stack effect of this procedure
    pub stack_effect: StackEffect,
    /// Full procedure signature (inputs and outputs with provenance)
    pub signature: Option<ProcSignature>,
    /// Location of the procedure definition
    pub definition_range: Option<Range>,
}

impl ProcContract {
    /// Returns true if this procedure validates its inputs.
    pub fn validates_inputs(&self) -> bool {
        matches!(self.validates, ValidationBehavior::ValidatesU32)
    }

    /// Returns true if this procedure requires u32 inputs based on its operations.
    pub fn requires_u32_inputs(&self) -> bool {
        self.uses_u32_ops && !self.validates_inputs()
    }

    /// Returns true if the input at the given position is an output address.
    ///
    /// This is useful for suppressing false positives in uninitialized locals
    /// analysis when a local's address is passed to a procedure that writes to it.
    pub fn is_input_output_address(&self, position: usize) -> bool {
        self.signature
            .as_ref()
            .map_or(false, |sig| sig.is_output_address(position))
    }

    /// Returns true if the input at the given position is an input address.
    pub fn is_input_address(&self, position: usize) -> bool {
        self.signature
            .as_ref()
            .map_or(false, |sig| sig.is_input_address(position))
    }

    /// Get the input kind for a specific position if known.
    pub fn get_input_kind(&self, position: usize) -> Option<InputKind> {
        self.signature.as_ref().and_then(|sig| sig.get_input(position))
    }

    /// Get the output kind for a specific position if known.
    pub fn get_output_kind(&self, position: usize) -> Option<OutputKind> {
        self.signature.as_ref().and_then(|sig| sig.get_output(position))
    }

    /// Get the full signature if available.
    pub fn get_signature(&self) -> Option<&ProcSignature> {
        self.signature.as_ref()
    }

    /// Format the signature for display in hover tooltips.
    ///
    /// Returns a human-readable signature string. If detailed signature is available,
    /// shows argument usage (value vs address). Otherwise falls back to basic stack effect.
    ///
    /// The `proc_name` parameter should be the procedure name with visibility prefix
    /// (e.g., "pub foo" or "export.bar").
    pub fn format_signature_for_display(&self, proc_name: &str) -> Option<String> {
        // If we have a detailed signature, use it
        if let Some(sig) = &self.signature {
            return Some(sig.format_for_display(proc_name));
        }

        // Otherwise, fall back to basic stack effect
        match &self.stack_effect {
            StackEffect::Known { inputs, outputs } => {
                let input_str = (0..*inputs)
                    .map(|i| format!("in a_{i}: felt"))
                    .collect::<Vec<_>>()
                    .join(", ");
                let output_str = (0..*outputs)
                    .map(|i| format!("r_{i}: felt"))
                    .collect::<Vec<_>>()
                    .join(", ");
                Some(format!("{proc_name}({input_str}) -> ({output_str})"))
            }
            StackEffect::KnownInputs { inputs } => {
                let input_str = (0..*inputs)
                    .map(|i| format!("in a_{i}: felt"))
                    .collect::<Vec<_>>()
                    .join(", ");
                Some(format!("{proc_name}({input_str}) -> (?)"))
            }
            StackEffect::Unknown => None,
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Tests
// ═══════════════════════════════════════════════════════════════════════════

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_input_kind_merge() {
        use InputKind::*;

        // Same kind stays the same
        assert_eq!(Value.merge(Value), Value);
        assert_eq!(InputAddress.merge(InputAddress), InputAddress);
        assert_eq!(OutputAddress.merge(OutputAddress), OutputAddress);
        assert_eq!(InOutAddress.merge(InOutAddress), InOutAddress);

        // Input + Output = InOut
        assert_eq!(InputAddress.merge(OutputAddress), InOutAddress);
        assert_eq!(OutputAddress.merge(InputAddress), InOutAddress);

        // InOut dominates everything
        assert_eq!(InOutAddress.merge(Value), InOutAddress);
        assert_eq!(Value.merge(InOutAddress), InOutAddress);
        assert_eq!(InOutAddress.merge(InputAddress), InOutAddress);
        assert_eq!(InOutAddress.merge(OutputAddress), InOutAddress);

        // Address usage dominates Value
        assert_eq!(InputAddress.merge(Value), InputAddress);
        assert_eq!(Value.merge(InputAddress), InputAddress);
        assert_eq!(OutputAddress.merge(Value), OutputAddress);
        assert_eq!(Value.merge(OutputAddress), OutputAddress);
    }

    #[test]
    fn test_input_kind_predicates() {
        assert!(!InputKind::Value.is_output());
        assert!(!InputKind::InputAddress.is_output());
        assert!(InputKind::OutputAddress.is_output());
        assert!(InputKind::InOutAddress.is_output());

        assert!(!InputKind::Value.is_address());
        assert!(InputKind::InputAddress.is_address());
        assert!(InputKind::OutputAddress.is_address());
        assert!(InputKind::InOutAddress.is_address());
    }

    #[test]
    fn test_input_signature_new() {
        let sig = InputSignature::new(3);
        assert_eq!(sig.len(), 3);
        assert_eq!(sig.get(0), Some(InputKind::Value));
        assert_eq!(sig.get(1), Some(InputKind::Value));
        assert_eq!(sig.get(2), Some(InputKind::Value));
        assert_eq!(sig.get(3), None);
    }

    #[test]
    fn test_input_signature_set() {
        let mut sig = InputSignature::new(3);

        sig.set(0, InputKind::OutputAddress);
        assert_eq!(sig.get(0), Some(InputKind::OutputAddress));

        // Setting again should merge
        sig.set(0, InputKind::InputAddress);
        assert_eq!(sig.get(0), Some(InputKind::InOutAddress));

        // Setting out of bounds should be ignored
        sig.set(10, InputKind::OutputAddress);
        assert_eq!(sig.get(10), None);
    }

    #[test]
    fn test_input_signature_is_output_address() {
        let mut sig = InputSignature::new(3);
        sig.set(1, InputKind::OutputAddress);

        assert!(!sig.is_output_address(0));
        assert!(sig.is_output_address(1));
        assert!(!sig.is_output_address(2));
        assert!(!sig.is_output_address(10)); // Out of bounds
    }

    #[test]
    fn test_proc_contract_input_helpers() {
        let contract = ProcContract {
            path: SymbolPath::new("::test::proc"),
            validates: ValidationBehavior::None,
            uses_u32_ops: false,
            reads_advice: false,
            uses_merkle_ops: false,
            stack_effect: StackEffect::Known { inputs: 3, outputs: 1 },
            signature: Some(ProcSignature {
                inputs: vec![InputKind::Value, InputKind::OutputAddress, InputKind::InputAddress],
                outputs: vec![OutputKind::Computed],
            }),
            definition_range: None,
        };

        assert!(!contract.is_input_output_address(0));
        assert!(contract.is_input_output_address(1));
        assert!(!contract.is_input_output_address(2)); // InputAddress is not output

        assert_eq!(contract.get_input_kind(0), Some(InputKind::Value));
        assert_eq!(contract.get_input_kind(1), Some(InputKind::OutputAddress));
        assert_eq!(contract.get_input_kind(2), Some(InputKind::InputAddress));
        assert_eq!(contract.get_input_kind(3), None);
    }

    #[test]
    fn test_proc_contract_no_signature() {
        let contract = ProcContract {
            path: SymbolPath::new("::test::proc"),
            validates: ValidationBehavior::None,
            uses_u32_ops: false,
            reads_advice: false,
            uses_merkle_ops: false,
            stack_effect: StackEffect::Unknown,
            signature: None,
            definition_range: None,
        };

        assert!(!contract.is_input_output_address(0));
        assert_eq!(contract.get_input_kind(0), None);
    }

    #[test]
    fn test_proc_signature_basic() {
        let sig = ProcSignature::new(2, 1);
        assert_eq!(sig.num_inputs(), 2);
        assert_eq!(sig.num_outputs(), 1);
        assert_eq!(sig.get_input(0), Some(InputKind::Value));
        assert_eq!(sig.get_output(0), Some(OutputKind::Computed));
    }

    #[test]
    fn test_proc_signature_set_input_output() {
        let mut sig = ProcSignature::new(2, 2);

        sig.set_input(0, InputKind::OutputAddress);
        assert!(sig.is_output_address(0));
        assert!(!sig.is_input_address(0));

        sig.set_input(1, InputKind::InputAddress);
        assert!(!sig.is_output_address(1));
        assert!(sig.is_input_address(1));

        sig.set_output(0, OutputKind::InputPassthrough { input_pos: 1 });
        assert_eq!(sig.get_output(0), Some(OutputKind::InputPassthrough { input_pos: 1 }));
    }

    #[test]
    fn test_output_kind_helpers() {
        assert_eq!(OutputKind::Computed.passthrough_input(), None);
        assert_eq!(OutputKind::InputPassthrough { input_pos: 2 }.passthrough_input(), Some(2));

        assert!(!OutputKind::Computed.is_memory_read());
        assert!(OutputKind::MemoryRead { from_input: Some(0) }.is_memory_read());
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Stack Effect Tests
    // ═══════════════════════════════════════════════════════════════════════════

    #[test]
    fn test_stack_effect_accessors() {
        assert_eq!(StackEffect::Unknown.inputs(), None);
        assert_eq!(StackEffect::Unknown.outputs(), None);

        let known_inputs = StackEffect::KnownInputs { inputs: 3 };
        assert_eq!(known_inputs.inputs(), Some(3));
        assert_eq!(known_inputs.outputs(), None);

        let known = StackEffect::Known { inputs: 2, outputs: 1 };
        assert_eq!(known.inputs(), Some(2));
        assert_eq!(known.outputs(), Some(1));
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Signature Formatting Tests
    // ═══════════════════════════════════════════════════════════════════════════

    #[test]
    fn test_proc_signature_format_simple_values() {
        let sig = ProcSignature::new(2, 1);
        assert_eq!(
            sig.format_for_display("pub proc foo"),
            "pub proc foo(in a_0: felt, in a_1: felt) -> (r_0: felt)"
        );
    }

    #[test]
    fn test_proc_signature_format_empty() {
        let sig = ProcSignature::new(0, 0);
        assert_eq!(sig.format_for_display("export.bar"), "export.bar() -> ()");
    }

    #[test]
    fn test_proc_signature_format_with_addresses() {
        let sig = ProcSignature {
            inputs: vec![
                InputKind::Value,
                InputKind::InputAddress,
                InputKind::OutputAddress,
            ],
            outputs: vec![OutputKind::Computed],
        };
        assert_eq!(
            sig.format_for_display("pub proc copy"),
            "pub proc copy(in a_0: felt, in a_1: *felt, out a_2: *felt) -> (r_0: felt)"
        );
    }

    #[test]
    fn test_proc_signature_format_with_inout_address() {
        let sig = ProcSignature {
            inputs: vec![InputKind::InOutAddress],
            outputs: vec![OutputKind::Computed],
        };
        assert_eq!(
            sig.format_for_display("pub proc update"),
            "pub proc update(inout a_0: *felt) -> (r_0: felt)"
        );
    }

    #[test]
    fn test_proc_signature_format_with_passthrough() {
        let sig = ProcSignature {
            inputs: vec![InputKind::Value, InputKind::Value],
            outputs: vec![
                OutputKind::Computed,
                OutputKind::InputPassthrough { input_pos: 0 },
            ],
        };
        assert_eq!(
            sig.format_for_display("pub proc dup_and_add"),
            "pub proc dup_and_add(in a_0: felt, in a_1: felt) -> (r_0: felt, r_1: felt)"
        );
    }

    #[test]
    fn test_proc_signature_format_with_memory_read() {
        let sig = ProcSignature {
            inputs: vec![InputKind::InputAddress],
            outputs: vec![OutputKind::MemoryRead { from_input: Some(0) }],
        };
        assert_eq!(
            sig.format_for_display("pub proc load"),
            "pub proc load(in a_0: *felt) -> (r_0: felt)"
        );
    }

    #[test]
    fn test_proc_signature_format_with_address_passthrough() {
        let sig = ProcSignature {
            inputs: vec![InputKind::OutputAddress],
            outputs: vec![OutputKind::InputPassthrough { input_pos: 0 }],
        };
        assert_eq!(
            sig.format_for_display("pub proc get_ptr"),
            "pub proc get_ptr(out a_0: *felt) -> (r_0: *felt)"
        );
    }

    #[test]
    fn test_proc_contract_format_with_signature() {
        let contract = ProcContract {
            path: SymbolPath::new("::test::store"),
            validates: ValidationBehavior::None,
            uses_u32_ops: false,
            reads_advice: false,
            uses_merkle_ops: false,
            stack_effect: StackEffect::Known { inputs: 2, outputs: 1 },
            signature: Some(ProcSignature {
                inputs: vec![InputKind::Value, InputKind::OutputAddress],
                outputs: vec![OutputKind::Computed],
            }),
            definition_range: None,
        };
        assert_eq!(
            contract.format_signature_for_display("pub proc store"),
            Some("pub proc store(in a_0: felt, out a_1: *felt) -> (r_0: felt)".to_string())
        );
    }

    #[test]
    fn test_proc_contract_format_fallback_to_stack_effect() {
        let contract = ProcContract {
            path: SymbolPath::new("::test::proc"),
            validates: ValidationBehavior::None,
            uses_u32_ops: false,
            reads_advice: false,
            uses_merkle_ops: false,
            stack_effect: StackEffect::Known { inputs: 3, outputs: 2 },
            signature: None,
            definition_range: None,
        };
        assert_eq!(
            contract.format_signature_for_display("pub proc compute"),
            Some("pub proc compute(in a_0: felt, in a_1: felt, in a_2: felt) -> (r_0: felt, r_1: felt)".to_string())
        );
    }

    #[test]
    fn test_proc_contract_format_known_inputs_only() {
        let contract = ProcContract {
            path: SymbolPath::new("::test::proc"),
            validates: ValidationBehavior::None,
            uses_u32_ops: false,
            reads_advice: false,
            uses_merkle_ops: false,
            stack_effect: StackEffect::KnownInputs { inputs: 2 },
            signature: None,
            definition_range: None,
        };
        assert_eq!(
            contract.format_signature_for_display("pub proc dynamic"),
            Some("pub proc dynamic(in a_0: felt, in a_1: felt) -> (?)".to_string())
        );
    }

    #[test]
    fn test_proc_contract_format_unknown_effect() {
        let contract = ProcContract {
            path: SymbolPath::new("::test::proc"),
            validates: ValidationBehavior::None,
            uses_u32_ops: false,
            reads_advice: false,
            uses_merkle_ops: false,
            stack_effect: StackEffect::Unknown,
            signature: None,
            definition_range: None,
        };
        assert_eq!(contract.format_signature_for_display("pub proc unknown"), None);
    }
}
