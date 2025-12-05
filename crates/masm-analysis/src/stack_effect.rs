use std::collections::HashMap;

use miden_assembly_syntax::ast::{Immediate, Instruction};

use crate::analysis::semantics::{semantics_of, InstructionEffect};
use crate::descriptions::{format_push_immediate, format_u32_immediate, format_u8_immediate};
use crate::instruction_docs::get_instruction_info;
use masm_instructions::StackOp;
use masm_instructions::{type_of, InstructionType};

/// Trait for rendering stack-effect strings for instructions.
pub trait FormatStackEffect {
    /// Returns a formatted stack-effect string, e.g.
    /// `[a, b, ...] → [b <= a, ...] (net effect: -1)`.
    fn format_stack_effect(&self) -> Option<String>;
}

impl FormatStackEffect for Instruction {
    fn format_stack_effect(&self) -> Option<String> {
        let semantics = semantics_of(self);
        if let Some(label) = format_by_effect(self, semantics) {
            return Some(label);
        }

        // Fall back to semantics counts when available.
        if let Some(sem) = semantics {
            let before = format_generic_shape(sem.pops(), "x");
            let after = format_generic_shape(sem.pushes(), "r");
            return Some(format_change(&before, &after, Some(sem.net_effect())));
        }

        // Fall back to instruction metadata (stack_input/stack_output)
        let rendered = self.to_string();
        let info = get_instruction_info(&rendered);
        if let Some(info) = info {
            let (input, output) = canonicalize_shapes(info.stack_input, info.stack_output);
            return Some(format_change(
                &input,
                &output,
                semantics.map(|s| s.net_effect()),
            ));
        }

        None
    }
}

/// Format instructions with known effects into before/after stack strings.
fn format_by_effect(inst: &Instruction, semantics: Option<InstructionEffect>) -> Option<String> {
    let net_effect = semantics.map(|s| s.net_effect());

    if let Some(op) = StackOp::of(inst) {
        return format_stack_op(op, net_effect?);
    }

    if let Some(count) = push_count(inst) {
        return format_push(inst, count, net_effect?);
    }

    if let Some(count) = drop_count(inst) {
        return format_drop(inst, count, net_effect?);
    }

    if is_memory(inst) {
        // Prefer semantics counts so memory ops reflect overrides.
        if let Some(sem) = semantics {
            return Some(format_stack_change_from_counts(sem.pops(), sem.pushes()));
        }
        return None; // fall back to metadata when semantics are unknown
    }

    semantics.and_then(|sem| format_static(inst, sem))
}

fn push_count(inst: &Instruction) -> Option<usize> {
    use Instruction::*;
    match inst {
        Push(_) => Some(1),
        PushFeltList(values) => Some(values.len()),
        AdvPush(n) => {
            let count = match n {
                Immediate::Value(v) => v.into_inner() as usize,
                _ => 1, // Unknown count, conservatively render a single push
            };
            Some(count)
        }
        PadW => Some(4),
        PushSlice(_, range) => Some(range.len()),
        _ => None,
    }
}

fn drop_count(inst: &Instruction) -> Option<usize> {
    use Instruction::*;
    match inst {
        Drop => Some(1),
        DropW => Some(4),
        _ => None,
    }
}

fn is_memory(inst: &Instruction) -> bool {
    matches!(
        type_of(inst),
        InstructionType::Memory(_) | InstructionType::Local(_)
    )
}

/// Try to format common static operations (arithmetic, comparisons, etc.).
fn format_static(inst: &Instruction, semantics: InstructionEffect) -> Option<String> {
    if let Some(label) = format_immediate_instruction(inst, Some(semantics.net_effect())) {
        return Some(label);
    }

    let net_effect = semantics.net_effect();
    if let Some(label) = format_u32_instruction(inst, semantics) {
        return Some(label);
    }
    let rendered = inst.to_string();

    let base = rendered
        .split('.')
        .next()
        .unwrap_or_else(|| rendered.as_str());
    match base {
        "add" | "sub" | "mul" | "div" | "and" | "or" | "xor" | "eq" | "neq" | "lt" | "lte"
        | "gt" | "gte"
            if semantics.pops() >= 2 && semantics.pushes() >= 1 =>
        {
            Some(binary_op(op_symbol(base), net_effect))
        }
        "neg" => Some(unary_op("-a", net_effect)),
        "inv" => Some(unary_op("a^{-1}", net_effect)),
        "not" => Some(unary_op("1 - a", net_effect)),
        "pow2" => Some(unary_op("2^a", net_effect)),
        "ilog2" => Some(unary_op("log2(a)", net_effect)),
        "is_odd" => Some(unary_op("is_odd(a)", net_effect)),
        "eqw" => Some(word_equality(Some(net_effect))),
        "caller" => Some(format_change(
            "[A, b, ...]",
            "[H, b, ...]",
            Some(net_effect),
        )),
        "reversew" => Some(reverse_word(Some(net_effect))),
        "padw" => Some(format_change("[...]", "[0, 0, 0, 0]", Some(net_effect))),
        _ => Some(format_stack_change_from_counts(
            semantics.pops(),
            semantics.pushes(),
        )),
    }
}

fn op_symbol(base: &str) -> &'static str {
    match base {
        "add" => "+",
        "sub" => "-",
        "mul" => "*",
        "div" => "/",
        "and" => "&",
        "or" => "|",
        "xor" => "^",
        "eq" => "==",
        "neq" => "!=",
        "lt" => "<",
        "lte" => "<=",
        "gt" => ">",
        "gte" => ">=",
        _ => "?",
    }
}

/// Render immediate arithmetic/comparison instructions with the constant inline.
fn format_immediate_instruction(inst: &Instruction, net_effect: Option<i32>) -> Option<String> {
    match inst {
        Instruction::AddImm(imm) => Some(format_change(
            "[a, ...]",
            &format!("[a + {}, ...]", imm),
            net_effect,
        )),
        Instruction::SubImm(imm) => Some(format_change(
            "[a, ...]",
            &format!("[a - {}, ...]", imm),
            net_effect,
        )),
        Instruction::MulImm(imm) => Some(format_change(
            "[a, ...]",
            &format!("[{} * a, ...]", imm),
            net_effect,
        )),
        Instruction::DivImm(imm) => Some(format_change(
            "[a, ...]",
            &format!("[a / {}, ...]", imm),
            net_effect,
        )),
        Instruction::EqImm(imm) => Some(format_change(
            "[a, ...]",
            &format!("[a == {}, ...]", imm),
            net_effect,
        )),
        Instruction::NeqImm(imm) => Some(format_change(
            "[a, ...]",
            &format!("[a != {}, ...]", imm),
            net_effect,
        )),
        Instruction::U32WrappingAddImm(imm) => Some(format_change(
            "[a, ...]",
            &format!("[a + {}, ...]", format_u32_immediate(imm)),
            net_effect,
        )),
        Instruction::U32WrappingSubImm(imm) => Some(format_change(
            "[a, ...]",
            &format!("[a - {}, ...]", format_u32_immediate(imm)),
            net_effect,
        )),
        Instruction::U32WrappingMulImm(imm) => Some(format_change(
            "[a, ...]",
            &format!("[a * {}, ...]", format_u32_immediate(imm)),
            net_effect,
        )),
        Instruction::U32DivImm(imm) => Some(format_change(
            "[a, ...]",
            &format!("[a / {}, ...]", format_u32_immediate(imm)),
            net_effect,
        )),
        Instruction::U32ModImm(imm) => Some(format_change(
            "[a, ...]",
            &format!("[a % {}, ...]", format_u32_immediate(imm)),
            net_effect,
        )),
        Instruction::U32ShlImm(imm) => Some(format_change(
            "[a, ...]",
            &format!("[a << {}, ...]", format_u8_immediate(imm)),
            net_effect,
        )),
        Instruction::U32ShrImm(imm) => Some(format_change(
            "[a, ...]",
            &format!("[a >> {}, ...]", format_u8_immediate(imm)),
            net_effect,
        )),
        Instruction::U32RotlImm(imm) => Some(format_change(
            "[a, ...]",
            &format!("[rotl(a, {}), ...]", format_u8_immediate(imm)),
            net_effect,
        )),
        Instruction::U32RotrImm(imm) => Some(format_change(
            "[a, ...]",
            &format!("[rotr(a, {}), ...]", format_u8_immediate(imm)),
            net_effect,
        )),
        Instruction::U32DivModImm(imm) => Some(format_change(
            "[a, ...]",
            &format!(
                "[a % {}, a / {}, ...]",
                format_u32_immediate(imm),
                format_u32_immediate(imm)
            ),
            net_effect,
        )),
        _ => None,
    }
}

/// Render u32 operations with their explicit expressions when possible.
fn format_u32_instruction(inst: &Instruction, semantics: InstructionEffect) -> Option<String> {
    use Instruction::*;

    let net_effect = semantics.net_effect();
    match inst {
        U32WrappingAdd => Some(binary_op("+", net_effect)),
        U32WrappingSub => Some(binary_op("-", net_effect)),
        U32WrappingMul => Some(binary_op("*", net_effect)),
        U32OverflowingAdd => Some(format_change(
            "[a, b, ...]",
            "[carry, b + a, ...]",
            Some(net_effect),
        )),
        U32OverflowingAddImm(imm) => Some(format_change(
            "[a, ...]",
            &format!("[carry, a + {}, ...]", format_u32_immediate(imm)),
            Some(net_effect),
        )),
        U32OverflowingSub => Some(format_change(
            "[a, b, ...]",
            "[borrow, b - a, ...]",
            Some(net_effect),
        )),
        U32OverflowingSubImm(imm) => Some(format_change(
            "[a, ...]",
            &format!("[borrow, a - {}, ...]", format_u32_immediate(imm)),
            Some(net_effect),
        )),
        U32OverflowingMul => Some(format_change(
            "[a, b, ...]",
            "[carry, b * a, ...]",
            Some(net_effect),
        )),
        U32OverflowingMulImm(imm) => Some(format_change(
            "[a, ...]",
            &format!("[carry, a * {}, ...]", format_u32_immediate(imm)),
            Some(net_effect),
        )),
        U32And => Some(binary_op("&", net_effect)),
        U32Or => Some(binary_op("|", net_effect)),
        U32Xor => Some(binary_op("^", net_effect)),
        U32Div => Some(binary_op("/", net_effect)),
        U32Mod => Some(binary_op("%", net_effect)),
        U32Shl => Some(binary_op("<<", net_effect)),
        U32Shr => Some(binary_op(">>", net_effect)),
        U32Rotl => Some(format_change(
            "[a, b, ...]",
            "[rotl(a, b), ...]",
            Some(net_effect),
        )),
        U32Rotr => Some(format_change(
            "[a, b, ...]",
            "[rotr(a, b), ...]",
            Some(net_effect),
        )),
        U32Min => Some(binary_op("min", net_effect)),
        U32Max => Some(binary_op("max", net_effect)),
        U32Lt => Some(binary_op("<", net_effect)),
        U32Lte => Some(binary_op("<=", net_effect)),
        U32Gt => Some(binary_op(">", net_effect)),
        U32Gte => Some(binary_op(">=", net_effect)),
        U32Not => Some(unary_op("~a", net_effect)),
        U32Popcnt => Some(unary_op("popcnt(a)", net_effect)),
        U32Clz => Some(unary_op("clz(a)", net_effect)),
        U32Ctz => Some(unary_op("ctz(a)", net_effect)),
        U32Clo => Some(unary_op("clo(a)", net_effect)),
        U32Cto => Some(unary_op("cto(a)", net_effect)),
        U32Cast => Some(unary_op("u32(a)", net_effect)),
        U32WrappingAdd3 => Some(ternary_op("a + b + c", net_effect)),
        U32OverflowingAdd3 => Some(format_change(
            "[a, b, c, ...]",
            "[carry, c + b + a, ...]",
            Some(net_effect),
        )),
        U32WrappingMadd => Some(ternary_op("a * b + c", net_effect)),
        U32OverflowingMadd => Some(format_change(
            "[a, b, c, ...]",
            "[overflow, b * a + c, ...]",
            Some(net_effect),
        )),
        U32DivMod => Some(format_change(
            "[a, b, ...]",
            "[a % b, a / b, ...]",
            Some(net_effect),
        )),
        U32Split => Some(format_change(
            "[a, ...]",
            "[hi(a), lo(a), ...]",
            Some(net_effect),
        )),
        _ => None,
    }
}

/// Stack-effect rendering for stack manipulation operations.
fn format_stack_op(op: StackOp, net_effect: i32) -> Option<String> {
    match op {
        StackOp::Swap(n) => Some(format_swap(n, ValueKind::Felt, net_effect)),
        StackOp::Dup(n) => Some(format_dup(n, ValueKind::Felt, net_effect)),
        StackOp::MovUp(n) => Some(format_mov_up(n, ValueKind::Felt, net_effect)),
        StackOp::MovDn(n) => Some(format_mov_down(n, ValueKind::Felt, net_effect)),
        StackOp::SwapW(n) => Some(format_swap(n, ValueKind::Word, net_effect)),
        StackOp::DupW(n) => Some(format_dup(n, ValueKind::Word, net_effect)),
        StackOp::MovUpW(n) => Some(format_mov_up(n, ValueKind::Word, net_effect)),
        StackOp::MovDnW(n) => Some(format_mov_down(n, ValueKind::Word, net_effect)),
        StackOp::SwapDW => Some(format_swap_double_word(net_effect)),
        StackOp::Drop => format_drop(&Instruction::Drop, 1, net_effect),
        StackOp::DropW => format_drop(&Instruction::DropW, 4, net_effect),
    }
}

/// Render a push instruction, including immediate values when available.
fn format_push(inst: &Instruction, count: usize, net_effect: i32) -> Option<String> {
    match inst {
        Instruction::Push(imm) => Some(format_change(
            "[...]",
            &format!("[{}, ...]", format_push_immediate(imm)),
            Some(net_effect),
        )),
        Instruction::PushFeltList(values) => {
            let values = values
                .iter()
                .map(|v| format!("{v}"))
                .collect::<Vec<_>>()
                .join(", ");
            Some(format_change(
                "[...]",
                &format!("[{}, ...]", values),
                Some(net_effect),
            ))
        }
        Instruction::PadW => Some(format_change("[...]", "[0, 0, 0, 0]", Some(net_effect))),
        _ if count > 0 => Some(format_change(
            "[...]",
            &format!("[{}, ...]", repeat_placeholder(count)),
            Some(net_effect),
        )),
        _ => None,
    }
}

/// Render drop operations.
fn format_drop(inst: &Instruction, count: usize, net_effect: i32) -> Option<String> {
    let kind = match inst {
        Instruction::DropW => ValueKind::Word,
        _ => ValueKind::Felt,
    };
    let before = render_prefix(count, kind, true);
    Some(format_change(
        &stack_to_string(&before),
        "[...]",
        Some(net_effect),
    ))
}

/// Render generic binary operations.
fn binary_op(symbol: &str, net_effect: i32) -> String {
    let before = "[a, b, ...]".to_string();
    let after = format!("[b {} a, ...]", symbol);
    format_change(&before, &after, Some(net_effect))
}

/// Render generic ternary operations.
fn ternary_op(expr: &str, net_effect: i32) -> String {
    let before = "[a, b, c, ...]".to_string();
    let after = format!("[{}, ...]", expr);
    format_change(&before, &after, Some(net_effect))
}

/// Render generic unary operations.
fn unary_op(expr: &str, net_effect: i32) -> String {
    let before = "[a, ...]".to_string();
    let after = format!("[{}, ...]", expr);
    format_change(&before, &after, Some(net_effect))
}

/// Render equality check for words.
fn word_equality(net_effect: Option<i32>) -> String {
    let before = "[A, B, ...]".to_string();
    let after = "[A == B, A, B, ...]".to_string();
    format_change(&before, &after, net_effect)
}

/// Render reversew.
fn reverse_word(net_effect: Option<i32>) -> String {
    let before = "[A, B, C, D, ...]".to_string();
    let after = "[D, C, B, A, ...]".to_string();
    format_change(&before, &after, net_effect)
}

/// Swap top with the nth element (or word).
fn format_swap(n: usize, kind: ValueKind, net_effect: i32) -> String {
    let before = render_prefix(n + 1, kind, true);
    let mut after = before.clone();
    after.swap(0, n);
    format_change(
        &stack_to_string(&before),
        &stack_to_string(&after),
        Some(net_effect),
    )
}

/// Duplicate element at position n to the top.
fn format_dup(n: usize, kind: ValueKind, net_effect: i32) -> String {
    let before = render_prefix(n + 1, kind, true);
    let mut after = Vec::with_capacity(before.len() + 1);
    let duplicate = before.get(n).cloned().unwrap_or_else(|| token(kind, 0));
    after.push(duplicate);
    after.extend(before.iter().cloned());
    format_change(
        &stack_to_string(&before),
        &stack_to_string(&after),
        Some(net_effect),
    )
}

/// Move the nth element to the top.
fn format_mov_up(n: usize, kind: ValueKind, net_effect: i32) -> String {
    let mut before = render_prefix(n + 1, kind, true);
    let depth = before.len().saturating_sub(1);
    let before_str = stack_to_string(&before);
    if n >= depth {
        return format_change(&before_str, "[...]", Some(net_effect));
    }
    let target = before.remove(n);
    let mut after = Vec::with_capacity(before.len() + 1);
    after.push(target);
    after.extend(before);
    format_change(&before_str, &stack_to_string(&after), Some(net_effect))
}

/// Move the top element down to position n.
fn format_mov_down(n: usize, kind: ValueKind, net_effect: i32) -> String {
    let mut before = render_prefix(n + 1, kind, true);
    let before_str = stack_to_string(&before);
    if before.is_empty() {
        return format_change(&before_str, "[...]", Some(net_effect));
    }
    let top = before.remove(0);
    let mut after = Vec::with_capacity(before.len() + 1);
    after.extend(before.iter().cloned());
    if n >= after.len() {
        after.push(top);
    } else {
        after.insert(n, top);
    }
    format_change(&before_str, &stack_to_string(&after), Some(net_effect))
}

/// Swap the top two words with the next two words.
fn format_swap_double_word(net_effect: i32) -> String {
    let before = vec![
        "A".to_string(),
        "B".to_string(),
        "C".to_string(),
        "D".to_string(),
        "...".to_string(),
    ];
    let after = vec![
        "C".to_string(),
        "D".to_string(),
        "A".to_string(),
        "B".to_string(),
        "...".to_string(),
    ];
    format_change(
        &stack_to_string(&before),
        &stack_to_string(&after),
        Some(net_effect),
    )
}

/// Normalize stack input/output shapes by renaming variables alphabetically.
pub(crate) fn canonicalize_shapes(input: &str, output: &str) -> (String, String) {
    let mut mapper = NameMapper::default();
    let before = mapper.render(input);
    let after = mapper.render(output);
    (before, after)
}

/// Format a before/after stack change with net effect.
fn format_change(
    before: impl AsRef<str>,
    after: impl AsRef<str>,
    net_effect: Option<i32>,
) -> String {
    format!(
        "{} → {} (net effect: {})",
        before.as_ref(),
        after.as_ref(),
        net_effect
            .map(|n| n.to_string())
            .unwrap_or_else(|| "unknown".to_string())
    )
}

/// Format a stack effect from input/output counts using felt placeholders.
pub fn format_stack_change_from_counts(inputs: usize, outputs: usize) -> String {
    let before = stack_to_string(&render_prefix(inputs, ValueKind::Felt, true));
    // Outputs use distinct placeholders starting after inputs to avoid reuse
    let mut after_tokens: Vec<String> = (0..outputs)
        .map(|idx| token(ValueKind::Felt, inputs + idx))
        .collect();
    after_tokens.push("...".into());
    let after = stack_to_string(&after_tokens);
    format_change(before, after, Some(outputs as i32 - inputs as i32))
}

fn format_generic_shape(count: usize, prefix: &str) -> String {
    if count == 0 {
        return "[...]".to_string();
    }
    let mut parts = Vec::new();
    for i in 0..count {
        parts.push(format!("{}{}", prefix, i));
    }
    format!("[{}, ...]", parts.join(", "))
}

/// Render a stack prefix of `len` elements, appending ellipsis if requested.
fn render_prefix(len: usize, kind: ValueKind, append_ellipsis: bool) -> Vec<String> {
    let mut tokens: Vec<String> = (0..len).map(|idx| token(kind, idx)).collect();
    if append_ellipsis {
        tokens.push("...".into());
    }
    tokens
}

fn stack_to_string(tokens: &[String]) -> String {
    format!("[{}]", tokens.join(", "))
}

/// Build a label for the nth position.
fn token(kind: ValueKind, idx: usize) -> String {
    let base = (idx % 26) as u8;
    let suffix = idx / 26;
    let ch = match kind {
        ValueKind::Felt => (b'a' + base) as char,
        ValueKind::Word => (b'A' + base) as char,
    };
    if suffix == 0 {
        ch.to_string()
    } else {
        format!("{ch}{suffix}")
    }
}

/// Repeat a placeholder token count times for generic push rendering.
fn repeat_placeholder(count: usize) -> String {
    let tokens: Vec<_> = (0..count).map(|idx| token(ValueKind::Felt, idx)).collect();
    tokens.join(", ")
}

/// Map original variable names to alphabetically ordered placeholders.
#[derive(Default)]
struct NameMapper {
    felt_idx: usize,
    word_idx: usize,
    mapping: HashMap<String, String>,
}

impl NameMapper {
    fn render(&mut self, stack: &str) -> String {
        let mapped: Vec<String> = tokenize(stack)
            .into_iter()
            .map(|tok| self.map_token(&tok))
            .collect();
        format!("[{}]", mapped.join(", "))
    }

    fn map_token(&mut self, tok: &str) -> String {
        if tok == "..." || is_constant(tok) {
            return tok.to_string();
        }
        if let Some(mapped) = self.mapping.get(tok) {
            return mapped.clone();
        }
        let kind = classify_kind(tok).unwrap_or(ValueKind::Felt);
        let label = match kind {
            ValueKind::Felt => {
                let label = token(ValueKind::Felt, self.felt_idx);
                self.felt_idx += 1;
                label
            }
            ValueKind::Word => {
                let label = token(ValueKind::Word, self.word_idx);
                self.word_idx += 1;
                label
            }
        };
        self.mapping.insert(tok.to_string(), label.clone());
        label
    }
}

/// Split a stack representation (`[a, b, ...]`) into tokens.
fn tokenize(stack: &str) -> Vec<String> {
    stack
        .trim_matches(|c| c == '[' || c == ']')
        .split(',')
        .map(|t| t.trim().to_string())
        .filter(|t| !t.is_empty())
        .collect()
}

/// Determine whether a token represents a constant (numeric) value.
fn is_constant(token: &str) -> bool {
    token
        .chars()
        .next()
        .map(|c| c.is_ascii_digit() || c == '-')
        .unwrap_or(false)
}

/// Classify token kind to decide placeholder casing.
fn classify_kind(token: &str) -> Option<ValueKind> {
    let alpha = token.chars().find(|c| c.is_ascii_alphabetic())?;
    if alpha.is_ascii_uppercase() {
        Some(ValueKind::Word)
    } else {
        Some(ValueKind::Felt)
    }
}

#[derive(Clone, Copy)]
enum ValueKind {
    Felt,
    Word,
}
