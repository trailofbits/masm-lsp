use std::collections::HashMap;

use miden_assembly_syntax::ast::Instruction;

use crate::analysis::instruction_effects::{InstructionEffect, StackOp};
use crate::analysis::stack_ops::{static_effect, StaticEffect};
use crate::descriptions::format_push_immediate;
use crate::instruction_docs::get_instruction_info;

/// Trait for rendering stack-effect strings for instructions.
pub trait ToStackEffect {
    /// Returns a formatted stack-effect string, e.g.
    /// `[a, b, ...] → [a <= b, ...] (net effect: -1)`.
    fn to_stack_effect(&self) -> Option<String>;
}

impl ToStackEffect for Instruction {
    fn to_stack_effect(&self) -> Option<String> {
        format_stack_effect(self)
    }
}

fn format_stack_effect(inst: &Instruction) -> Option<String> {
    let rendered = inst.to_string();
    let info = get_instruction_info(&rendered);
    let static_eff = static_effect(inst);
    let net_effect = static_eff.map(|e| e.net());

    // Try rich, instruction-specific rendering first
    let effect = InstructionEffect::of(inst);
    if let Some(label) = format_by_effect(inst, effect, static_eff, net_effect) {
        return Some(label);
    }

    // Fall back to instruction metadata (stack_input/stack_output)
    if let Some(info) = info {
        let (input, output) = canonicalize_shapes(info.stack_input, info.stack_output);
        return Some(format_change(&input, &output, net_effect));
    }

    None
}

/// Format instructions with known effects into before/after stack strings.
fn format_by_effect(
    inst: &Instruction,
    effect: InstructionEffect,
    static_eff: Option<StaticEffect>,
    net_effect: Option<i32>,
) -> Option<String> {
    match effect {
        InstructionEffect::Manipulation(op) => format_stack_op(op, net_effect?),
        InstructionEffect::Push { count } => format_push(inst, count, net_effect?),
        InstructionEffect::Drop { count } => format_drop(inst, count, net_effect?),
        InstructionEffect::Memory { .. } => None, // fall back to metadata for memory ops
        InstructionEffect::Static(_) => format_static(inst, static_eff, net_effect),
        InstructionEffect::Unknown => None,
    }
}

/// Try to format common static operations (arithmetic, comparisons, etc.).
fn format_static(
    inst: &Instruction,
    static_eff: Option<StaticEffect>,
    net_effect: Option<i32>,
) -> Option<String> {
    let counts = static_eff?;
    let net_effect = net_effect.unwrap_or_else(|| counts.net());
    if let Some(label) = format_immediate_instruction(inst, counts) {
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
            if counts.pops >= 2 && counts.pushes >= 1 =>
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
        "reversew" => Some(reverse_word(Some(net_effect))),
        "padw" => Some(format_change("[...]", "[0, 0, 0, 0]", Some(net_effect))),
        _ => Some(format_stack_change_from_counts(counts.pops, counts.pushes)),
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
fn format_immediate_instruction(inst: &Instruction, counts: StaticEffect) -> Option<String> {
    let net = counts.net();
    match inst {
        Instruction::AddImm(imm) => Some(format_change(
            "[a, ...]",
            &format!("[a + {}, ...]", imm),
            Some(net),
        )),
        Instruction::SubImm(imm) => Some(format_change(
            "[a, ...]",
            &format!("[a - {}, ...]", imm),
            Some(net),
        )),
        Instruction::MulImm(imm) => Some(format_change(
            "[a, ...]",
            &format!("[{} * a, ...]", imm),
            Some(net),
        )),
        Instruction::DivImm(imm) => Some(format_change(
            "[a, ...]",
            &format!("[a / {}, ...]", imm),
            Some(net),
        )),
        Instruction::EqImm(imm) => Some(format_change(
            "[a, ...]",
            &format!("[a == {}, ...]", imm),
            Some(net),
        )),
        Instruction::NeqImm(imm) => Some(format_change(
            "[a, ...]",
            &format!("[a != {}, ...]", imm),
            Some(net),
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
        StackOp::CSwap | StackOp::CSwapW => None,
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
    let after = format!("[a {} b, ...]", symbol);
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
fn canonicalize_shapes(input: &str, output: &str) -> (String, String) {
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
