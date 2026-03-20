pub trait ToDescription {
    fn to_description(&self) -> Option<String>;
}

pub trait ToStackEffect {
    fn to_stack_effect(&self) -> Option<String>;
}

/// Returns the stack effect for push instructions that operate on inline words.
///
/// Handles `push.[a, b, c, d]` (word push) and `push.[a, b, c, d][i..j]` (word
/// slice push). For word pushes, element `a` ends up on top of the stack. Returns
/// `None` for all other instruction forms, so the caller can fall through to
/// template-based lookup.
fn inline_word_stack_effect(inst: &miden_assembly_syntax::ast::Instruction) -> Option<String> {
    use miden_assembly_syntax::ast::{Immediate, Instruction};
    use miden_assembly_syntax::parser::{PushValue, WordValue};
    use miden_assembly_syntax::Felt;

    fn fmt_felts(felts: &[Felt]) -> String {
        felts
            .iter()
            .map(|f| f.as_canonical_u64().to_string())
            .collect::<Vec<_>>()
            .join(", ")
    }

    match inst {
        Instruction::Push(Immediate::Value(spanned)) => {
            let PushValue::Word(WordValue(felts)) = spanned.into_inner() else {
                return None;
            };
            Some(format!("(...) \u{2192} ({}, ...)", fmt_felts(&felts)))
        }
        Instruction::PushSlice(Immediate::Value(spanned), range) => {
            let WordValue(felts) = spanned.into_inner();
            let slice = felts.get(range.clone())?;
            Some(format!("(...) \u{2192} ({}, ...)", fmt_felts(slice)))
        }
        _ => None,
    }
}

fn apply_template(template: &str, value: &str) -> String {
    let mut rendered = replace_offset_placeholders(template, value);
    rendered = rendered.replace("{n}", value);
    if rendered.contains("{nth}") {
        rendered = rendered.replace("{nth}", &ordinal_suffix(value));
    }
    rendered
}

fn replace_offset_placeholders(template: &str, value: &str) -> String {
    let Ok(base) = value.parse::<i64>() else {
        return template.to_string();
    };

    let mut output = String::new();
    let mut cursor = 0;
    while let Some(rel_start) = template[cursor..].find("{n+") {
        let start = cursor + rel_start;
        output.push_str(&template[cursor..start]);

        let after_start = start + 3;
        let Some(rel_end) = template[after_start..].find('}') else {
            output.push_str(&template[start..]);
            return output;
        };
        let end = after_start + rel_end;
        let offset_str = &template[after_start..end];
        if let Ok(offset) = offset_str.parse::<i64>() {
            output.push_str(&(base + offset).to_string());
        } else {
            output.push_str(&template[start..=end]);
        }
        cursor = end + 1;
    }

    output.push_str(&template[cursor..]);
    output
}

fn ordinal_suffix(value: &str) -> String {
    let Ok(number) = value.parse::<u32>() else {
        return value.to_string();
    };

    let suffix = if (11..=13).contains(&(number % 100)) {
        "th"
    } else {
        match number % 10 {
            1 => "st",
            2 => "nd",
            3 => "rd",
            _ => "th",
        }
    };

    format!("{number}{suffix}")
}

#[derive(Clone, Copy)]
struct InstructionEntry {
    name: &'static str,
    description: &'static str,
    stack_effect: &'static str,
}

#[derive(Clone, Copy)]
struct InstructionTemplate {
    prefix: &'static str,
    suffix: &'static str,
    description: &'static str,
    stack_effect: &'static str,
}

macro_rules! impl_instruction_traits {
    ($exact:expr, $templates:expr) => {
        pub fn description_for_name(name: &str) -> Option<String> {
            for entry in $exact {
                if entry.name == name {
                    if entry.description.trim().is_empty() {
                        return None;
                    }
                    return Some(entry.description.to_string());
                }
            }

            for entry in $templates {
                if let Some(value) = match_template(name, entry) {
                    let rendered = apply_template(entry.description, value);
                    if rendered.trim().is_empty() {
                        return None;
                    }
                    return Some(rendered);
                }
            }

            None
        }

        pub fn stack_effect_for_name(name: &str) -> Option<String> {
            for entry in $exact {
                if entry.name == name {
                    if entry.stack_effect.trim().is_empty() {
                        return None;
                    }
                    return Some(entry.stack_effect.to_string());
                }
            }

            for entry in $templates {
                if let Some(value) = match_template(name, entry) {
                    let rendered = apply_template(entry.stack_effect, value);
                    if rendered.trim().is_empty() {
                        return None;
                    }
                    return Some(rendered);
                }
            }

            None
        }

        fn match_template<'a>(name: &'a str, template: &InstructionTemplate) -> Option<&'a str> {
            if !name.starts_with(template.prefix) || !name.ends_with(template.suffix) {
                return None;
            }
            if name.len() <= template.prefix.len() + template.suffix.len() {
                return None;
            }
            let start = template.prefix.len();
            let end = name.len() - template.suffix.len();
            Some(&name[start..end])
        }

        impl ToDescription for miden_assembly_syntax::ast::Instruction {
            fn to_description(&self) -> Option<String> {
                description_for_name(&self.to_string())
            }
        }

        impl ToStackEffect for miden_assembly_syntax::ast::Instruction {
            fn to_stack_effect(&self) -> Option<String> {
                if let Some(effect) = inline_word_stack_effect(self) {
                    return Some(effect);
                }
                stack_effect_for_name(&self.to_string())
            }
        }
    };
}

include!(concat!(env!("OUT_DIR"), "/instruction_data.rs"));

#[cfg(test)]
mod tests {
    use super::{apply_template, inline_word_stack_effect};

    #[test]
    fn apply_template_numeric_ordinal() {
        let template = "Pushes the {nth} item; value {n}.";
        assert_eq!(
            apply_template(template, "1"),
            "Pushes the 1st item; value 1."
        );
        assert_eq!(
            apply_template(template, "2"),
            "Pushes the 2nd item; value 2."
        );
        assert_eq!(
            apply_template(template, "3"),
            "Pushes the 3rd item; value 3."
        );
        assert_eq!(
            apply_template(template, "4"),
            "Pushes the 4th item; value 4."
        );
        assert_eq!(
            apply_template(template, "11"),
            "Pushes the 11th item; value 11."
        );
        assert_eq!(
            apply_template(template, "12"),
            "Pushes the 12th item; value 12."
        );
        assert_eq!(
            apply_template(template, "13"),
            "Pushes the 13th item; value 13."
        );
        assert_eq!(
            apply_template(template, "21"),
            "Pushes the 21st item; value 21."
        );
    }

    #[test]
    fn apply_template_non_numeric_falls_back() {
        let template = "Uses {nth} argument and {n}.";
        assert_eq!(
            apply_template(template, "CONST"),
            "Uses CONST argument and CONST."
        );
    }

    #[test]
    fn apply_template_leaves_unknown_placeholders() {
        let template = "Value {name} stays.";
        assert_eq!(apply_template(template, "X"), "Value {name} stays.");
    }

    #[test]
    fn apply_template_numeric_offsets() {
        let template = "Value {n+3} and {n+10}.";
        assert_eq!(apply_template(template, "2"), "Value 5 and 12.");
    }

    #[test]
    fn apply_template_offset_non_numeric_is_unchanged() {
        let template = "Value {n+3}.";
        assert_eq!(apply_template(template, "CONST"), "Value {n+3}.");
    }

    fn felt(v: u64) -> miden_assembly_syntax::Felt {
        miden_assembly_syntax::Felt::new(v)
    }

    fn push_word_inst(
        felts: [miden_assembly_syntax::Felt; 4],
    ) -> miden_assembly_syntax::ast::Instruction {
        use miden_assembly_syntax::ast::{Immediate, Instruction};
        use miden_assembly_syntax::parser::{PushValue, WordValue};
        Instruction::Push(Immediate::Value(
            miden_assembly_syntax::debuginfo::Span::unknown(PushValue::Word(WordValue(felts))),
        ))
    }

    fn push_slice_inst(
        felts: [miden_assembly_syntax::Felt; 4],
        range: core::ops::Range<usize>,
    ) -> miden_assembly_syntax::ast::Instruction {
        use miden_assembly_syntax::ast::{Immediate, Instruction};
        use miden_assembly_syntax::parser::WordValue;
        Instruction::PushSlice(
            Immediate::Value(miden_assembly_syntax::debuginfo::Span::unknown(WordValue(
                felts,
            ))),
            range,
        )
    }

    fn push_int_inst(v: u8) -> miden_assembly_syntax::ast::Instruction {
        use miden_assembly_syntax::ast::{Immediate, Instruction};
        use miden_assembly_syntax::parser::{IntValue, PushValue};
        Instruction::Push(Immediate::Value(
            miden_assembly_syntax::debuginfo::Span::unknown(PushValue::Int(IntValue::U8(v))),
        ))
    }

    #[test]
    fn inline_word_stack_effect_basic() {
        let inst = push_word_inst([felt(1), felt(2), felt(3), felt(4)]);
        assert_eq!(
            inline_word_stack_effect(&inst),
            Some("(...) \u{2192} (1, 2, 3, 4, ...)".to_string())
        );
    }

    #[test]
    fn inline_word_stack_effect_zeros() {
        let inst = push_word_inst([felt(1), felt(0), felt(0), felt(0)]);
        assert_eq!(
            inline_word_stack_effect(&inst),
            Some("(...) \u{2192} (1, 0, 0, 0, ...)".to_string())
        );
    }

    #[test]
    fn inline_word_stack_effect_returns_none_for_int_push() {
        let inst = push_int_inst(42);
        assert_eq!(inline_word_stack_effect(&inst), None);
    }

    #[test]
    fn push_slice_stack_effect_single_element() {
        let inst = push_slice_inst([felt(10), felt(20), felt(30), felt(40)], 0..1);
        assert_eq!(
            inline_word_stack_effect(&inst),
            Some("(...) \u{2192} (10, ...)".to_string())
        );
    }

    #[test]
    fn push_slice_stack_effect_range() {
        let inst = push_slice_inst([felt(10), felt(20), felt(30), felt(40)], 1..3);
        assert_eq!(
            inline_word_stack_effect(&inst),
            Some("(...) \u{2192} (20, 30, ...)".to_string())
        );
    }

    #[test]
    fn push_slice_stack_effect_full_range() {
        let inst = push_slice_inst([felt(1), felt(2), felt(3), felt(4)], 0..4);
        assert_eq!(
            inline_word_stack_effect(&inst),
            Some("(...) \u{2192} (1, 2, 3, 4, ...)".to_string())
        );
    }

    #[test]
    fn push_slice_stack_effect_out_of_range() {
        let inst = push_slice_inst([felt(1), felt(2), felt(3), felt(4)], 2..5);
        assert_eq!(inline_word_stack_effect(&inst), None);
    }
}
