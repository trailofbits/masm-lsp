pub trait ToDescription {
    fn to_description(&self) -> Option<String>;
}

pub trait ToStackEffect {
    fn to_stack_effect(&self) -> Option<String>;
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
                stack_effect_for_name(&self.to_string())
            }
        }
    };
}

include!(concat!(env!("OUT_DIR"), "/instruction_data.rs"));

#[cfg(test)]
mod tests {
    use super::apply_template;

    #[test]
    fn apply_template_numeric_ordinal() {
        let template = "Pushes the {nth} item; value {n}.";
        assert_eq!(apply_template(template, "1"), "Pushes the 1st item; value 1.");
        assert_eq!(apply_template(template, "2"), "Pushes the 2nd item; value 2.");
        assert_eq!(apply_template(template, "3"), "Pushes the 3rd item; value 3.");
        assert_eq!(apply_template(template, "4"), "Pushes the 4th item; value 4.");
        assert_eq!(apply_template(template, "11"), "Pushes the 11th item; value 11.");
        assert_eq!(apply_template(template, "12"), "Pushes the 12th item; value 12.");
        assert_eq!(apply_template(template, "13"), "Pushes the 13th item; value 13.");
        assert_eq!(apply_template(template, "21"), "Pushes the 21st item; value 21.");
    }

    #[test]
    fn apply_template_non_numeric_falls_back() {
        let template = "Uses {nth} argument and {n}.";
        assert_eq!(apply_template(template, "CONST"), "Uses CONST argument and CONST.");
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
}
