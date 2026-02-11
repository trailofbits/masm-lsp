use std::{env, fs, path::PathBuf};

use serde::Deserialize;

#[derive(Deserialize)]
struct InstructionList {
    instructions: Vec<InstructionEntry>,
}

#[derive(Deserialize)]
struct InstructionEntry {
    name: String,
    description: String,
    stack_effect: String,
}

fn main() {
    let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").expect("manifest dir"));
    let input_path = manifest_dir.join("data").join("instruction_reference.toml");
    println!("cargo:rerun-if-changed={}", input_path.display());

    let data = fs::read_to_string(&input_path)
        .unwrap_or_else(|err| panic!("failed to read {}: {err}", input_path.display()));
    let list: InstructionList = toml::from_str(&data)
        .unwrap_or_else(|err| panic!("failed to parse {}: {err}", input_path.display()));

    let mut exact = Vec::new();
    let mut templates = Vec::new();

    for entry in list.instructions {
        if entry.name.contains('{') && entry.name.contains('}') {
            templates.push(entry);
        } else {
            exact.push(entry);
        }
    }

    let out_dir = PathBuf::from(env::var("OUT_DIR").expect("OUT_DIR"));
    let out_path = out_dir.join("instruction_data.rs");
    let rendered = render_instruction_data(&exact, &templates);
    fs::write(&out_path, rendered)
        .unwrap_or_else(|err| panic!("failed to write {}: {err}", out_path.display()));
}

fn render_instruction_data(exact: &[InstructionEntry], templates: &[InstructionEntry]) -> String {
    let mut output = String::new();
    output.push_str("const EXACT_INSTRUCTIONS: &[InstructionEntry] = &[\n");
    for entry in exact {
        output.push_str("    InstructionEntry {");
        output.push_str(&format!(
            " name: {name}, description: {description}, stack_effect: {stack_effect} ",
            name = quote(&entry.name),
            description = quote(&entry.description),
            stack_effect = quote(&entry.stack_effect),
        ));
        output.push_str("},\n");
    }
    output.push_str("];\n\n");

    output.push_str("const TEMPLATE_INSTRUCTIONS: &[InstructionTemplate] = &[\n");
    for entry in templates {
        let (prefix, suffix) = split_template(&entry.name);
        output.push_str("    InstructionTemplate {");
        output.push_str(&format!(
            " prefix: {prefix}, suffix: {suffix}, description: {description}, stack_effect: {stack_effect} ",
            prefix = quote(prefix),
            suffix = quote(suffix),
            description = quote(&entry.description),
            stack_effect = quote(&entry.stack_effect),
        ));
        output.push_str("},\n");
    }
    output.push_str("];\n\n");

    output.push_str("impl_instruction_traits!(EXACT_INSTRUCTIONS, TEMPLATE_INSTRUCTIONS);\n");
    output
}

fn split_template(name: &str) -> (&str, &str) {
    let start = name.find('{').unwrap_or_else(|| {
        panic!("template name must contain a placeholder like '{{n}}': {name}");
    });
    let end = name[start + 1..]
        .find('}')
        .map(|idx| idx + start + 1)
        .unwrap_or_else(|| panic!("template name missing closing '}}': {name}"));
    if name[end + 1..].contains('{') || name[end + 1..].contains('}') {
        panic!("template name must contain exactly one placeholder: {name}");
    }
    (&name[..start], &name[end + 1..])
}

fn quote(value: &str) -> String {
    let mut out = String::new();
    out.push('"');
    for ch in value.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            _ => out.push(ch),
        }
    }
    out.push('"');
    out
}
