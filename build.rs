//! Build script to generate compile-time instruction reference map.
//!
//! This script reads `data/instructions.toml` and generates a static phf::Map
//! that maps instruction names to InstructionInfo structs.

use std::env;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::Path;

fn main() {
    // Re-run if the instruction data changes
    println!("cargo:rerun-if-changed=data/instructions.toml");
    println!("cargo:rerun-if-changed=data/semantics_overrides.toml");

    let out_dir = env::var("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("instruction_map.rs");
    let mut file = BufWriter::new(File::create(&dest_path).unwrap());

    // Parse the TOML file
    let content = std::fs::read_to_string("data/instructions.toml")
        .expect("Failed to read data/instructions.toml");
    let table: toml::Table = content
        .parse()
        .expect("Failed to parse data/instructions.toml");

    // Write the struct definition
    writeln!(
        &mut file,
        "/// Information about a Miden assembly instruction.\n\
         #[derive(Debug, Clone, Copy)]\n\
         pub struct InstructionInfo {{\n\
         \x20   pub description: &'static str,\n\
         \x20   pub stack_input: &'static str,\n\
         \x20   pub stack_output: &'static str,\n\
         \x20   pub cycles: &'static str,\n\
         }}\n"
    )
    .unwrap();

    // Build the map
    let mut builder = phf_codegen::Map::new();
    let mut sem_builder = phf_codegen::Map::new();
    let mut override_builder = phf_codegen::Map::new();

    for (key, value) in &table {
        if let toml::Value::Table(instruction) = value {
            let description = instruction
                .get("description")
                .and_then(|v| v.as_str())
                .unwrap_or("");
            let stack_input = instruction
                .get("stack_input")
                .and_then(|v| v.as_str())
                .unwrap_or("");
            let stack_output = instruction
                .get("stack_output")
                .and_then(|v| v.as_str())
                .unwrap_or("");
            let cycles = instruction
                .get("cycles")
                .and_then(|v| v.as_str())
                .unwrap_or("");

            let struct_literal = format!(
                "InstructionInfo {{ description: \"{}\", stack_input: \"{}\", stack_output: \"{}\", cycles: \"{}\" }}",
                escape_string(&sanitize_text(description)),
                escape_string(stack_input),
                escape_string(stack_output),
                escape_string(cycles)
            );
            builder.entry(key.as_str(), &struct_literal);

            let pops = count_tokens(stack_input);
            let pushes = count_tokens(stack_output);
            sem_builder.entry(
                key.as_str(),
                &format!(
                    "InstructionSemEntry {{ pops: {}, pushes: {} }}",
                    pops, pushes
                ),
            );
        }
    }

    writeln!(
        &mut file,
        "/// Compile-time generated instruction reference map.\n\
         /// Maps instruction names to their info.\n\
         #[allow(dead_code)]\n\
         static INSTRUCTION_MAP: phf::Map<&'static str, InstructionInfo> = {};",
        builder.build()
    )
    .unwrap();

    writeln!(
        &mut file,
        "/// Basic stack semantics derived from documentation shapes.\n\
         #[derive(Debug, Clone, Copy)]\n\
         pub struct InstructionSemEntry {{\n\
         \x20   pub pops: usize,\n\
         \x20   pub pushes: usize,\n\
         }}\n"
    )
    .unwrap();

    writeln!(
        &mut file,
        "#[allow(dead_code)]\n\
         static INSTRUCTION_SEMANTICS: phf::Map<&'static str, InstructionSemEntry> = {};",
        sem_builder.build()
    )
    .unwrap();

    // Optional overrides that correct doc-derived semantics
    let override_entries = load_overrides("data/semantics_overrides.toml");
    for (name, pops, pushes) in override_entries {
        override_builder.entry(
            name,
            &format!(
                "InstructionSemEntry {{ pops: {}, pushes: {} }}",
                pops, pushes
            ),
        );
    }

    writeln!(
        &mut file,
        "#[allow(dead_code)]\n\
         static INSTRUCTION_SEMANTICS_OVERRIDES: phf::Map<&'static str, InstructionSemEntry> = {};",
        override_builder.build()
    )
    .unwrap();
}

/// Sanitize text for display: remove LaTeX delimiters and replace braces with parentheses.
fn sanitize_text(s: &str) -> String {
    s.replace('$', "")
        .replace('{', "(")
        .replace('}', ")")
        .replace("\\leftarrow", "←")
        .replace("\\rightarrow", "→")
        .replace("\\lfloor", "⌊")
        .replace("\\rfloor", "⌋")
        .replace("\\mod", "mod")
        .replace("\\neq", "≠")
        .replace("\\le", "≤")
        .replace("\\ge", "≥")
        .replace("\\cdot", "·")
        .replace("\\times", "×")
}

/// Escape special characters in a string for use in Rust string literals.
fn escape_string(s: &str) -> String {
    s.replace('\\', "\\\\").replace('"', "\\\"")
}

fn count_tokens(shape: &str) -> usize {
    shape
        .trim_matches(['[', ']'])
        .split(',')
        .filter(|s| {
            let t = s.trim();
            !t.is_empty() && t != "..." && t.to_ascii_lowercase() != "stack"
        })
        .count()
}

fn load_overrides(path: &str) -> Vec<(String, usize, usize)> {
    match std::fs::read_to_string(path) {
        Ok(content) => {
            let table: toml::Table = match content.parse() {
                Ok(t) => t,
                Err(_) => return Vec::new(),
            };
            table
                .into_iter()
                .filter_map(|(k, v)| {
                    let Some(t) = v.as_table() else { return None };
                    let pops = t.get("pops")?.as_integer()? as usize;
                    let pushes = t.get("pushes")?.as_integer()? as usize;
                    Some((k, pops, pushes))
                })
                .collect()
        }
        Err(_) => Vec::new(),
    }
}
