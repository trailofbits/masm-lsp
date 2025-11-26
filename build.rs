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
        }
    }

    writeln!(
        &mut file,
        "/// Compile-time generated instruction reference map.\n\
         /// Maps instruction names to their info.\n\
         static INSTRUCTION_MAP: phf::Map<&'static str, InstructionInfo> = {};",
        builder.build()
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
        .replace("\\mod", "mod")
        .replace("\\neq", "≠")
        .replace("\\cdot", "·")
        .replace("\\times", "×")
}

/// Escape special characters in a string for use in Rust string literals.
fn escape_string(s: &str) -> String {
    s.replace('\\', "\\\\").replace('"', "\\\"")
}
