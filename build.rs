//! Build script to generate compile-time instruction reference map.
//!
//! This script reads `data/instructions.toml` and generates a static phf::Map
//! that provides O(1) lookup for instruction descriptions without runtime parsing.

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

    // Build the phf map
    let mut builder = phf_codegen::Map::new();
    for (key, value) in &table {
        if let toml::Value::String(desc) = value {
            builder.entry(key.as_str(), &format!("\"{}\"", escape_string(desc)));
        }
    }

    writeln!(
        &mut file,
        "/// Compile-time generated instruction reference map.\n\
         /// Maps instruction names to their short descriptions.\n\
         static INSTRUCTION_MAP: phf::Map<&'static str, &'static str> = {};",
        builder.build()
    )
    .unwrap();
}

/// Escape special characters in a string for use in Rust string literals.
fn escape_string(s: &str) -> String {
    s.replace('\\', "\\\\").replace('"', "\\\"")
}
