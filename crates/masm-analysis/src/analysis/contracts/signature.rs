//! Procedure signature parsing.
//!
//! This module handles parsing explicit type annotations from procedure
//! signatures, such as `proc foo(a: u256, b: u64) -> u128`.

// ═══════════════════════════════════════════════════════════════════════════
// Parsed Signature
// ═══════════════════════════════════════════════════════════════════════════

/// Parsed stack effect from an explicit procedure signature.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedSignature {
    /// Number of input stack elements
    pub inputs: usize,
    /// Number of output stack elements
    pub outputs: usize,
}

// ═══════════════════════════════════════════════════════════════════════════
// Signature Parsing
// ═══════════════════════════════════════════════════════════════════════════

/// Get the number of stack elements for a type name.
///
/// Returns the number of field elements that the type occupies on the stack:
/// - `u64` = 2 elements
/// - `u128` = 4 elements
/// - `u256` = 8 elements
/// - Unknown types default to 1 element
fn type_to_element_count(type_name: &str) -> usize {
    match type_name.trim() {
        "u64" => 2,
        "u128" => 4,
        "u256" => 8,
        _ => 1, // Default: single field element
    }
}

/// Parse a procedure signature to extract input and output element counts.
///
/// Supports signatures like:
/// - `proc foo(a: u256, b: u64) -> u128`
/// - `pub proc bar(x: u256) -> u256`
/// - `export.baz(a: u64, b: u64)`
///
/// Returns `None` if no signature with types is found.
pub fn parse_procedure_signature(signature_line: &str) -> Option<ParsedSignature> {
    // Find the parameter list: everything between ( and )
    let params_start = signature_line.find('(')?;
    let params_end = signature_line.find(')')?;
    if params_start >= params_end {
        return None;
    }

    let params_str = &signature_line[params_start + 1..params_end];

    // Parse input parameters
    let mut inputs = 0;
    if !params_str.trim().is_empty() {
        for param in params_str.split(',') {
            let param = param.trim();
            if param.is_empty() {
                continue;
            }
            // Look for type annotation: "name: type"
            if let Some(colon_pos) = param.find(':') {
                let type_name = param[colon_pos + 1..].trim();
                inputs += type_to_element_count(type_name);
            } else {
                // No type annotation, assume single element
                inputs += 1;
            }
        }
    }

    // Parse return type(s): everything after "->"
    let mut outputs = 0;
    if let Some(arrow_pos) = signature_line.find("->") {
        let return_str = &signature_line[arrow_pos + 2..];
        // Return can be a single type or comma-separated types
        for ret_type in return_str.split(',') {
            let ret_type = ret_type.trim();
            if ret_type.is_empty() {
                continue;
            }
            outputs += type_to_element_count(ret_type);
        }
    }

    // Only return if we found typed parameters (otherwise fall back to inference)
    if inputs > 0 || outputs > 0 {
        Some(ParsedSignature { inputs, outputs })
    } else {
        None
    }
}

/// Extract and parse a procedure signature from source text at a given line.
///
/// Returns the parsed signature if the procedure has explicit type annotations.
pub fn extract_and_parse_signature(source: &str, def_line: usize) -> Option<ParsedSignature> {
    let line = source.lines().nth(def_line)?;
    parse_procedure_signature(line)
}

// ═══════════════════════════════════════════════════════════════════════════
// Tests
// ═══════════════════════════════════════════════════════════════════════════

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_signature_u256_params() {
        let sig = parse_procedure_signature("pub proc xor(rhs: u256, lhs: u256) -> u256");
        assert!(sig.is_some());
        let sig = sig.unwrap();
        // u256 = 8 elements each, so 2 params = 16 inputs
        assert_eq!(sig.inputs, 16);
        // Return u256 = 8 outputs
        assert_eq!(sig.outputs, 8);
    }

    #[test]
    fn test_parse_signature_u64_params() {
        let sig = parse_procedure_signature("proc add_u64(a: u64, b: u64) -> u64");
        assert!(sig.is_some());
        let sig = sig.unwrap();
        // u64 = 2 elements each, so 2 params = 4 inputs
        assert_eq!(sig.inputs, 4);
        // Return u64 = 2 outputs
        assert_eq!(sig.outputs, 2);
    }

    #[test]
    fn test_parse_signature_u128_params() {
        let sig = parse_procedure_signature("proc transform(x: u128) -> u128");
        assert!(sig.is_some());
        let sig = sig.unwrap();
        // u128 = 4 elements
        assert_eq!(sig.inputs, 4);
        assert_eq!(sig.outputs, 4);
    }

    #[test]
    fn test_parse_signature_mixed_types() {
        let sig = parse_procedure_signature("proc mixed(a: u64, b: u128, c: u256) -> u64");
        assert!(sig.is_some());
        let sig = sig.unwrap();
        // u64=2 + u128=4 + u256=8 = 14 inputs
        assert_eq!(sig.inputs, 14);
        // Return u64 = 2 outputs
        assert_eq!(sig.outputs, 2);
    }

    #[test]
    fn test_parse_signature_no_types() {
        // No type annotations - should return None
        let sig = parse_procedure_signature("proc simple");
        assert!(sig.is_none());
    }

    #[test]
    fn test_parse_signature_no_params_with_return() {
        let sig = parse_procedure_signature("proc get_value() -> u256");
        assert!(sig.is_some());
        let sig = sig.unwrap();
        assert_eq!(sig.inputs, 0);
        assert_eq!(sig.outputs, 8);
    }

    #[test]
    fn test_parse_signature_unknown_type_defaults() {
        // Unknown types default to 1 element
        let sig = parse_procedure_signature("proc foo(x: felt) -> felt");
        assert!(sig.is_some());
        let sig = sig.unwrap();
        assert_eq!(sig.inputs, 1);
        assert_eq!(sig.outputs, 1);
    }

    #[test]
    fn test_type_to_element_count() {
        assert_eq!(type_to_element_count("u64"), 2);
        assert_eq!(type_to_element_count("u128"), 4);
        assert_eq!(type_to_element_count("u256"), 8);
        assert_eq!(type_to_element_count("felt"), 1);
        assert_eq!(type_to_element_count("unknown"), 1);
    }
}
