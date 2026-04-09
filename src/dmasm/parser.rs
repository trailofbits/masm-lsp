//! DMASM parser — procedure and variable extraction.
//!
//! Parses decompiled MASM pseudocode text to extract procedure boundaries,
//! parameter signatures, and variable occurrence locations. This is a
//! lightweight text-based scanner, not a full grammar parser.

use tower_lsp::lsp_types::{Position, Range};

/// A variable occurrence in DMASM text.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariableOccurrence {
    /// The full variable token (e.g., `"v_0"`, `"v_i"`, `"v_(2 + i)"`).
    pub name: String,
    /// The text range of this occurrence.
    pub range: Range,
}

/// A variable defined in a procedure signature.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParameterDefinition {
    /// The variable name (e.g., `"v_0"`).
    pub name: String,
    /// The declared type (e.g., `"Felt"`, `"Bool"`, `"U32"`).
    pub type_name: Option<String>,
    /// The text range of the variable name in the signature.
    pub range: Range,
}

/// A parsed DMASM procedure.
#[derive(Debug, Clone)]
pub struct DmasmProcedure {
    /// The procedure name (e.g., `"add_and_square"`).
    pub name: String,
    /// The text range of the procedure name.
    pub name_range: Range,
    /// Parameters extracted from the signature.
    pub parameters: Vec<ParameterDefinition>,
    /// All variable occurrences within the procedure (signature + body).
    pub variables: Vec<VariableOccurrence>,
    /// Full range of the procedure (from `proc` keyword to closing `}`).
    pub range: Range,
}

/// Parsed information for a DMASM document.
#[derive(Debug, Clone, Default)]
pub struct DmasmDocument {
    /// Procedures found in the document, in source order.
    pub procedures: Vec<DmasmProcedure>,
}

/// Parse a DMASM text into a [`DmasmDocument`].
///
/// Scans line-by-line to find `proc` declarations and tracks brace depth
/// to determine procedure boundaries.
pub fn parse_dmasm(text: &str) -> DmasmDocument {
    let lines: Vec<&str> = text.lines().collect();
    let mut procedures = Vec::new();
    let mut i = 0;

    while i < lines.len() {
        if let Some((name, name_range, params, opening_brace_line, brace_col)) =
            try_parse_proc_header(&lines, i)
        {
            let close_line = find_matching_close_brace(&lines, opening_brace_line, brace_col + 1);
            let proc_range = Range::new(
                Position::new(i as u32, 0),
                Position::new(
                    close_line as u32,
                    lines.get(close_line).map(|l| l.len() as u32).unwrap_or(0),
                ),
            );

            let mut variables = Vec::new();

            // Collect variable occurrences from parameters in the signature.
            for param in &params {
                variables.push(VariableOccurrence {
                    name: param.name.clone(),
                    range: param.range,
                });
            }

            // Collect variable occurrences from the body (starting after the
            // opening brace, which may be on the same line as proc).
            let body_start_line = opening_brace_line;
            let body_start_col = brace_col + 1;
            for line_idx in body_start_line..=close_line {
                if line_idx < lines.len() {
                    let scan_from = if line_idx == body_start_line {
                        body_start_col
                    } else {
                        0
                    };
                    collect_variables_in_line(
                        lines[line_idx],
                        line_idx as u32,
                        scan_from,
                        &mut variables,
                    );
                }
            }

            procedures.push(DmasmProcedure {
                name,
                name_range,
                parameters: params,
                variables,
                range: proc_range,
            });

            i = close_line + 1;
        } else {
            i += 1;
        }
    }

    DmasmDocument { procedures }
}

/// Try to parse a `proc` header starting at line `start`.
///
/// Returns `(name, name_range, parameters, opening_brace_line, brace_col)`
/// if successful. The header may span multiple lines (the opening `{` can
/// be on a later line).
fn try_parse_proc_header(
    lines: &[&str],
    start: usize,
) -> Option<(String, Range, Vec<ParameterDefinition>, usize, usize)> {
    let line = lines.get(start)?;
    let trimmed = line.trim_start();
    if !trimmed.starts_with("proc ") {
        return None;
    }

    let proc_keyword_col = line.len() - trimmed.len();
    let after_proc = &trimmed["proc ".len()..];

    // Extract procedure name (identifier chars and `::` for qualified names).
    let name_len = after_proc
        .find(|c: char| !c.is_ascii_alphanumeric() && c != '_' && c != ':')
        .unwrap_or(after_proc.len());
    if name_len == 0 {
        return None;
    }
    let name = after_proc[..name_len].to_string();
    let name_col = proc_keyword_col + "proc ".len();
    let name_range = Range::new(
        Position::new(start as u32, name_col as u32),
        Position::new(start as u32, (name_col + name_len) as u32),
    );

    // Collect the full header text across lines until we find `{`.
    let mut header_text = String::from(*line);
    let mut brace_line = start;
    let mut brace_col = line.find('{');
    if brace_col.is_none() {
        for (j, line_j) in lines.iter().enumerate().skip(start + 1) {
            header_text.push('\n');
            header_text.push_str(line_j);
            if let Some(col) = line_j.find('{') {
                brace_line = j;
                brace_col = Some(col);
                break;
            }
        }
    }

    let brace_col = brace_col?;

    // Extract parameters from the parenthesized section.
    let params = parse_parameters(&header_text, start);

    Some((name, name_range, params, brace_line, brace_col))
}

/// Parse parameter list from a header string like `proc name(v_0: Felt, v_1: U32) -> ...`.
fn parse_parameters(header: &str, header_start_line: usize) -> Vec<ParameterDefinition> {
    let Some(open_paren) = header.find('(') else {
        return Vec::new();
    };
    let Some(close_paren) = header[open_paren..].find(')') else {
        return Vec::new();
    };
    let params_str = &header[open_paren + 1..open_paren + close_paren];
    if params_str.trim().is_empty() {
        return Vec::new();
    }

    let mut results = Vec::new();
    // Byte offset of the params section start within the full header.
    let params_section_start = open_paren + 1;

    for segment in params_str.split(',') {
        let trimmed = segment.trim();
        if trimmed.is_empty() {
            continue;
        }

        // Find position of this segment in the header.
        // Safe: split() yields subslices of the original string, so pointer
        // subtraction gives the offset within params_str.
        let segment_offset_in_params = segment.as_ptr() as usize - params_str.as_ptr() as usize;
        let segment_start_in_header = params_section_start + segment_offset_in_params;

        // Find the variable name within the segment.
        let var_start_in_segment = segment.len() - segment.trim_start().len();
        let colon_pos = trimmed.find(':');
        let var_name = colon_pos
            .map(|pos| trimmed[..pos].trim())
            .unwrap_or(trimmed);
        let type_name = colon_pos.map(|pos| trimmed[pos + 1..].trim().to_string());

        // Compute the line and column of the variable name.
        let var_offset_in_header = segment_start_in_header + var_start_in_segment;
        let (line, col) = offset_to_line_col(header, var_offset_in_header);
        let abs_line = header_start_line as u32 + line;

        results.push(ParameterDefinition {
            name: var_name.to_string(),
            type_name,
            range: Range::new(
                Position::new(abs_line, col),
                Position::new(abs_line, col + var_name.len() as u32),
            ),
        });
    }

    results
}

/// Find the line that contains the matching close brace for an opening `{`.
///
/// Starts scanning from `(start_line, start_col)` — the position immediately
/// after the opening `{`. This handles single-line bodies where `{` and `}`
/// are on the same line.
fn find_matching_close_brace(lines: &[&str], start_line: usize, start_col: usize) -> usize {
    let mut depth: i32 = 1;
    for (i, line) in lines.iter().enumerate().skip(start_line) {
        let from = if i == start_line { start_col } else { 0 };
        for ch in line[from..].chars() {
            match ch {
                '{' => depth += 1,
                '}' => {
                    depth -= 1;
                    if depth == 0 {
                        return i;
                    }
                }
                _ => {}
            }
        }
    }
    // If no matching brace found, return last line.
    lines.len().saturating_sub(1)
}

/// DMASM keywords that should not be collected as variable occurrences.
const DMASM_KEYWORDS: &[&str] = &[
    "call", "else", "exec", "false", "for", "if", "in", "let", "memory", "proc", "return",
    "syscall", "true", "use", "while",
];

/// Check whether an identifier is a DMASM keyword.
fn is_dmasm_keyword(name: &str) -> bool {
    DMASM_KEYWORDS.contains(&name)
}

/// Collect all variable occurrences in a single line.
///
/// Scans for all identifiers (alphabetic start, followed by alphanumeric or
/// `_`) and collects those that are not keywords or qualified `::` paths.
/// The `v_(expr)` complex subscript form is handled as a special case.
/// Scanning starts at `from_col` (byte offset) to support skipping the
/// header portion on the opening brace line.
fn collect_variables_in_line(
    line: &str,
    line_num: u32,
    from_col: usize,
    out: &mut Vec<VariableOccurrence>,
) {
    let bytes = line.as_bytes();
    let mut pos = from_col;

    while pos < bytes.len() {
        // Only start scanning at alphabetic characters. This avoids matching
        // operator suffixes like `_u32` in `<=_u32`.
        if !bytes[pos].is_ascii_alphabetic() {
            pos += 1;
            continue;
        }

        // Word boundary: the preceding character must not be alphanumeric
        // or `_`, so we don't match inside a longer token.
        if pos > 0 {
            let prev = bytes[pos - 1];
            if prev.is_ascii_alphanumeric() || prev == b'_' {
                pos += 1;
                continue;
            }
        }

        // Special case: `v_(expr)` complex subscript variables.
        if bytes[pos] == b'v'
            && pos + 2 < bytes.len()
            && bytes[pos + 1] == b'_'
            && bytes[pos + 2] == b'('
        {
            if let Some(close) = find_matching_paren(line, pos + 2) {
                let end = close + 1;
                out.push(VariableOccurrence {
                    name: line[pos..end].to_string(),
                    range: Range::new(
                        Position::new(line_num, pos as u32),
                        Position::new(line_num, end as u32),
                    ),
                });
                pos = end;
            } else {
                pos += 3;
            }
            continue;
        }

        // General identifier: extract the full token.
        let ident_start = pos;
        while pos < bytes.len() && (bytes[pos].is_ascii_alphanumeric() || bytes[pos] == b'_') {
            pos += 1;
        }
        let ident = &line[ident_start..pos];

        // Skip qualified paths (e.g. `std::crypto::sha256::hash`).
        if pos + 1 < bytes.len() && bytes[pos] == b':' && bytes[pos + 1] == b':' {
            while pos + 1 < bytes.len() && bytes[pos] == b':' && bytes[pos + 1] == b':' {
                pos += 2;
                while pos < bytes.len()
                    && (bytes[pos].is_ascii_alphanumeric() || bytes[pos] == b'_')
                {
                    pos += 1;
                }
            }
            continue;
        }

        // Skip keywords.
        if is_dmasm_keyword(ident) {
            continue;
        }

        // Collect as a variable occurrence.
        out.push(VariableOccurrence {
            name: ident.to_string(),
            range: Range::new(
                Position::new(line_num, ident_start as u32),
                Position::new(line_num, pos as u32),
            ),
        });
    }
}

/// Find the position of the matching `)` for a `(` at `open_pos`.
fn find_matching_paren(line: &str, open_pos: usize) -> Option<usize> {
    let mut depth = 0;
    for (i, ch) in line[open_pos..].char_indices() {
        match ch {
            '(' => depth += 1,
            ')' => {
                depth -= 1;
                if depth == 0 {
                    return Some(open_pos + i);
                }
            }
            _ => {}
        }
    }
    None
}

/// Convert a byte offset in a string to a (line, column) pair.
///
/// Lines are separated by `\n`. Both line and column are 0-indexed.
fn offset_to_line_col(text: &str, offset: usize) -> (u32, u32) {
    let mut line = 0u32;
    let mut col = 0u32;
    for (i, ch) in text.char_indices() {
        if i == offset {
            return (line, col);
        }
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }
    (line, col)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fixture(name: &str) -> String {
        let path = format!("{}/tests/fixtures/{name}", env!("CARGO_MANIFEST_DIR"));
        std::fs::read_to_string(path).expect("fixture file should exist")
    }

    #[test]
    fn parse_finds_all_procedures_in_simple_fixture() {
        let text = fixture("simple.dmasm");
        let doc = parse_dmasm(&text);
        assert_eq!(doc.procedures.len(), 2);
        assert_eq!(doc.procedures[0].name, "add_and_square");
        assert_eq!(doc.procedures[1].name, "check_bound");
    }

    #[test]
    fn parse_finds_all_procedures_in_multi_var_fixture() {
        let text = fixture("multi_var.dmasm");
        let doc = parse_dmasm(&text);
        assert_eq!(doc.procedures.len(), 3);
        assert_eq!(doc.procedures[0].name, "hash_loop");
        assert_eq!(doc.procedures[1].name, "constant_proc");
        assert_eq!(doc.procedures[2].name, "multi_use");
    }

    #[test]
    fn procedure_range_spans_from_proc_keyword_to_closing_brace() {
        let text = fixture("simple.dmasm");
        let doc = parse_dmasm(&text);
        let first = &doc.procedures[0];
        // "proc add_and_square..." starts on line 2 (0-indexed)
        assert_eq!(first.range.start.line, 2);
        // Closing brace is on line 6
        assert_eq!(first.range.end.line, 6);
    }

    #[test]
    fn parse_extracts_parameters_with_types() {
        let text = fixture("simple.dmasm");
        let doc = parse_dmasm(&text);
        let first = &doc.procedures[0];
        assert_eq!(first.parameters.len(), 2);
        assert_eq!(first.parameters[0].name, "v_0");
        assert_eq!(first.parameters[0].type_name.as_deref(), Some("Felt"));
        assert_eq!(first.parameters[1].name, "v_1");
        assert_eq!(first.parameters[1].type_name.as_deref(), Some("Felt"));
    }

    #[test]
    fn parse_extracts_parameters_with_different_types() {
        let text = fixture("simple.dmasm");
        let doc = parse_dmasm(&text);
        let second = &doc.procedures[1];
        assert_eq!(second.parameters.len(), 2);
        assert_eq!(second.parameters[0].name, "v_0");
        assert_eq!(second.parameters[0].type_name.as_deref(), Some("U32"));
        assert_eq!(second.parameters[1].name, "v_1");
        assert_eq!(second.parameters[1].type_name.as_deref(), Some("U32"));
    }

    #[test]
    fn parameterless_procedure_has_empty_parameters() {
        let text = fixture("multi_var.dmasm");
        let doc = parse_dmasm(&text);
        let constant_proc = &doc.procedures[1];
        assert_eq!(constant_proc.name, "constant_proc");
        assert!(constant_proc.parameters.is_empty());
    }

    #[test]
    fn single_line_procedure_body() {
        let text = "proc nop() { }";
        let doc = parse_dmasm(text);
        assert_eq!(doc.procedures.len(), 1);
        assert_eq!(doc.procedures[0].name, "nop");
        assert_eq!(doc.procedures[0].range.start.line, 0);
        assert_eq!(doc.procedures[0].range.end.line, 0);
    }

    #[test]
    fn parse_collects_all_simple_variables_in_procedure() {
        let text = fixture("simple.dmasm");
        let doc = parse_dmasm(&text);
        let proc = &doc.procedures[0]; // add_and_square
        let var_names: Vec<&str> = proc.variables.iter().map(|v| v.name.as_str()).collect();
        // Parameters (v_0, v_1) + body occurrences (v_2, v_0, v_1, v_3, v_2, v_2, v_3)
        assert!(var_names.contains(&"v_0"));
        assert!(var_names.contains(&"v_1"));
        assert!(var_names.contains(&"v_2"));
        assert!(var_names.contains(&"v_3"));
    }

    #[test]
    fn parse_collects_complex_subscript_variables() {
        let text = fixture("multi_var.dmasm");
        let doc = parse_dmasm(&text);
        let proc = &doc.procedures[0]; // hash_loop
        let var_names: Vec<&str> = proc.variables.iter().map(|v| v.name.as_str()).collect();
        assert!(var_names.contains(&"v_(2 + i)"));
    }

    #[test]
    fn variables_have_correct_ranges() {
        // Minimal inline test for precise range checking.
        let text = "proc foo(v_0: Felt) -> Felt {\n  v_1 = v_0 + 1;\n  return v_1;\n}";
        let doc = parse_dmasm(text);
        assert_eq!(doc.procedures.len(), 1);
        let proc = &doc.procedures[0];

        // Find v_1 on line 1 (the assignment target).
        let v1_first = proc
            .variables
            .iter()
            .find(|v| v.name == "v_1" && v.range.start.line == 1)
            .expect("v_1 should appear on line 1");
        assert_eq!(v1_first.range.start.character, 2); // "  v_1"
        assert_eq!(v1_first.range.end.character, 5);
    }

    #[test]
    fn all_occurrences_of_a_variable_are_collected() {
        let text = fixture("multi_var.dmasm");
        let doc = parse_dmasm(&text);
        let proc = &doc.procedures[2]; // multi_use
        let v0_count = proc.variables.iter().filter(|v| v.name == "v_0").count();
        // v_0 appears in: parameter(1) + v_0+v_0(2) + v_1*v_0(1) + v_0=...(1) + return v_0(1) = 6
        assert_eq!(
            v0_count, 6,
            "expected exactly 6 occurrences of v_0, got {v0_count}"
        );
    }

    #[test]
    fn word_boundary_prevents_partial_matches() {
        // "some_v_0" should be collected as one identifier, not split into
        // a false "v_0" match.
        let text = "proc foo() {\n  some_v_0 = 1;\n}";
        let doc = parse_dmasm(text);
        let proc = &doc.procedures[0];
        let var_names: Vec<&str> = proc.variables.iter().map(|v| v.name.as_str()).collect();
        assert!(
            var_names.contains(&"some_v_0"),
            "should collect the full identifier"
        );
        assert!(
            !var_names.contains(&"v_0"),
            "should not split into a partial v_0 match"
        );
    }

    #[test]
    fn renamed_variable_found_in_body() {
        // After renaming v_0 → input, the parser should find "input" in body.
        let text = "proc foo(input: Felt) -> Felt {\n  v_1 = input + 1;\n  return v_1;\n}";
        let doc = parse_dmasm(text);
        let proc = &doc.procedures[0];
        let input_count = proc.variables.iter().filter(|v| v.name == "input").count();
        // Parameter + one body usage.
        assert_eq!(
            input_count, 2,
            "should find 'input' in both parameter and body"
        );
    }

    #[test]
    fn renamed_body_local_found_as_identifier() {
        // After renaming v_1 → sum, body-local "sum" should be collected.
        let text = "proc foo(v_0: Felt) -> Felt {\n  sum = v_0 + 1;\n  return sum;\n}";
        let doc = parse_dmasm(text);
        let proc = &doc.procedures[0];
        let sum_count = proc.variables.iter().filter(|v| v.name == "sum").count();
        assert_eq!(sum_count, 2, "should find 'sum' in assignment and return");
    }

    #[test]
    fn keywords_not_collected_as_variables() {
        let text = "proc foo(v_0: Felt) -> Felt {\n  if (v_0) {\n    return true;\n  } else {\n    return false;\n  }\n}";
        let doc = parse_dmasm(text);
        let proc = &doc.procedures[0];
        let var_names: Vec<&str> = proc.variables.iter().map(|v| v.name.as_str()).collect();
        for kw in &["if", "return", "true", "false", "else"] {
            assert!(
                !var_names.contains(kw),
                "keyword '{kw}' should not be collected as a variable"
            );
        }
    }

    #[test]
    fn qualified_paths_not_collected_as_variables() {
        let text = "proc foo() {\n  (v_0, v_1) = exec std::crypto::sha256::hash(v_2);\n}";
        let doc = parse_dmasm(text);
        let proc = &doc.procedures[0];
        let var_names: Vec<&str> = proc.variables.iter().map(|v| v.name.as_str()).collect();
        assert!(
            !var_names.contains(&"std"),
            "should skip qualified path component 'std'"
        );
        assert!(
            !var_names.contains(&"crypto"),
            "should skip qualified path component 'crypto'"
        );
        assert!(
            !var_names.contains(&"sha256"),
            "should skip qualified path component 'sha256'"
        );
        assert!(
            !var_names.contains(&"hash"),
            "should skip qualified path component 'hash'"
        );
        // But the variables should still be found.
        assert!(var_names.contains(&"v_0"));
        assert!(var_names.contains(&"v_1"));
        assert!(var_names.contains(&"v_2"));
    }

    #[test]
    fn operator_suffix_not_collected() {
        // The `_u32` suffix in `<=_u32` should not be collected.
        let text =
            "proc foo(v_0: U32, v_1: U32) -> Bool {\n  v_2 = (v_0 <=_u32 v_1);\n  return v_2;\n}";
        let doc = parse_dmasm(text);
        let proc = &doc.procedures[0];
        let var_names: Vec<&str> = proc.variables.iter().map(|v| v.name.as_str()).collect();
        assert!(
            !var_names.iter().any(|n| *n == "_u32" || *n == "u32"),
            "should not collect operator suffix as variable, got: {var_names:?}"
        );
    }

    #[test]
    fn loop_variable_collected() {
        let text = "proc foo() {\n  for i in 0..4 {\n    v_0 = i;\n  }\n}";
        let doc = parse_dmasm(text);
        let proc = &doc.procedures[0];
        let i_count = proc.variables.iter().filter(|v| v.name == "i").count();
        assert_eq!(
            i_count, 2,
            "loop variable 'i' should be found on for-line and in body"
        );
    }
}
