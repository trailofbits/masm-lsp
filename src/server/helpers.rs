//! Helper functions for LSP handlers.
//!
//! This module contains utility functions used by the LSP handlers for
//! source code analysis, such as detecting use statements and extracting
//! procedure signatures and documentation.

use miden_assembly_syntax::ast::{Module, ModuleKind};

/// Determine the appropriate module kind from the parsed AST.
///
/// This examines the module's structure to determine whether it should be:
/// - `Executable`: if it has an entrypoint (`begin..end` block)
/// - `Library`: otherwise (the default for modules without entrypoints)
///
/// Note: Kernel modules are not auto-detected since they require explicit
/// namespace configuration during parsing.
pub fn determine_module_kind_from_ast(module: &Module) -> ModuleKind {
    if module.has_entrypoint() {
        ModuleKind::Executable
    } else {
        ModuleKind::Library
    }
}

/// Check if the cursor position is on a `use` statement line.
pub fn is_on_use_statement(source: &str, pos: tower_lsp::lsp_types::Position) -> bool {
    if let Some(line) = source.lines().nth(pos.line as usize) {
        let trimmed = line.trim_start();
        trimmed.starts_with("use ")
    } else {
        false
    }
}

/// Extract the procedure signature (attributes + definition line) at the given line.
///
/// Returns lines like "@locals(48)" and "export.double" as a multi-line string.
pub fn extract_procedure_signature(source: &str, def_line: usize) -> Option<String> {
    let lines: Vec<&str> = source.lines().collect();
    let def = lines.get(def_line)?.trim();

    // The definition line should be the procedure declaration
    if !def.starts_with("proc")
        && !def.starts_with("pub")
        && !def.starts_with("export.")
        && !def.starts_with("begin")
        && !def.starts_with("const.")
        && !def.starts_with("const ")
    {
        return None;
    }

    let mut signature_lines = Vec::new();

    // Collect attribute lines above the definition (walking backwards)
    let mut line_idx = def_line.saturating_sub(1);
    while line_idx < lines.len() {
        let line = lines.get(line_idx).map(|l| l.trim()).unwrap_or("");
        if line.starts_with('@') {
            signature_lines.push(line.to_string());
        } else if line.is_empty() || line.starts_with("#!") || line.starts_with('#') {
            // Skip blank lines and comments, keep looking for attributes
            if !signature_lines.is_empty() {
                break; // Found attributes, stop at non-attribute
            }
        } else {
            break;
        }
        if line_idx == 0 {
            break;
        }
        line_idx -= 1;
    }

    // Reverse attributes since we collected bottom-up
    signature_lines.reverse();
    // Add the definition line
    signature_lines.push(def.to_string());

    Some(signature_lines.join("\n"))
}

/// Extract procedure attribute lines (e.g., "@locals", "@extern") above a definition.
///
/// Returns attributes in top-down order. Lines starting with comments or blank
/// lines are skipped until an attribute is found.
pub fn extract_procedure_attributes(source: &str, def_line: usize) -> Vec<String> {
    let lines: Vec<&str> = source.lines().collect();
    if def_line >= lines.len() {
        return Vec::new();
    }

    let mut attrs = Vec::new();
    let mut line_idx = def_line.saturating_sub(1);
    while line_idx < lines.len() {
        let line = lines.get(line_idx).map(|l| l.trim()).unwrap_or("");
        if line.starts_with('@') {
            attrs.push(line.to_string());
        } else if line.is_empty() || line.starts_with("#!") || line.starts_with('#') {
            if !attrs.is_empty() {
                break;
            }
        } else {
            break;
        }

        if line_idx == 0 {
            break;
        }
        line_idx -= 1;
    }

    attrs.reverse();
    attrs
}

/// Extract doc comments above a definition at the given line.
///
/// Doc comments are lines starting with "#!" immediately before the definition.
/// Skips over attributes (lines starting with "@") between comments and definition.
/// Returns the comment text with "#!" prefixes stripped.
pub fn extract_doc_comment(source: &str, def_line: usize) -> Option<String> {
    let lines: Vec<&str> = source.lines().collect();
    if def_line == 0 || def_line > lines.len() {
        return None;
    }

    let mut comment_lines = Vec::new();
    let mut line_idx = def_line.saturating_sub(1);

    // Walk backwards collecting comment lines
    loop {
        let line = lines.get(line_idx)?;
        let trimmed = line.trim();
        if trimmed.starts_with("#!") {
            // Strip the "#!" and optional space
            let comment_text = trimmed.strip_prefix("#!").unwrap_or(trimmed);
            let comment_text = comment_text.strip_prefix(' ').unwrap_or(comment_text);
            comment_lines.push(comment_text.to_string());
        } else if trimmed.is_empty() {
            // Skip blank lines between comments and definition
            if !comment_lines.is_empty() {
                break;
            }
        } else if trimmed.starts_with('@') {
            // Skip attribute lines (e.g., @locals(48), @extern(foo))
            // Continue looking for doc comments above the attribute
        } else {
            // Non-comment, non-empty, non-attribute line - stop
            break;
        }

        if line_idx == 0 {
            break;
        }
        line_idx -= 1;
    }

    if comment_lines.is_empty() {
        return None;
    }

    // Reverse since we collected bottom-up
    comment_lines.reverse();
    Some(comment_lines.join("\n"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn is_on_use_statement_true() {
        let source = "use std::crypto\nproc foo\nend\n";
        let pos = tower_lsp::lsp_types::Position::new(0, 5);
        assert!(is_on_use_statement(source, pos));
    }

    #[test]
    fn is_on_use_statement_false() {
        let source = "proc foo\nend\n";
        let pos = tower_lsp::lsp_types::Position::new(0, 0);
        assert!(!is_on_use_statement(source, pos));
    }

    #[test]
    fn extract_doc_comment_single_line() {
        let source = "#! This is a comment\nproc foo\n  nop\nend\n";
        let comment = extract_doc_comment(source, 1);
        assert_eq!(comment, Some("This is a comment".to_string()));
    }

    #[test]
    fn extract_doc_comment_multi_line() {
        let source = "#! Line one\n#! Line two\nproc foo\n  nop\nend\n";
        let comment = extract_doc_comment(source, 2);
        assert_eq!(comment, Some("Line one\nLine two".to_string()));
    }

    #[test]
    fn extract_doc_comment_no_comment() {
        let source = "proc foo\n  nop\nend\n";
        let comment = extract_doc_comment(source, 0);
        assert_eq!(comment, None);
    }

    #[test]
    fn extract_doc_comment_with_blank_line() {
        let source = "#! Comment\n\nproc foo\n  nop\nend\n";
        let comment = extract_doc_comment(source, 2);
        assert_eq!(comment, Some("Comment".to_string()));
    }

    #[test]
    fn extract_doc_comment_ignores_regular_comments() {
        let source = "# Regular comment\nproc foo\n  nop\nend\n";
        let comment = extract_doc_comment(source, 1);
        assert_eq!(comment, None);
    }

    #[test]
    fn extract_doc_comment_skips_attributes() {
        let source = "#! Comment here!\n@locals(48)\nexport.double\n";
        let comment = extract_doc_comment(source, 2);
        assert_eq!(comment, Some("Comment here!".to_string()));
    }

    #[test]
    fn extract_doc_comment_skips_multiple_attributes() {
        let source = "#! Multi-line\n#! doc comment\n@locals(16)\n@extern(foo)\nproc bar\nend\n";
        let comment = extract_doc_comment(source, 4);
        assert_eq!(comment, Some("Multi-line\ndoc comment".to_string()));
    }

    #[test]
    fn extract_signature_simple_proc() {
        let source = "proc foo\n  nop\nend\n";
        let sig = extract_procedure_signature(source, 0);
        assert_eq!(sig, Some("proc foo".to_string()));
    }

    #[test]
    fn extract_signature_export() {
        let source = "export.double\n  nop\nend\n";
        let sig = extract_procedure_signature(source, 0);
        assert_eq!(sig, Some("export.double".to_string()));
    }

    #[test]
    fn extract_signature_with_locals() {
        let source = "@locals(48)\nexport.double\n  nop\nend\n";
        let sig = extract_procedure_signature(source, 1);
        assert_eq!(sig, Some("@locals(48)\nexport.double".to_string()));
    }

    #[test]
    fn extract_signature_with_multiple_attributes() {
        let source = "@locals(16)\n@extern(foo)\nproc bar\nend\n";
        let sig = extract_procedure_signature(source, 2);
        assert_eq!(sig, Some("@locals(16)\n@extern(foo)\nproc bar".to_string()));
    }

    #[test]
    fn extract_signature_with_comment_above() {
        let source = "#! Doc comment\n@locals(48)\nexport.double\n";
        let sig = extract_procedure_signature(source, 2);
        assert_eq!(sig, Some("@locals(48)\nexport.double".to_string()));
    }

    #[test]
    fn extract_signature_pub_proc() {
        let source = "pub proc foo\n  nop\nend\n";
        let sig = extract_procedure_signature(source, 0);
        assert_eq!(sig, Some("pub proc foo".to_string()));
    }

    #[test]
    fn extract_signature_pub_proc_with_locals() {
        let source = "@locals(16)\npub proc bar\nend\n";
        let sig = extract_procedure_signature(source, 1);
        assert_eq!(sig, Some("@locals(16)\npub proc bar".to_string()));
    }

    #[test]
    fn extract_signature_constant_definition() {
        let source = "const FOO = 42\nproc foo\nend\n";
        let sig = extract_procedure_signature(source, 0);
        assert_eq!(sig, Some("const FOO = 42".to_string()));
    }

    #[test]
    fn extract_attributes_only() {
        let source = "#! Comment\n@locals(16)\n@extern(foo)\nexport.bar\n";
        let attrs = extract_procedure_attributes(source, 3);
        assert_eq!(attrs, vec!["@locals(16)", "@extern(foo)"]);
    }

    #[test]
    fn extract_attributes_none() {
        let source = "proc foo\nend\n";
        let attrs = extract_procedure_attributes(source, 0);
        assert!(attrs.is_empty());
    }
}
