use crate::analysis::contracts::types::ProcSignature;
use miden_assembly_syntax::ast::Visibility;

/// Extract the whitespace prefix for a given line (0-based) from source text.
pub fn extract_declaration_prefix(source_text: &str, line: u32) -> String {
    source_text
        .lines()
        .nth(line as usize)
        .map(|l| l.chars().take_while(|c| c.is_whitespace()).collect())
        .unwrap_or_default()
}

/// Format a procedure signature string for inlay hints.
pub fn format_procedure_signature(
    decl_prefix: &str,
    proc_name: &str,
    visibility: Visibility,
    input_count: usize,
    output_count: Option<usize>,
    contract_signature: Option<&ProcSignature>,
    output_names: Option<&[String]>,
    annotate_inputs: bool,
) -> String {
    let visibility_prefix = if visibility.is_public() { "pub " } else { "" };

    let signature = if let Some(sig) = contract_signature {
        sig.format_for_display(&format!("{decl_prefix}{visibility_prefix}proc {proc_name}"))
    } else {
        let inputs = (0..input_count)
            .map(|i| {
                if annotate_inputs {
                    format!("in a_{i}: felt")
                } else {
                    format!("a_{i}: felt")
                }
            })
            .collect::<Vec<_>>()
            .join(", ");
        let outputs = output_names
            .map(|names| {
                names
                    .iter()
                    .map(|n| format!("{n}: felt"))
                    .collect::<Vec<_>>()
                    .join(", ")
            })
            .or_else(|| {
                output_count.map(|o| {
                    (0..o)
                        .map(|i| format!("r_{i}: felt"))
                        .collect::<Vec<_>>()
                        .join(", ")
                })
            })
            .unwrap_or_else(|| "?".to_string());

        format!("{decl_prefix}{visibility_prefix}proc {proc_name}({inputs}) -> ({outputs})")
    };

    format!("{signature}:")
}
