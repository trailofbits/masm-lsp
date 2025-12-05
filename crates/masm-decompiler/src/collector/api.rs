use std::collections::HashMap;

use masm_analysis::{infer_module_contracts, ContractStore};
use miden_assembly_syntax::ast::visit;
use miden_assembly_syntax::ast::Module;
use miden_debug_types::DefaultSourceManager;
use tower_lsp::lsp_types::{
    Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, InlayHint, InlayHintKind,
    InlayHintLabel, Location, Position, Range, Url,
};

use crate::range::span_to_range;
use crate::SOURCE_DECOMPILATION;

use super::pseudocode_collector::PseudocodeCollector;

/// Result of decompilation hint collection.
pub struct DecompilationResult {
    /// Inlay hints for pseudocode
    pub hints: Vec<InlayHint>,
    /// Diagnostics for tracking failures
    pub diagnostics: Vec<Diagnostic>,
}

/// Collect decompilation hints for all procedures in a module.
///
/// This function uses SSA (Static Single Assignment) form with phi nodes to track
/// variable identity across control flow, producing consistent variable names in loops.
pub fn collect_decompilation_hints(
    module: &Module,
    sources: &DefaultSourceManager,
    uri: &Url,
    visible_range: &Range,
    tab_count: usize,
    source_text: &str,
    contracts: Option<&ContractStore>,
) -> DecompilationResult {
    let mut owned_contracts = None;
    let contracts = match contracts {
        Some(c) => Some(c),
        None => {
            let inferred = infer_module_contracts(module, sources);
            let mut store = ContractStore::new();
            store.update_document(inferred);
            owned_contracts = Some(store);
            owned_contracts.as_ref()
        }
    };

    let contracts_are_fallback = owned_contracts.is_some();
    let mut collector = PseudocodeCollector::new(
        module,
        sources,
        contracts,
        source_text,
        contracts_are_fallback,
    );
    let _ = visit::visit_module(&mut collector, module);

    let padding = (tab_count.max(1)) as u32;
    let line_lengths: Vec<u32> = source_text
        .lines()
        .map(|line| line.trim_end().len() as u32)
        .collect();

    let mut line_hints: HashMap<u32, Vec<String>> = HashMap::new();
    for (line, hint) in collector.resolved_hints {
        if line >= visible_range.start.line && line < visible_range.end.line {
            line_hints.entry(line).or_default().push(hint);
        }
    }

    let mut hints: Vec<InlayHint> = line_hints
        .into_iter()
        .map(|(line_num, pseudocodes)| {
            let line_end = line_lengths.get(line_num as usize).copied().unwrap_or(0);

            let mut normalized = pseudocodes.clone();
            for code in normalized.iter_mut().skip(1) {
                *code = code.trim_start().to_string();
            }
            let label = normalized.join("; ");

            InlayHint {
                position: Position {
                    line: line_num,
                    character: line_end.saturating_add(padding),
                },
                label: InlayHintLabel::String(label),
                kind: Some(InlayHintKind::TYPE),
                text_edits: None,
                tooltip: None,
                padding_left: None,
                padding_right: None,
                data: None,
            }
        })
        .collect();

    hints.sort_by(|a, b| {
        a.position
            .line
            .cmp(&b.position.line)
            .then_with(|| a.position.character.cmp(&b.position.character))
    });

    let diagnostics: Vec<Diagnostic> = collector
        .failures
        .into_iter()
        .filter_map(|failure| {
            let range = span_to_range(sources, failure.span)?;

            let related_information = failure.related.and_then(|related| {
                let related_range = span_to_range(sources, related.span)?;
                Some(vec![DiagnosticRelatedInformation {
                    location: Location {
                        uri: uri.clone(),
                        range: related_range,
                    },
                    message: related.message,
                }])
            });

            Some(Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::WARNING),
                code: None,
                code_description: None,
                source: Some(SOURCE_DECOMPILATION.to_string()),
                message: format!(
                    "Pseudocode unavailable in `{}`: {}",
                    failure.proc_name, failure.reason
                ),
                related_information,
                tags: None,
                data: None,
            })
        })
        .collect();

    DecompilationResult { hints, diagnostics }
}
