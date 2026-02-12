use std::path::PathBuf;
use std::sync::Arc;

use masm_decompiler::{
    fmt::{CodeWriter, FormattingConfig},
    frontend::{LibraryRoot, Program, Workspace},
    lift::LiftingError,
    DecompilationError, Decompiler,
};
use masm_instructions::ToDescription;
use miden_assembly_syntax::ast::{Block, Instruction, Module, Op};
use miden_debug_types::{DefaultSourceManager, SourceSpan, Span, Spanned};
use tower_lsp::lsp_types::{
    Diagnostic, DiagnosticSeverity, InlayHint, InlayHintKind, InlayHintLabel, NumberOrString,
    Position, Range, Url,
};

use crate::diagnostics::{normalize_message, span_to_range, SOURCE_DECOMPILATION};
use crate::{InlayHintType, LibraryPath};

pub struct InlayHintResult {
    pub hints: Vec<InlayHint>,
    pub diagnostics: Vec<Diagnostic>,
}

enum InstructionHintKind {
    Description,
}

/// Collect inlay hints for a module by decompiling each procedure.
pub fn collect_inlay_hints(
    module: &Module,
    sources: Arc<DefaultSourceManager>,
    uri: &Url,
    visible_range: &Range,
    source_text: &str,
    library_paths: &[LibraryPath],
    hint_type: InlayHintType,
) -> InlayHintResult {
    match hint_type {
        InlayHintType::Decompilation => collect_decompilation_hints(
            module,
            sources,
            uri,
            visible_range,
            source_text,
            library_paths,
        ),
        InlayHintType::Description => collect_instruction_hints(
            module,
            sources,
            visible_range,
            source_text,
            InstructionHintKind::Description,
        ),
        InlayHintType::None => InlayHintResult {
            hints: Vec::new(),
            diagnostics: Vec::new(),
        },
    }
}

fn collect_decompilation_hints(
    module: &Module,
    sources: Arc<DefaultSourceManager>,
    uri: &Url,
    visible_range: &Range,
    source_text: &str,
    library_paths: &[LibraryPath],
) -> InlayHintResult {
    let module_path = module.path().to_string();
    let source_path = uri
        .to_file_path()
        .unwrap_or_else(|_| PathBuf::from("in-memory.masm"));

    let roots: Vec<LibraryRoot> = library_paths
        .iter()
        .map(|lib| LibraryRoot::new(lib.prefix.clone(), lib.root.clone()))
        .collect();

    let mut workspace = Workspace::with_source_manager(roots, sources.clone());
    let program = Program::from_parts(
        Box::new(module.clone()),
        source_path,
        module.path().to_path_buf(),
    );
    workspace.add_program(program);
    workspace.load_dependencies();
    let unresolved_modules = workspace.unresolved_module_paths();

    let decompiler = Decompiler::new(&workspace);
    let line_lengths: Vec<u32> = source_text
        .lines()
        .map(|line| line.trim_end().len() as u32)
        .collect();
    const DEFAULT_INLAY_HINT_TABS: u32 = 2;
    let padding = DEFAULT_INLAY_HINT_TABS;

    let mut proc_infos: Vec<(Range, String, bool)> = Vec::new();
    for proc in module.procedures() {
        let Some(range) = span_to_range(sources.as_ref(), proc.name().span()) else {
            continue;
        };
        let in_range = line_in_range(range.start.line, visible_range);
        let proc_name = proc.name().as_str().to_string();
        proc_infos.push((range, proc_name, in_range));
    }

    proc_infos.sort_by_key(|(range, _, _)| range.start.line);
    let mut hints = Vec::new();
    let mut diagnostics = Vec::new();
    if !unresolved_modules.is_empty() {
        diagnostics.push(unresolved_dependency_diagnostic(
            module,
            sources.as_ref(),
            &unresolved_modules,
        ));
    }
    for idx in 0..proc_infos.len() {
        let (proc_range, proc_name, in_range) = proc_infos[idx].clone();
        let line = proc_range.start.line;

        if !in_range {
            continue;
        }

        let fq_name = if module_path.is_empty() {
            proc_name.clone()
        } else {
            format!("{}::{}", module_path, proc_name)
        };

        let decompiled = match decompiler.decompile_proc(&fq_name) {
            Ok(decompiled) => decompiled,
            Err(error) => {
                if !unresolved_modules.is_empty()
                    && should_suppress_with_unresolved_dependencies(&error)
                {
                    continue;
                }
                if let Some(diag) =
                    decompilation_error_diagnostic(sources.as_ref(), proc_range.clone(), error)
                {
                    diagnostics.push(diag);
                }
                continue;
            }
        };

        let mut writer = CodeWriter::with_config(FormattingConfig::default().with_color(false));
        writer.write(&decompiled);
        let output = writer.finish();
        let mut lines: Vec<String> = output.lines().map(|line| line.to_string()).collect();

        if let Some((next_proc_range, _, _)) = proc_infos.get(idx + 1) {
            let allowed_lines = next_proc_range.start.line as i32 - line as i32 - 1;
            if allowed_lines <= 0 {
                continue;
            }
            if lines.len() as i32 > allowed_lines {
                let allowed = allowed_lines as usize;
                if allowed == 0 {
                    continue;
                } else if allowed == 1 {
                    lines = vec!["…".to_string()];
                } else {
                    lines.truncate(allowed - 1);
                    lines.push("…".to_string());
                }
            }
        }

        if lines.is_empty() {
            continue;
        }

        let max_line = line_lengths.len().saturating_sub(1) as u32;
        for (offset, output_line) in lines.into_iter().enumerate() {
            let hint_line = line.saturating_add(offset as u32);
            if hint_line > max_line {
                break;
            }
            if !line_in_range(hint_line, visible_range) {
                continue;
            }
            let line_end = line_lengths.get(hint_line as usize).copied().unwrap_or(0);
            let position = Position {
                line: hint_line,
                character: line_end.saturating_add(padding),
            };
            hints.push(InlayHint {
                position,
                label: InlayHintLabel::String(output_line),
                kind: Some(InlayHintKind::TYPE),
                text_edits: None,
                tooltip: None,
                padding_left: None,
                padding_right: None,
                data: None,
            });
        }
    }

    InlayHintResult { hints, diagnostics }
}

fn unresolved_dependency_diagnostic(
    module: &Module,
    sources: &DefaultSourceManager,
    unresolved: &[masm_decompiler::SymbolPath],
) -> Diagnostic {
    let summary = unresolved_dependencies_summary(unresolved);
    let range = span_to_range(sources, module.span())
        .unwrap_or_else(|| Range::new(Position::new(0, 0), Position::new(0, 0)));
    Diagnostic {
        range,
        severity: Some(DiagnosticSeverity::WARNING),
        source: Some(SOURCE_DECOMPILATION.to_string()),
        message: normalize_message(&format!(
            "decompilation is incomplete: unresolved transitive module dependencies ({summary})"
        )),
        ..Default::default()
    }
}

fn unresolved_dependencies_summary(unresolved: &[masm_decompiler::SymbolPath]) -> String {
    const MAX_ITEMS: usize = 5;
    let mut modules: Vec<String> = unresolved.iter().map(ToString::to_string).collect();
    modules.sort();
    modules.dedup();
    if modules.len() <= MAX_ITEMS {
        return modules.join(", ");
    }
    let remaining = modules.len() - MAX_ITEMS;
    let shown = modules
        .into_iter()
        .take(MAX_ITEMS)
        .collect::<Vec<_>>()
        .join(", ");
    format!("{shown}, and {remaining} more")
}

fn collect_instruction_hints(
    module: &Module,
    sources: Arc<DefaultSourceManager>,
    visible_range: &Range,
    source_text: &str,
    kind: InstructionHintKind,
) -> InlayHintResult {
    const DEFAULT_INLAY_HINT_TABS: u32 = 2;
    let padding = DEFAULT_INLAY_HINT_TABS;
    let line_lengths: Vec<u32> = source_text
        .lines()
        .map(|line| line.trim_end().len() as u32)
        .collect();

    let mut instructions: Vec<(Range, Span<Instruction>)> = Vec::new();
    for proc in module.procedures() {
        let mut proc_instructions = Vec::new();
        collect_instructions(proc.body(), &mut proc_instructions);
        for inst in proc_instructions {
            let Some(range) = span_to_range(sources.as_ref(), inst.span()) else {
                continue;
            };
            instructions.push((range, inst));
        }
    }

    instructions.sort_by(|(a, _), (b, _)| {
        a.start
            .line
            .cmp(&b.start.line)
            .then_with(|| a.start.character.cmp(&b.start.character))
    });

    let mut hints = Vec::new();
    for (range, inst) in instructions {
        let line = range.end.line;
        if !line_in_range(line, visible_range) {
            continue;
        }

        let label = match kind {
            InstructionHintKind::Description => inst.to_description(),
        };
        let Some(label) = label else {
            continue;
        };

        let line_end = line_lengths
            .get(line as usize)
            .copied()
            .unwrap_or(range.end.character);
        let character = range.end.character.min(line_end).saturating_add(padding);

        hints.push(InlayHint {
            position: Position { line, character },
            label: InlayHintLabel::String(label),
            kind: Some(InlayHintKind::TYPE),
            text_edits: None,
            tooltip: None,
            padding_left: None,
            padding_right: None,
            data: None,
        });
    }

    InlayHintResult {
        hints,
        diagnostics: Vec::new(),
    }
}

fn collect_instructions(block: &Block, out: &mut Vec<Span<Instruction>>) {
    for op in block.iter() {
        match op {
            Op::Inst(inst) => out.push(inst.clone()),
            Op::If {
                then_blk, else_blk, ..
            } => {
                collect_instructions(then_blk, out);
                collect_instructions(else_blk, out);
            }
            Op::While { body, .. } | Op::Repeat { body, .. } => {
                collect_instructions(body, out);
            }
        }
    }
}

fn line_in_range(line: u32, range: &Range) -> bool {
    line >= range.start.line && line <= range.end.line
}

pub(crate) fn decompilation_error_diagnostic(
    sources: &DefaultSourceManager,
    fallback_range: Range,
    error: DecompilationError,
) -> Option<Diagnostic> {
    let (range, kind, message) = match error {
        DecompilationError::Lifting(err) => {
            let message = err.to_string();
            let kind = classify_lifting_error(&message);
            let message = lift_diagnostic_message(kind, &message);
            let range = lifting_error_span(&err)
                .and_then(|span| span_to_range(sources, span))
                .unwrap_or(fallback_range);
            (range, kind, message)
        }
        DecompilationError::ProcedureNotFound(_) | DecompilationError::ModuleNotFound(_) => {
            return None;
        }
    };

    Some(Diagnostic {
        range,
        severity: Some(DiagnosticSeverity::ERROR),
        source: Some(SOURCE_DECOMPILATION.to_string()),
        code: Some(NumberOrString::String(kind.code().to_string())),
        message: normalize_message(&message),
        ..Default::default()
    })
}

fn should_suppress_with_unresolved_dependencies(error: &DecompilationError) -> bool {
    match error {
        DecompilationError::Lifting(err) => {
            classify_lifting_error(&err.to_string()).is_call_related()
        }
        DecompilationError::ProcedureNotFound(_) | DecompilationError::ModuleNotFound(_) => false,
    }
}

#[derive(Clone, Copy)]
enum LiftingDiagnosticKind {
    CallTargetResolution,
    MissingSignature,
    UnknownSignature,
    Other,
}

impl LiftingDiagnosticKind {
    fn code(self) -> &'static str {
        match self {
            Self::CallTargetResolution => "call-target-resolution",
            Self::MissingSignature => "missing-signature",
            Self::UnknownSignature => "unknown-signature",
            Self::Other => "lifting-error",
        }
    }

    fn is_call_related(self) -> bool {
        matches!(
            self,
            Self::CallTargetResolution | Self::MissingSignature | Self::UnknownSignature
        )
    }
}

fn classify_lifting_error(message: &str) -> LiftingDiagnosticKind {
    let message = message.to_ascii_lowercase();

    if message.contains("missing inferred signature") {
        return LiftingDiagnosticKind::MissingSignature;
    }
    if message.contains("unknown inferred signature") {
        return LiftingDiagnosticKind::UnknownSignature;
    }
    if message.contains("call target")
        && (message.contains("resolve") || message.contains("unknown"))
    {
        return LiftingDiagnosticKind::CallTargetResolution;
    }

    LiftingDiagnosticKind::Other
}

fn lift_diagnostic_message(kind: LiftingDiagnosticKind, message: &str) -> String {
    match kind {
        LiftingDiagnosticKind::CallTargetResolution => {
            format!("{message}. Check library paths and unresolved imports used by this module")
        }
        LiftingDiagnosticKind::MissingSignature => {
            format!("{message}. The call resolved, but no inferred stack effect was found")
        }
        LiftingDiagnosticKind::UnknownSignature => {
            format!("{message}. The call resolved, but its inferred stack effect is unknown")
        }
        LiftingDiagnosticKind::Other => message.to_string(),
    }
}

fn lifting_error_span(error: &LiftingError) -> Option<SourceSpan> {
    match error {
        LiftingError::UnsupportedInstruction { span, .. } => Some(*span),
        LiftingError::UnbalancedIf { span } => Some(*span),
        LiftingError::NonNeutralWhile { span } => Some(*span),
        LiftingError::IncompatibleIfMerge { span } => Some(*span),
        LiftingError::UnsupportedRepeatPattern { span, .. } => Some(*span),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::{classify_lifting_error, lift_diagnostic_message, LiftingDiagnosticKind};

    #[test]
    fn classify_old_unknown_call_target_message() {
        let message = "unknown call target `foo::bar` found";
        assert!(matches!(
            classify_lifting_error(message),
            LiftingDiagnosticKind::CallTargetResolution
        ));
    }

    #[test]
    fn classify_new_unresolved_call_target_message() {
        let message = "failed to resolve call target `foo::bar`: module not loaded";
        assert!(matches!(
            classify_lifting_error(message),
            LiftingDiagnosticKind::CallTargetResolution
        ));
    }

    #[test]
    fn classify_new_missing_signature_message() {
        let message = "missing inferred signature for call target `foo::bar`";
        assert!(matches!(
            classify_lifting_error(message),
            LiftingDiagnosticKind::MissingSignature
        ));
    }

    #[test]
    fn classify_new_unknown_signature_message() {
        let message = "call target `foo::bar` has unknown inferred signature";
        assert!(matches!(
            classify_lifting_error(message),
            LiftingDiagnosticKind::UnknownSignature
        ));
    }

    #[test]
    fn call_target_messages_include_actionable_hint() {
        let message = "failed to resolve call target `foo::bar`";
        let rendered =
            lift_diagnostic_message(LiftingDiagnosticKind::CallTargetResolution, message);
        assert!(rendered.contains("Check library paths and unresolved imports"));
    }
}
