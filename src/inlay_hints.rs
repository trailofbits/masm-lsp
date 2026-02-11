use std::path::PathBuf;
use std::sync::Arc;

use masm_decompiler::{
    DecompilationError, Decompiler,
    fmt::{CodeWriter, FormattingConfig},
    frontend::{LibraryRoot, Program, Workspace},
    lift::LiftingError,
};
use masm_instructions::ToDescription;
use miden_assembly_syntax::ast::{Block, Instruction, Module, Op};
use miden_debug_types::{DefaultSourceManager, SourceSpan, Span, Spanned};
use tower_lsp::lsp_types::{
    Diagnostic, DiagnosticSeverity, InlayHint, InlayHintKind, InlayHintLabel, Position, Range,
    Url,
};

use crate::diagnostics::{SOURCE_DECOMPILATION, normalize_message, span_to_range};
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

    let decompiler = Decompiler::new(&workspace);
    let line_lengths: Vec<u32> = source_text
        .lines()
        .map(|line| line.trim_end().len() as u32)
        .collect();
    const DEFAULT_INLAY_HINT_TABS: u32 = 2;
    let padding = DEFAULT_INLAY_HINT_TABS;

    let mut proc_infos: Vec<(u32, String, bool)> = Vec::new();
    for proc in module.procedures() {
        let Some(range) = span_to_range(sources.as_ref(), proc.name().span()) else {
            continue;
        };
        let line = range.start.line;
        let in_range = line_in_range(line, visible_range);
        let proc_name = proc.name().as_str().to_string();
        proc_infos.push((line, proc_name, in_range));
    }

    proc_infos.sort_by_key(|(line, _, _)| *line);
    let mut hints = Vec::new();
    let mut diagnostics = Vec::new();
    for idx in 0..proc_infos.len() {
        let (line, proc_name, in_range) = proc_infos[idx].clone();

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
                if let Some(diag) = decompilation_error_diagnostic(sources.as_ref(), error) {
                    diagnostics.push(diag);
                }
                continue;
            }
        };

        let mut writer = CodeWriter::with_config(FormattingConfig::default().with_color(false));
        writer.write(&decompiled);
        let output = writer.finish();
        let mut lines: Vec<String> = output.lines().map(|line| line.to_string()).collect();

        if let Some((next_line, _, _)) = proc_infos.get(idx + 1) {
            let allowed_lines = *next_line as i32 - line as i32 - 1;
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

        let line_end = line_lengths.get(line as usize).copied().unwrap_or(range.end.character);
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
                then_blk,
                else_blk,
                ..
            } => {
                collect_instructions(then_blk, out);
                collect_instructions(else_blk, out);
            },
            Op::While { body, .. } | Op::Repeat { body, .. } => {
                collect_instructions(body, out);
            },
        }
    }
}

fn line_in_range(line: u32, range: &Range) -> bool {
    line >= range.start.line && line <= range.end.line
}

fn decompilation_error_diagnostic(
    sources: &DefaultSourceManager,
    error: DecompilationError,
) -> Option<Diagnostic> {
    let (span, message) = match error {
        DecompilationError::Lifting(err) => (lifting_error_span(&err), err.to_string()),
        DecompilationError::ProcedureNotFound(_) | DecompilationError::ModuleNotFound(_) => {
            return None;
        }
    };

    let range = span_to_range(sources, span)
        .unwrap_or_else(|| Range::new(Position::new(0, 0), Position::new(0, 0)));

    Some(Diagnostic {
        range,
        severity: Some(DiagnosticSeverity::ERROR),
        source: Some(SOURCE_DECOMPILATION.to_string()),
        message: normalize_message(&message),
        ..Default::default()
    })
}

fn lifting_error_span(error: &LiftingError) -> SourceSpan {
    match error {
        LiftingError::UnsupportedInstruction { span, .. } => *span,
        LiftingError::UnknownCallTarget { span, .. } => *span,
        LiftingError::UnbalancedIf { span } => *span,
        LiftingError::NonNeutralWhile { span } => *span,
        LiftingError::IncompatibleIfMerge { span } => *span,
        LiftingError::UnsupportedRepeatPattern { span, .. } => *span,
    }
}
