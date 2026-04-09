//! Reusable analysis passes for MASM LSP.

use std::path::PathBuf;
use std::sync::Arc;

use masm_decompiler::{
    callgraph::CallGraph,
    frontend::{LibraryRoot, Program, Workspace},
    signature::{infer_signatures, ProcSignature},
    types::infer_type_summaries,
    Decompiler, SymbolPath,
};
use miden_assembly_syntax::ast::{
    types::Type as AstType, FunctionType, Module, SymbolResolutionError, TypeResolver,
};
use miden_debug_types::{DefaultSourceManager, SourceSpan, Spanned};

pub mod prepared;
mod unconstrained_advice;
mod uninitialized_locals;

pub use unconstrained_advice::{
    infer_unconstrained_advice, infer_unconstrained_advice_in_workspace, AdviceDiagnostic,
    AdviceDiagnosticsMap, AdviceSinkKind, AdviceSummary, AdviceSummaryMap,
};
pub use uninitialized_locals::{
    infer_uninitialized_locals_in_workspace, LocalInitDiagnostic, LocalInitDiagnosticKind,
    LocalInitDiagnosticsMap, LocalInitSummary, LocalInitSummaryMap,
};

pub use masm_decompiler::signature::SignatureMap;
pub use masm_decompiler::types::{
    InferredType, TypeDiagnostic, TypeDiagnosticsMap, TypeRequirement, TypeSummary, TypeSummaryMap,
};

/// Results of running all analysis passes on a workspace.
#[derive(Debug)]
pub struct AnalysisSnapshot {
    /// Inferred procedure signatures.
    pub signatures: SignatureMap,
    /// Inferred procedure type summaries.
    pub type_summaries: TypeSummaryMap,
    /// Type inconsistency diagnostics.
    pub type_diagnostics: TypeDiagnosticsMap,
    /// Unconstrained advice flow diagnostics.
    pub advice_diagnostics: AdviceDiagnosticsMap,
    /// Uninitialized local read diagnostics.
    pub local_init_diagnostics: LocalInitDiagnosticsMap,
    /// Module paths that could not be resolved.
    pub unresolved_modules: Vec<SymbolPath>,
}

impl AnalysisSnapshot {
    /// Run all analysis passes on a workspace and return the combined results.
    pub fn from_workspace(workspace: &Workspace) -> Self {
        let unresolved_modules = workspace.unresolved_module_paths();

        let callgraph = CallGraph::from(workspace);
        let signatures = infer_signatures(workspace, &callgraph);
        let (type_summaries, type_diagnostics) =
            infer_type_summaries(workspace, &callgraph, &signatures);
        let (_, advice_diagnostics) =
            infer_unconstrained_advice(workspace, &callgraph, &signatures, &type_summaries);
        let (_, local_init_diagnostics) =
            uninitialized_locals::inter::infer_uninitialized_locals_with_inputs(
                workspace,
                &callgraph,
                &signatures,
                &type_summaries,
            );

        Self {
            signatures,
            type_summaries,
            type_diagnostics,
            advice_diagnostics,
            local_init_diagnostics,
            unresolved_modules,
        }
    }
}

/// Stack-effect counts extracted from a procedure signature.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StackSignature {
    /// Number of inputs.
    pub inputs: usize,
    /// Number of outputs.
    pub outputs: usize,
}

/// Mismatch between declared and inferred stack signatures.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SignatureMismatch {
    /// Procedure name without module path.
    pub proc_name: String,
    /// Source span associated with the mismatch.
    pub span: SourceSpan,
    /// Declared stack signature.
    pub declared: StackSignature,
    /// Inferred stack signature.
    pub inferred: StackSignature,
}

/// Generate a human-readable message describing a signature mismatch.
pub fn signature_mismatch_message(mismatch: &SignatureMismatch) -> String {
    let inputs_diff = mismatch.declared.inputs != mismatch.inferred.inputs;
    let outputs_diff = mismatch.declared.outputs != mismatch.inferred.outputs;
    match (inputs_diff, outputs_diff) {
        (true, true) => format!(
            "the definition declares {} inputs and {} outputs, but the inferred counts are {} and {} respectively",
            mismatch.declared.inputs,
            mismatch.declared.outputs,
            mismatch.inferred.inputs,
            mismatch.inferred.outputs
        ),
        (true, false) => format!(
            "the definition declares {} inputs, but the inferred input count is {}",
            mismatch.declared.inputs, mismatch.inferred.inputs
        ),
        (false, true) => format!(
            "the definition declares {} outputs, but the inferred output count is {}",
            mismatch.declared.outputs, mismatch.inferred.outputs
        ),
        (false, false) => String::new(),
    }
}

/// Build a temporary workspace for a module and compute signature mismatches.
pub fn signature_mismatches(
    module: &Module,
    sources: Arc<DefaultSourceManager>,
    source_path: PathBuf,
    library_roots: &[LibraryRoot],
) -> Vec<SignatureMismatch> {
    let mut workspace = Workspace::with_source_manager(library_roots.to_vec(), sources.clone());
    let program = Program::from_parts(
        Box::new(module.clone()),
        source_path,
        module.path().to_path_buf(),
    );
    workspace.add_program(program);
    workspace.load_dependencies();

    signature_mismatches_in_workspace(module, sources, &workspace)
}

/// Compute signature mismatches for a module within an existing workspace.
pub fn signature_mismatches_in_workspace(
    module: &Module,
    sources: Arc<DefaultSourceManager>,
    workspace: &Workspace,
) -> Vec<SignatureMismatch> {
    let decompiler = Decompiler::new(workspace);
    let signatures = decompiler.signatures();
    let resolver = module.type_resolver(sources.clone());

    let mut findings = Vec::new();
    for proc in module.procedures() {
        let Some(signature) = proc.signature() else {
            continue;
        };
        let Some(declared) = signature_stack_signature(signature, &resolver) else {
            continue;
        };

        let symbol_path = SymbolPath::from_module_and_name(module, proc.name().as_str());
        let Some(inferred) = signatures.get(&symbol_path) else {
            continue;
        };
        let (inputs, outputs) = match inferred {
            ProcSignature::Known {
                inputs, outputs, ..
            } => (*inputs, *outputs),
            ProcSignature::Unknown => continue,
        };

        if declared.inputs != inputs || declared.outputs != outputs {
            let span = {
                let sig_span = signature.span();
                if sig_span == SourceSpan::UNKNOWN {
                    proc.name().span()
                } else {
                    sig_span
                }
            };
            findings.push(SignatureMismatch {
                proc_name: proc.name().as_str().to_string(),
                span,
                declared,
                inferred: StackSignature { inputs, outputs },
            });
        }
    }

    findings
}

/// Compute signature mismatches using a pre-computed signature map.
///
/// This avoids rebuilding the call graph and signatures from scratch, unlike
/// [`signature_mismatches_in_workspace`] which creates a new [`Decompiler`]
/// internally. Use this when an [`AnalysisSnapshot`] is already available.
pub fn signature_mismatches_from_snapshot(
    module: &Module,
    sources: Arc<DefaultSourceManager>,
    signatures: &SignatureMap,
) -> Vec<SignatureMismatch> {
    let resolver = module.type_resolver(sources);

    let mut findings = Vec::new();
    for proc in module.procedures() {
        let Some(signature) = proc.signature() else {
            continue;
        };
        let Some(declared) = signature_stack_signature(signature, &resolver) else {
            continue;
        };

        let symbol_path = SymbolPath::from_module_and_name(module, proc.name().as_str());
        let Some(inferred) = signatures.get(&symbol_path) else {
            continue;
        };
        let (inputs, outputs) = match inferred {
            ProcSignature::Known {
                inputs, outputs, ..
            } => (*inputs, *outputs),
            ProcSignature::Unknown => continue,
        };

        if declared.inputs != inputs || declared.outputs != outputs {
            let span = {
                let sig_span = signature.span();
                if sig_span == SourceSpan::UNKNOWN {
                    proc.name().span()
                } else {
                    sig_span
                }
            };
            findings.push(SignatureMismatch {
                proc_name: proc.name().as_str().to_string(),
                span,
                declared,
                inferred: StackSignature { inputs, outputs },
            });
        }
    }

    findings
}

/// Compute the analysis inputs needed by the advice pass in one place.
pub fn analysis_inputs(workspace: &Workspace) -> (CallGraph, SignatureMap, TypeSummaryMap) {
    let callgraph = CallGraph::from(workspace);
    let signatures = infer_signatures(workspace, &callgraph);
    let (type_summaries, _) = infer_type_summaries(workspace, &callgraph, &signatures);
    (callgraph, signatures, type_summaries)
}

fn signature_stack_signature<R>(signature: &FunctionType, resolver: &R) -> Option<StackSignature>
where
    R: TypeResolver<SymbolResolutionError>,
{
    let mut inputs = 0usize;
    for arg in signature.args.iter() {
        let ty = arg.resolve_type(resolver).ok().flatten()?;
        let felts = type_felts(&ty)?;
        inputs = inputs.checked_add(felts)?;
    }

    let mut outputs = 0usize;
    for result in signature.results.iter() {
        let ty = result.resolve_type(resolver).ok().flatten()?;
        let felts = type_felts(&ty)?;
        outputs = outputs.checked_add(felts)?;
    }

    Some(StackSignature { inputs, outputs })
}

fn type_felts(ty: &AstType) -> Option<usize> {
    match ty {
        AstType::Unknown | AstType::Never | AstType::List(_) => None,
        _ => Some(ty.size_in_felts()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn signature_mismatch_message_both_differ() {
        let m = SignatureMismatch {
            proc_name: "foo".into(),
            span: SourceSpan::UNKNOWN,
            declared: StackSignature {
                inputs: 2,
                outputs: 1,
            },
            inferred: StackSignature {
                inputs: 3,
                outputs: 2,
            },
        };
        let msg = signature_mismatch_message(&m);
        assert!(msg.contains("2 inputs and 1 outputs"));
        assert!(msg.contains("3 and 2 respectively"));
    }

    #[test]
    fn signature_mismatch_message_inputs_only() {
        let m = SignatureMismatch {
            proc_name: "bar".into(),
            span: SourceSpan::UNKNOWN,
            declared: StackSignature {
                inputs: 2,
                outputs: 1,
            },
            inferred: StackSignature {
                inputs: 3,
                outputs: 1,
            },
        };
        let msg = signature_mismatch_message(&m);
        assert!(msg.contains("declares 2 inputs"));
        assert!(msg.contains("inferred input count is 3"));
    }

    #[test]
    fn signature_mismatch_message_outputs_only() {
        let m = SignatureMismatch {
            proc_name: "baz".into(),
            span: SourceSpan::UNKNOWN,
            declared: StackSignature {
                inputs: 1,
                outputs: 2,
            },
            inferred: StackSignature {
                inputs: 1,
                outputs: 3,
            },
        };
        let msg = signature_mismatch_message(&m);
        assert!(msg.contains("declares 2 outputs"));
        assert!(msg.contains("inferred output count is 3"));
    }

    #[test]
    fn signature_mismatch_message_no_diff_is_empty() {
        let m = SignatureMismatch {
            proc_name: "qux".into(),
            span: SourceSpan::UNKNOWN,
            declared: StackSignature {
                inputs: 1,
                outputs: 1,
            },
            inferred: StackSignature {
                inputs: 1,
                outputs: 1,
            },
        };
        assert!(signature_mismatch_message(&m).is_empty());
    }

    #[test]
    fn snapshot_includes_local_init_diagnostics() {
        use masm_decompiler::frontend::testing::workspace_from_modules;

        let ws = workspace_from_modules(&[(
            "test",
            "@locals(1)\nproc bad\n  loc_load.0\n  drop\nend\n",
        )]);
        let snapshot = AnalysisSnapshot::from_workspace(&ws);
        assert!(
            !snapshot.local_init_diagnostics.is_empty(),
            "expected local_init_diagnostics in snapshot, got empty"
        );
        let diags: Vec<_> = snapshot.local_init_diagnostics.values().flatten().collect();
        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("read before initialization")),
            "expected a 'read before initialization' diagnostic, got: {:?}",
            diags.iter().map(|d| &d.message).collect::<Vec<_>>()
        );
    }
}
