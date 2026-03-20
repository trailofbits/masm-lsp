//! Reusable analysis passes for MASM LSP.

use std::path::PathBuf;
use std::sync::Arc;

use masm_decompiler::{
    callgraph::CallGraph,
    frontend::{LibraryRoot, Program, Workspace},
    signature::{infer_signatures, ProcSignature, SignatureMap},
    types::{infer_type_summaries, TypeSummaryMap},
    Decompiler, SymbolPath,
};
use miden_assembly_syntax::ast::{
    types::Type as AstType, FunctionType, Module, SymbolResolutionError, TypeResolver,
};
use miden_debug_types::{DefaultSourceManager, SourceSpan, Spanned};

mod unconstrained_advice;

pub use unconstrained_advice::{
    infer_unconstrained_advice, infer_unconstrained_advice_in_workspace, AdviceDiagnostic,
    AdviceDiagnosticsMap, AdviceSinkKind, AdviceSummary, AdviceSummaryMap,
};

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
