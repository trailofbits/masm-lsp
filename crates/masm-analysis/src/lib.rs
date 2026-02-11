use std::path::PathBuf;
use std::sync::Arc;

use masm_decompiler::{
    Decompiler,
    frontend::{LibraryRoot, Program, Workspace},
    signature::ProcSignature,
    SymbolPath,
};
use miden_assembly_syntax::ast::{
    FunctionType, Module, SymbolResolutionError, TypeResolver,
    types::Type as HirType,
};
use miden_debug_types::{DefaultSourceManager, SourceSpan, Spanned};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StackSignature {
    pub inputs: usize,
    pub outputs: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SignatureMismatch {
    pub proc_name: String,
    pub span: SourceSpan,
    pub declared: StackSignature,
    pub inferred: StackSignature,
}

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

    let decompiler = Decompiler::new(&workspace);
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
            ProcSignature::Known { inputs, outputs, .. } => (*inputs, *outputs),
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

fn signature_stack_signature<R>(
    signature: &FunctionType,
    resolver: &R,
) -> Option<StackSignature>
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

fn type_felts(ty: &HirType) -> Option<usize> {
    match ty {
        HirType::Unknown | HirType::Never | HirType::List(_) => None,
        _ => Some(ty.size_in_felts()),
    }
}
