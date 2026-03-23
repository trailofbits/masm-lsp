//! CLI linter for Miden Assembly (MASM).

mod render;

use std::collections::{BTreeSet, HashSet};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use clap::Parser;
use log::{error, warn};
use masm_analysis::{
    AnalysisSnapshot, AdviceDiagnostic, SignatureMismatch, TypeDiagnostic,
    signature_mismatch_message, signature_mismatches_from_snapshot,
};
use masm_decompiler::{
    SymbolPath,
    frontend::{LibraryRoot, Workspace},
};
use miden_debug_types::DefaultSourceManager;

use render::{LintDiagnostic, RelatedSpan};

// ── CLI ───────────────────────────────────────────────────────────────────────

#[derive(Parser, Debug)]
#[command(
    name = "masm-lint",
    version,
    about = "Static analysis linter for Miden Assembly (MASM) files"
)]
struct Cli {
    /// MASM source files or directories to lint
    #[arg(required = true, value_name = "INPUT")]
    inputs: Vec<PathBuf>,

    /// Register an additional library root: <namespace>=<path>
    #[arg(long = "library", value_parser = parse_library_spec)]
    libraries: Vec<LibraryRoot>,

    /// Disable colored output
    #[arg(long)]
    no_color: bool,
}

fn main() {
    lovely_env_logger::init_default();

    let cli = Cli::parse();

    if cli.no_color {
        yansi::disable();
    }

    std::process::exit(run(cli));
}

// ── Driver ────────────────────────────────────────────────────────────────────

/// Run the linter. Returns an exit code: 0 = clean, 1 = warnings, 2 = hard error.
fn run(cli: Cli) -> i32 {
    let cwd = match std::env::current_dir() {
        Ok(d) => d,
        Err(e) => {
            error!("cannot determine working directory: {e}");
            return 2;
        }
    };

    // Validate library root paths before doing anything else.
    for root in &cli.libraries {
        if !root.path.exists() {
            error!(
                "library root path does not exist: {} (for namespace `{}`)",
                root.path.display(),
                root.namespace
            );
            return 2;
        }
    }

    // Collect .masm files from all inputs, canonicalized and deduplicated.
    let mut masm_files: BTreeSet<PathBuf> = BTreeSet::new();
    for input in &cli.inputs {
        let abs = normalize_cli_path(input, &cwd);
        if abs.is_dir() {
            collect_masm_files(&abs, &mut masm_files);
        } else if abs.is_file() {
            masm_files.insert(abs);
        } else {
            error!("input path does not exist: {}", input.display());
            return 2;
        }
    }

    if masm_files.is_empty() {
        warn!("no .masm files found in the given inputs");
        return 0;
    }

    // Build library roots: user-supplied + CWD as the default root.
    let mut roots = normalize_library_roots(&cli.libraries, &cwd);
    roots.push(LibraryRoot::new("", normalize_cli_path(&cwd, &cwd)));

    // Build workspace using a shared source manager so spans resolve correctly
    // across modules.
    let sources: Arc<DefaultSourceManager> = Arc::new(DefaultSourceManager::default());
    let mut workspace = Workspace::with_source_manager(roots, sources.clone());

    for file in &masm_files {
        if let Err(e) = workspace.load_entry(file) {
            warn!("skipping {}: {e}", file.display());
        }
    }

    workspace.load_dependencies();
    let unresolved = workspace.unresolved_module_paths();

    // Run all analysis passes.
    let snapshot = AnalysisSnapshot::from_workspace(&workspace);

    // Collect all lint diagnostics.
    let mut diagnostics: Vec<LintDiagnostic> = Vec::new();

    // Signature mismatches — only when all dependencies are resolved.
    if unresolved.is_empty() {
        for program in workspace.modules() {
            let module = program.module();
            let mismatches =
                signature_mismatches_from_snapshot(module, sources.clone(), &snapshot.signatures);
            for m in mismatches {
                if let Some(diag) = signature_mismatch_to_lint(&m) {
                    diagnostics.push(diag);
                }
            }
        }
    } else {
        emit_unresolved_dependency_warnings(&unresolved, &workspace);
    }

    // TODO: Re-enable once the decompiler's type analysis distinguishes
    // genuinely incorrect types from unresolved (default Felt) types.
    // for type_diags in snapshot.type_diagnostics.values() {
    //     for td in type_diags {
    //         diagnostics.push(type_diagnostic_to_lint(td));
    //     }
    // }

    // Advice diagnostics.
    for advice_diags in snapshot.advice_diagnostics.values() {
        for ad in advice_diags {
            diagnostics.push(advice_diagnostic_to_lint(ad));
        }
    }

    // Sort diagnostics deterministically by file/line/col.
    sort_diagnostics(&mut diagnostics, sources.as_ref());

    let count = diagnostics.len();

    // Render.
    for diag in &diagnostics {
        render::render_diagnostic(diag, sources.as_ref());
    }

    // Summary to stderr.
    if count == 0 {
        eprintln!("masm-lint: no warnings found");
        0
    } else {
        eprintln!("masm-lint: {} warning(s) found", count);
        1
    }
}

// ── Diagnostic conversion ─────────────────────────────────────────────────────

/// Convert a [`SignatureMismatch`] into a [`LintDiagnostic`].
fn signature_mismatch_to_lint(m: &SignatureMismatch) -> Option<LintDiagnostic> {
    let message = signature_mismatch_message(m);
    if message.is_empty() {
        return None;
    }
    let procedure = SymbolPath::new(&m.proc_name);
    Some(LintDiagnostic {
        message,
        span: m.span,
        procedure,
        related: Vec::new(),
    })
}

/// Convert a [`TypeDiagnostic`] into a [`LintDiagnostic`].
#[allow(dead_code)]
fn type_diagnostic_to_lint(td: &TypeDiagnostic) -> LintDiagnostic {
    LintDiagnostic {
        message: td.message.clone(),
        span: td.span,
        procedure: td.procedure.clone(),
        related: Vec::new(),
    }
}

/// Convert an [`AdviceDiagnostic`] into a [`LintDiagnostic`], attaching
/// advice origin spans as related locations.
fn advice_diagnostic_to_lint(ad: &AdviceDiagnostic) -> LintDiagnostic {
    let related = ad
        .origins
        .iter()
        .map(|&origin_span| RelatedSpan {
            span: origin_span,
            message: "unconstrained advice introduced here".to_string(),
        })
        .collect();

    LintDiagnostic {
        message: ad.message.clone(),
        span: ad.span,
        procedure: ad.procedure.clone(),
        related,
    }
}

// ── Sorting ───────────────────────────────────────────────────────────────────

/// Sort diagnostics by (uri, line, col) of their primary span.
fn sort_diagnostics(diagnostics: &mut [LintDiagnostic], sources: &dyn miden_debug_types::SourceManager) {
    diagnostics.sort_by(|a, b| {
        let key_a = sort_key(a.span, sources);
        let key_b = sort_key(b.span, sources);
        key_a.cmp(&key_b)
    });
}

/// Produce a sortable key `(file_uri, line, col)` for a span.
fn sort_key(
    span: miden_debug_types::SourceSpan,
    sources: &dyn miden_debug_types::SourceManager,
) -> (String, usize, usize) {
    sources
        .file_line_col(span)
        .ok()
        .map(|flc| {
            (
                flc.uri.as_str().to_owned(),
                flc.line.to_usize(),
                flc.column.to_usize(),
            )
        })
        .unwrap_or_default()
}

// ── Unresolved dependencies ───────────────────────────────────────────────────

/// Emit warnings about modules that could not be resolved, with guidance on
/// how to configure the missing library roots.
fn emit_unresolved_dependency_warnings(unresolved: &[SymbolPath], workspace: &Workspace) {
    let rendered_modules = unresolved
        .iter()
        .map(|m| m.as_str().to_string())
        .collect::<Vec<_>>()
        .join(", ");
    warn!(
        "unable to load {} referenced module(s): {rendered_modules}",
        unresolved.len()
    );

    let rendered_roots = workspace
        .roots()
        .iter()
        .map(format_library_root)
        .collect::<Vec<_>>()
        .join(", ");
    warn!("configured library roots: {rendered_roots}");
    warn!("signature mismatch checks are skipped when dependencies are unresolved");

    let mut seen_configured: HashSet<String> = HashSet::new();
    let mut seen_unconfigured: HashSet<String> = HashSet::new();
    for module in unresolved {
        if let Some(ns) = configured_namespace_for_module(module, workspace.roots()) {
            if seen_configured.insert(ns.to_string()) {
                warn!(
                    "namespace `{ns}` is configured, but some referenced modules were not found under its roots"
                );
            }
        } else if seen_unconfigured.insert(module.as_str().to_string()) {
            warn!(
                "no library root configured for referenced module `{}`. \
                 Add `--library <namespace>=<path>` using the exact MASM path prefix for that module tree",
                module.as_str()
            );
        }
    }
}

/// Return the longest configured namespace that matches `module`.
fn configured_namespace_for_module<'a>(
    module: &SymbolPath,
    roots: &'a [LibraryRoot],
) -> Option<&'a str> {
    roots
        .iter()
        .filter(|root| !root.namespace.is_empty())
        .filter(|root| root.matches_module_path(module.as_str()))
        .map(|root| root.namespace.as_str())
        .max_by_key(|ns| ns.len())
}

// ── Filesystem helpers ────────────────────────────────────────────────────────

/// Recursively collect `.masm` files under `dir`.
fn collect_masm_files(dir: &Path, out: &mut BTreeSet<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect_masm_files(&path, out);
        } else if path.extension().and_then(|e| e.to_str()) == Some("masm") {
            if let Ok(canonical) = std::fs::canonicalize(&path) {
                out.insert(canonical);
            } else {
                out.insert(path);
            }
        }
    }
}

/// Normalize a CLI path: resolve to absolute and canonicalize if possible.
fn normalize_cli_path(path: &Path, cwd: &Path) -> PathBuf {
    let abs = if path.is_absolute() {
        path.to_path_buf()
    } else {
        cwd.join(path)
    };
    std::fs::canonicalize(&abs).unwrap_or(abs)
}

/// Normalize user-supplied library roots to absolute, canonicalized paths.
fn normalize_library_roots(roots: &[LibraryRoot], cwd: &Path) -> Vec<LibraryRoot> {
    roots
        .iter()
        .map(|root| LibraryRoot::new(&root.namespace, normalize_cli_path(&root.path, cwd)))
        .collect()
}

/// Render a [`LibraryRoot`] for human-readable output.
fn format_library_root(root: &LibraryRoot) -> String {
    if root.namespace.is_empty() {
        format!("<default>={}", root.path.display())
    } else {
        format!("{}={}", root.namespace, root.path.display())
    }
}

// ── Argument parsing ──────────────────────────────────────────────────────────

/// Parse a `<namespace>=<path>` library spec from the command line.
fn parse_library_spec(spec: &str) -> Result<LibraryRoot, String> {
    let (ns, path) = spec
        .split_once('=')
        .ok_or_else(|| "library spec must be <namespace>=<path>".to_string())?;
    if ns.is_empty() {
        return Err("library namespace cannot be empty".to_string());
    }
    if path.is_empty() {
        return Err("library path cannot be empty".to_string());
    }
    Ok(LibraryRoot::new(ns, PathBuf::from(path)))
}

// ── Tests ─────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_library_spec_valid() {
        let root = parse_library_spec("miden::core=../some/path").expect("should parse");
        assert_eq!(root.namespace, "miden::core");
        assert_eq!(root.path, PathBuf::from("../some/path"));
    }

    #[test]
    fn parse_library_spec_missing_equals() {
        assert!(parse_library_spec("miden::core../some/path").is_err());
    }

    #[test]
    fn parse_library_spec_empty_namespace() {
        assert!(parse_library_spec("=../some/path").is_err());
    }

    #[test]
    fn parse_library_spec_empty_path() {
        assert!(parse_library_spec("miden::core=").is_err());
    }

    #[test]
    fn collect_masm_files_recurses_directories() {
        use std::fs;

        let tmp = std::env::temp_dir().join(format!(
            "masm-lint-test-collect-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        let sub = tmp.join("sub");
        fs::create_dir_all(&sub).unwrap();
        fs::write(tmp.join("a.masm"), "").unwrap();
        fs::write(sub.join("b.masm"), "").unwrap();
        fs::write(tmp.join("readme.txt"), "").unwrap();

        let mut files = BTreeSet::new();
        collect_masm_files(&tmp, &mut files);

        let names: Vec<_> = files
            .iter()
            .map(|p| p.file_name().unwrap().to_str().unwrap().to_owned())
            .collect();
        assert!(names.contains(&"a.masm".to_string()));
        assert!(names.contains(&"b.masm".to_string()));
        assert!(!names.contains(&"readme.txt".to_string()));

        let _ = fs::remove_dir_all(tmp);
    }
}
