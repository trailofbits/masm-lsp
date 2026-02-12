use clap::Parser;
use masm_lsp::{server::Backend, LibraryPath, ServerConfig};
use std::{
    ffi::OsStr,
    path::{Path, PathBuf},
};
use tower_lsp::{LspService, Server};

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt()
        .with_env_filter("info")
        .with_target(false)
        .with_ansi(false)
        .with_writer(std::io::stderr)
        .init();

    let args = Args::parse();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let config = build_config(&args);

    let (service, socket) =
        LspService::new(|client| Backend::new_with_config(client, config.clone()));
    Server::new(stdin, stdout, socket).serve(service).await;
}

#[derive(Parser, Debug)]
#[command(name = "masm-lsp")]
struct Args {
    /// Path to miden-vm repo root, stdlib root, or stdlib/asm (defaults to auto-discovery, then managed copy)
    #[arg(long)]
    stdlib_path: Option<PathBuf>,
}

fn build_config(args: &Args) -> ServerConfig {
    let mut config = ServerConfig::default();
    let lib_root = resolve_stdlib_library_root(args.stdlib_path.as_deref());
    config.library_paths = vec![LibraryPath {
        root: lib_root,
        prefix: "std".to_string(),
    }];
    config
}

fn resolve_stdlib_library_root(stdlib_path: Option<&Path>) -> PathBuf {
    let cwd = std::env::current_dir().ok();
    resolve_stdlib_library_root_from(stdlib_path, cwd.as_deref())
}

fn resolve_stdlib_library_root_from(stdlib_path: Option<&Path>, cwd: Option<&Path>) -> PathBuf {
    if let Some(path) = stdlib_path {
        let root = normalize_stdlib_path(path);
        tracing::info!("using stdlib from --stdlib-path: {}", root.display());
        return root;
    }

    if let Some(cwd) = cwd {
        if let Some(root) = discover_stdlib_from(&cwd) {
            tracing::info!(
                "resolved stdlib by walking from {}: {}",
                cwd.display(),
                root.display()
            );
            return root;
        }
    }

    let root = ensure_default_repo().join("stdlib").join("asm");
    tracing::info!("using managed stdlib copy: {}", root.display());
    root
}

fn normalize_stdlib_path(path: &Path) -> PathBuf {
    let normalized = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
    if is_stdlib_asm_path(&normalized) {
        return normalized;
    }
    if is_stdlib_path(&normalized) {
        return normalized.join("asm");
    }
    normalized.join("stdlib").join("asm")
}

fn discover_stdlib_from(start: &Path) -> Option<PathBuf> {
    let normalized = start.canonicalize().unwrap_or_else(|_| start.to_path_buf());
    let mut probe = normalized;
    if probe.is_file() {
        probe = probe.parent()?.to_path_buf();
    }

    for ancestor in probe.ancestors() {
        let direct = ancestor.join("stdlib").join("asm");
        if direct.is_dir() {
            return Some(direct);
        }

        let sibling = ancestor.join("miden-vm").join("stdlib").join("asm");
        if sibling.is_dir() {
            return Some(sibling);
        }
    }

    None
}

fn is_stdlib_path(path: &Path) -> bool {
    path.file_name() == Some(OsStr::new("stdlib"))
}

fn is_stdlib_asm_path(path: &Path) -> bool {
    path.file_name() == Some(OsStr::new("asm"))
        && path.parent().and_then(|p| p.file_name()) == Some(OsStr::new("stdlib"))
}

fn ensure_default_repo() -> PathBuf {
    use std::process::Command;
    let base = dirs::state_dir()
        .unwrap_or_else(|| PathBuf::from("."))
        .join("masm-lsp")
        .join("miden-vm");
    if !base.exists() {
        let _ = std::fs::create_dir_all(base.parent().unwrap_or_else(|| Path::new(".")));
        let _ = Command::new("git")
            .args([
                "clone",
                "--depth",
                "1",
                "https://github.com/0xMiden/miden-vm",
                base.to_string_lossy().as_ref(),
            ])
            .output();
    } else {
        let _ = Command::new("git")
            .args(["-C", base.to_string_lossy().as_ref(), "fetch", "--prune"])
            .output();
        let _ = Command::new("git")
            .args(["-C", base.to_string_lossy().as_ref(), "pull", "--ff-only"])
            .output();
    }
    base
}

#[cfg(test)]
mod tests {
    use super::*;

    fn unique_temp_dir(name: &str) -> PathBuf {
        let stamp = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("time should move forward")
            .as_nanos();
        std::env::temp_dir().join(format!("masm-lsp-{name}-{}-{stamp}", std::process::id()))
    }

    #[test]
    fn normalize_stdlib_path_accepts_repo_root_stdlib_and_asm() {
        let base = unique_temp_dir("normalize");
        let repo = base.join("miden-vm");
        let stdlib = repo.join("stdlib");
        let asm = stdlib.join("asm");
        std::fs::create_dir_all(&asm).expect("create stdlib/asm");
        let expected = asm
            .canonicalize()
            .expect("canonicalize expected stdlib/asm path");

        assert_eq!(normalize_stdlib_path(&repo), expected);
        assert_eq!(normalize_stdlib_path(&stdlib), expected);
        assert_eq!(normalize_stdlib_path(&asm), expected);

        let _ = std::fs::remove_dir_all(base);
    }

    #[test]
    fn discover_stdlib_from_finds_ancestor_repo_root() {
        let base = unique_temp_dir("discover-ancestor");
        let repo = base.join("miden-vm");
        let asm = repo.join("stdlib").join("asm");
        let nested = repo.join("crates").join("masm-lsp");
        std::fs::create_dir_all(&asm).expect("create stdlib/asm");
        std::fs::create_dir_all(&nested).expect("create nested project");
        let expected = asm
            .canonicalize()
            .expect("canonicalize expected stdlib/asm path");

        assert_eq!(discover_stdlib_from(&nested), Some(expected));

        let _ = std::fs::remove_dir_all(base);
    }

    #[test]
    fn discover_stdlib_from_finds_sibling_miden_vm() {
        let base = unique_temp_dir("discover-sibling");
        let workspace = base.join("workspace");
        let project = workspace.join("masm-lsp");
        let asm = workspace.join("miden-vm").join("stdlib").join("asm");
        std::fs::create_dir_all(&project).expect("create project dir");
        std::fs::create_dir_all(&asm).expect("create sibling stdlib/asm");
        let expected = asm
            .canonicalize()
            .expect("canonicalize expected stdlib/asm path");

        assert_eq!(discover_stdlib_from(&project), Some(expected));

        let _ = std::fs::remove_dir_all(base);
    }

    #[test]
    fn resolve_stdlib_library_root_prefers_cli_argument_over_discovery() {
        let base = unique_temp_dir("resolve-explicit");
        let workspace = base.join("workspace");
        let project = workspace.join("masm-lsp");
        let discovered = workspace.join("miden-vm").join("stdlib").join("asm");
        let explicit_repo = base.join("explicit").join("miden-vm");
        let explicit_asm = explicit_repo.join("stdlib").join("asm");

        std::fs::create_dir_all(&project).expect("create project dir");
        std::fs::create_dir_all(&discovered).expect("create discovered stdlib/asm");
        std::fs::create_dir_all(&explicit_asm).expect("create explicit stdlib/asm");

        let expected = explicit_asm
            .canonicalize()
            .expect("canonicalize explicit stdlib/asm path");
        let resolved =
            resolve_stdlib_library_root_from(Some(&explicit_repo), Some(project.as_path()));

        assert_eq!(resolved, expected);

        let _ = std::fs::remove_dir_all(base);
    }
}
