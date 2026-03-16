use clap::Parser;
use masm_lsp::{
    core_lib::{
        core_library_root_from_repo_root, default_core_library_path, discover_core_library_from,
        normalize_core_library_path,
    },
    server::Backend,
    ServerConfig,
};
use std::path::{Path, PathBuf};
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
    /// Path to the miden-vm repo root, or core library root
    #[arg(long)]
    core_path: Option<PathBuf>,
}

fn build_config(args: &Args) -> ServerConfig {
    let mut config = ServerConfig::default();
    let lib_root = resolve_core_library_root(args.core_path.as_deref());
    tracing::info!("using core library root: {}", lib_root.display());
    config.library_paths = vec![default_core_library_path(lib_root)];
    config
}

fn resolve_core_library_root(core_path: Option<&Path>) -> PathBuf {
    let cwd = std::env::current_dir().ok();
    resolve_core_library_root_from(core_path, cwd.as_deref())
}

fn resolve_core_library_root_from(core_path: Option<&Path>, cwd: Option<&Path>) -> PathBuf {
    if let Some(path) = core_path {
        let root = normalize_core_library_path(path);
        tracing::info!("using core library from --core-path: {}", root.display());
        return root;
    }

    if let Some(cwd) = cwd
        && let Some(root) = discover_core_library_from(cwd)
    {
        tracing::info!(
            "resolved core library by walking from {}: {}",
            cwd.display(),
            root.display()
        );
        return root;
    }

    let root = core_library_root_from_repo_root(&ensure_default_repo());
    tracing::info!("using managed core library copy: {}", root.display());
    root
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
    fn resolve_core_library_root_accepts_repo_root_core_and_asm() {
        let base = unique_temp_dir("normalize");
        let repo = base.join("miden-vm");
        let core = repo.join("crates").join("lib").join("core");
        let asm = core.join("asm");
        std::fs::create_dir_all(&asm).expect("create core library asm");
        let expected = asm
            .canonicalize()
            .expect("canonicalize expected core library path");

        assert_eq!(normalize_core_library_path(&repo), expected);
        assert_eq!(normalize_core_library_path(&core), expected);
        assert_eq!(normalize_core_library_path(&asm), expected);

        let _ = std::fs::remove_dir_all(base);
    }

    #[test]
    fn discover_core_library_from_finds_ancestor_repo_root() {
        let base = unique_temp_dir("discover-ancestor");
        let repo = base.join("miden-vm");
        let asm = repo.join("crates").join("lib").join("core").join("asm");
        let nested = repo.join("crates").join("masm-lsp");
        std::fs::create_dir_all(&asm).expect("create core library asm");
        std::fs::create_dir_all(&nested).expect("create nested project");
        let expected = asm
            .canonicalize()
            .expect("canonicalize expected core library path");

        assert_eq!(discover_core_library_from(&nested), Some(expected));

        let _ = std::fs::remove_dir_all(base);
    }

    #[test]
    fn discover_core_library_from_finds_sibling_miden_vm() {
        let base = unique_temp_dir("discover-sibling");
        let workspace = base.join("workspace");
        let project = workspace.join("masm-lsp");
        let asm = workspace
            .join("miden-vm")
            .join("crates")
            .join("lib")
            .join("core")
            .join("asm");
        std::fs::create_dir_all(&project).expect("create project dir");
        std::fs::create_dir_all(&asm).expect("create sibling core library asm");
        let expected = asm
            .canonicalize()
            .expect("canonicalize expected core library path");

        assert_eq!(discover_core_library_from(&project), Some(expected));

        let _ = std::fs::remove_dir_all(base);
    }

    #[test]
    fn resolve_core_library_root_prefers_cli_argument_over_discovery() {
        let base = unique_temp_dir("resolve-explicit");
        let workspace = base.join("workspace");
        let project = workspace.join("masm-lsp");
        let discovered = workspace
            .join("miden-vm")
            .join("crates")
            .join("lib")
            .join("core")
            .join("asm");
        let explicit_repo = base.join("explicit").join("miden-vm");
        let explicit_asm = explicit_repo
            .join("crates")
            .join("lib")
            .join("core")
            .join("asm");

        std::fs::create_dir_all(&project).expect("create project dir");
        std::fs::create_dir_all(&discovered).expect("create discovered core library");
        std::fs::create_dir_all(&explicit_asm).expect("create explicit core library");

        let expected = explicit_asm
            .canonicalize()
            .expect("canonicalize explicit core library path");
        let resolved =
            resolve_core_library_root_from(Some(&explicit_repo), Some(project.as_path()));

        assert_eq!(resolved, expected);

        let _ = std::fs::remove_dir_all(base);
    }
}
