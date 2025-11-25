use clap::Parser;
use masm_lsp::{server::Backend, LibraryPath, ServerConfig};
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
    /// Path to miden-vm repository containing stdlib (defaults to managed copy under ~/.local/state/masm-lsp/miden-vm)
    #[arg(long)]
    stdlib_path: Option<std::path::PathBuf>,
}

fn build_config(args: &Args) -> ServerConfig {
    let mut config = ServerConfig::default();
    let repo_root = match &args.stdlib_path {
        Some(path) => path.clone(),
        None => ensure_default_repo(),
    };
    let lib_root = repo_root.join("stdlib").join("asm");
    config.library_paths = vec![LibraryPath {
        root: lib_root,
        prefix: "std".to_string(),
    }];
    config
}

fn ensure_default_repo() -> std::path::PathBuf {
    use std::process::Command;
    let base = dirs::state_dir()
        .unwrap_or_else(|| std::path::PathBuf::from("."))
        .join("masm-lsp")
        .join("miden-vm");
    if !base.exists() {
        let _ = std::fs::create_dir_all(base.parent().unwrap_or_else(|| std::path::Path::new(".")));
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
