pub mod client;
pub mod diagnostics;
pub mod index;
pub mod inlay_hints;
pub mod resolution;
pub mod server;
pub mod service;
pub mod symbol_path;
pub mod util;

pub use resolution::ResolutionError;
pub use symbol_path::SymbolPath;

/// Configuration shared across handlers.
#[derive(Debug, Clone)]
pub struct ServerConfig {
    /// Number of tab characters to insert before an inlay hint label.
    pub inlay_hint_tabs: usize,
    /// Library roots to preload and use for module path derivation.
    pub library_paths: Vec<LibraryPath>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LibraryPath {
    pub root: std::path::PathBuf,
    pub prefix: String,
}

impl Default for ServerConfig {
    fn default() -> Self {
        Self {
            // Use two tabs by default to give hints breathing room.
            inlay_hint_tabs: 2,
            library_paths: default_library_paths(),
        }
    }
}

fn default_library_paths() -> Vec<LibraryPath> {
    let mut paths = Vec::new();
    let root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join(".codex")
        .join("miden-vm")
        .join("stdlib")
        .join("asm");
    if root.is_dir() {
        paths.push(LibraryPath {
            root,
            prefix: "std".to_string(),
        });
    }
    paths
}
