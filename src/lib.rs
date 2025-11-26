pub mod client;
pub mod diagnostics;
pub mod index;
pub mod inlay_hints;
pub mod instruction_hints;
pub mod module_path;
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
    /// Whether to show hover information for built-in instructions.
    pub instruction_hovers_enabled: bool,
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
            // Instruction hovers are disabled by default.
            instruction_hovers_enabled: false,
        }
    }
}

impl ServerConfig {
    /// Create a new builder for `ServerConfig`.
    pub fn builder() -> ServerConfigBuilder {
        ServerConfigBuilder::default()
    }
}

/// Builder for `ServerConfig` with fluent API.
#[derive(Default)]
pub struct ServerConfigBuilder {
    inlay_hint_tabs: Option<usize>,
    library_paths: Option<Vec<LibraryPath>>,
    instruction_hovers_enabled: Option<bool>,
}

impl ServerConfigBuilder {
    /// Set the number of tab characters to insert before an inlay hint label.
    pub fn inlay_hint_tabs(mut self, tabs: usize) -> Self {
        self.inlay_hint_tabs = Some(tabs);
        self
    }

    /// Set the library roots to preload and use for module path derivation.
    pub fn library_paths(mut self, paths: Vec<LibraryPath>) -> Self {
        self.library_paths = Some(paths);
        self
    }

    /// Set whether to show hover information for built-in instructions.
    pub fn instruction_hovers_enabled(mut self, enabled: bool) -> Self {
        self.instruction_hovers_enabled = Some(enabled);
        self
    }

    /// Build the `ServerConfig` with the configured values.
    ///
    /// Uses defaults for any values not explicitly set.
    pub fn build(self) -> ServerConfig {
        ServerConfig {
            inlay_hint_tabs: self.inlay_hint_tabs.unwrap_or(2),
            library_paths: self.library_paths.unwrap_or_else(default_library_paths),
            instruction_hovers_enabled: self.instruction_hovers_enabled.unwrap_or(false),
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
