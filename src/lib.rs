pub mod analysis;
pub mod client;
pub mod decompiler;
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

/// The type of inlay hints to display.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum InlayHintType {
    /// Show instruction descriptions.
    Description,
    /// Show decompiled pseudocode (default behavior).
    #[default]
    Decompilation,
    /// Disable inlay hints.
    None,
}

/// Configuration shared across handlers.
#[derive(Debug, Clone)]
pub struct ServerConfig {
    /// Number of tab characters to insert before an inlay hint label.
    pub inlay_hint_tabs: usize,
    /// Library roots to preload and use for module path derivation.
    pub library_paths: Vec<LibraryPath>,
    /// Whether to show hover information for built-in instructions.
    pub instruction_hovers_enabled: bool,
    /// Whether to run taint analysis and report security warnings.
    pub taint_analysis_enabled: bool,
    /// The inlay hint type to display.
    pub inlay_hint_type: InlayHintType,
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
            // Taint analysis is enabled by default.
            taint_analysis_enabled: true,
            // Decompilation hints are shown by default.
            inlay_hint_type: InlayHintType::Decompilation,
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
    taint_analysis_enabled: Option<bool>,
    inlay_hint_type: Option<InlayHintType>,
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

    /// Set whether to run taint analysis and report security warnings.
    pub fn taint_analysis_enabled(mut self, enabled: bool) -> Self {
        self.taint_analysis_enabled = Some(enabled);
        self
    }

    /// Set the inlay hint type.
    pub fn inlay_hint_type(mut self, mode: InlayHintType) -> Self {
        self.inlay_hint_type = Some(mode);
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
            taint_analysis_enabled: self.taint_analysis_enabled.unwrap_or(true),
            inlay_hint_type: self.inlay_hint_type.unwrap_or(InlayHintType::Decompilation),
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
