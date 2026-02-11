pub use masm_decompiler as decompiler;
pub use masm_decompiler::symbol::resolution as symbol_resolution;
pub mod client;
pub mod code_lens;
pub mod cursor_resolution;
pub mod diagnostics;
pub mod index;
pub mod inlay_hints;
pub mod module_path;
pub mod server;
pub mod service;
pub mod util;

pub use cursor_resolution::ResolutionError;
pub use masm_decompiler::SymbolPath;

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
    /// Library roots to preload and use for module path derivation.
    pub library_paths: Vec<LibraryPath>,
    /// Whether to show stack-effect code lenses.
    pub code_lens_stack_effects: bool,
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
            library_paths: default_library_paths(),
            code_lens_stack_effects: true,
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
    library_paths: Option<Vec<LibraryPath>>,
    code_lens_stack_effects: Option<bool>,
    taint_analysis_enabled: Option<bool>,
    inlay_hint_type: Option<InlayHintType>,
}

impl ServerConfigBuilder {
    /// Set the library roots to preload and use for module path derivation.
    pub fn library_paths(mut self, paths: Vec<LibraryPath>) -> Self {
        self.library_paths = Some(paths);
        self
    }

    /// Set whether to show stack-effect code lenses.
    pub fn code_lens_stack_effects(mut self, enabled: bool) -> Self {
        self.code_lens_stack_effects = Some(enabled);
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
            library_paths: self.library_paths.unwrap_or_else(default_library_paths),
            code_lens_stack_effects: self.code_lens_stack_effects.unwrap_or(true),
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
