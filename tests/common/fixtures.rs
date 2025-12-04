//! Test fixture loading and management.

#![allow(dead_code)]
use masm_lsp::decompiler::collect_decompilation_hints;
use miden_assembly_syntax::ast::{Module, ModuleKind};
use miden_assembly_syntax::{Parse, ParseOptions};
use miden_debug_types::{DefaultSourceManager, SourceLanguage, SourceManager};
use std::path::PathBuf;
use tower_lsp::lsp_types::Url;
use tower_lsp::lsp_types::{InlayHintLabel, Position, Range};

/// Load a fixture file from tests/fixtures/
pub fn load_fixture(name: &str) -> String {
    let path = fixtures_dir().join(name);
    std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("failed to load fixture '{}': {}", path.display(), e))
}

/// Load an example file from examples/
pub fn load_example(path: &str) -> String {
    let full_path = examples_dir().join(path);
    std::fs::read_to_string(&full_path)
        .unwrap_or_else(|e| panic!("failed to load example '{}': {}", full_path.display(), e))
}

/// Load and parse an example file as a module.
pub fn load_example_module(path: &str) -> Option<(String, DefaultSourceManager, Module)> {
    let content = load_example(path);
    let source_manager = DefaultSourceManager::default();
    let uri_str = format!("file:///test/{}", path);
    let uri = miden_debug_types::Uri::from(uri_str.as_str());
    source_manager.load(SourceLanguage::Masm, uri.clone(), content.clone());

    let source_file = source_manager.get_by_uri(&uri)?;

    let opts = ParseOptions {
        kind: miden_assembly_syntax::ast::ModuleKind::Library,
        path: None,
        ..Default::default()
    };

    let module = source_file.parse_with_options(&source_manager, opts).ok()?;
    Some((content, source_manager, *module))
}

pub fn collect_labels_and_diags(source: &str) -> (Vec<String>, Vec<String>) {
    // Prepare source manager and parse module
    let source_manager = DefaultSourceManager::default();
    let uri = miden_debug_types::Uri::from("file:///test.masm");
    source_manager.load(SourceLanguage::Masm, uri.clone(), source.to_string());

    let source_file = source_manager
        .get_by_uri(&uri)
        .expect("failed to load source");

    let opts = ParseOptions {
        kind: ModuleKind::Library,
        path: None,
        ..Default::default()
    };

    let module = source_file
        .parse_with_options(&source_manager, opts)
        .expect("failed to parse MASM");

    let line_count = source.lines().count() as u32;
    let last_line_len = source.lines().last().map(|l| l.len()).unwrap_or(0) as u32;
    let visible_range = Range {
        start: Position {
            line: 0,
            character: 0,
        },
        end: Position {
            line: line_count,
            character: last_line_len,
        },
    };

    let url = Url::parse("file:///test.masm").unwrap();

    let result = collect_decompilation_hints(
        &module,
        &source_manager,
        &url,
        &visible_range,
        4,
        source,
        None,
    );

    let labels = result
        .hints
        .iter()
        .filter_map(|h| match &h.label {
            InlayHintLabel::String(s) => Some(s.clone()),
            _ => None,
        })
        .collect::<Vec<_>>();

    let diags = result
        .diagnostics
        .into_iter()
        .map(|d| d.message)
        .collect::<Vec<_>>();

    (labels, diags)
}

/// Get the path to the fixtures directory.
fn fixtures_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures")
}

/// Get the path to the examples directory.
fn examples_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples")
}

/// Create a file:// URL for a test file.
fn test_url(name: &str) -> Url {
    Url::parse(&format!("file:///tmp/test/{}", name)).expect("valid URL")
}

/// Types of test fixtures available.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FixtureKind {
    /// Module with aliases and imports (module_with_aliases.masm)
    WithAliases,
    /// Multiple procedures calling each other
    MultiProc,
    /// File with nested control flow blocks
    NestedBlocks,
}

impl FixtureKind {
    /// Get the fixture content for this kind.
    pub fn content(&self) -> String {
        match self {
            FixtureKind::WithAliases => load_fixture("module_with_aliases.masm"),
            FixtureKind::MultiProc => load_fixture("multi_proc.masm"),
            FixtureKind::NestedBlocks => load_fixture("nested_blocks.masm"),
        }
    }

    /// Get a suggested filename for this fixture.
    pub fn filename(&self) -> &'static str {
        match self {
            FixtureKind::WithAliases => "aliases.masm",
            FixtureKind::MultiProc => "multi_proc.masm",
            FixtureKind::NestedBlocks => "nested_blocks.masm",
        }
    }

    /// Get a test URL for this fixture.
    pub fn url(&self) -> Url {
        test_url(self.filename())
    }
}

/// Inline fixture content for tests that don't need file-based fixtures.
pub mod inline {
    /// A simple procedure definition.
    pub const SIMPLE_PROC: &str = r#"proc foo
    push.1
end
"#;

    /// An executable with a begin block.
    pub const SIMPLE_EXECUTABLE: &str = r#"proc foo
    push.1
end

begin
    exec.foo
end
"#;

    /// Multiple procedures with various call patterns.
    pub const MULTI_CALL: &str = r#"proc f
    nop
end

proc g
    exec.f
    exec.f
    exec.f
end
"#;

    /// Cross-module reference pattern.
    pub const CROSS_MODULE_DEF: &str = r#"proc foo
    push.1
end
"#;

    /// Cross-module call pattern.
    pub const CROSS_MODULE_CALL: &str = r#"proc bar
    exec.::defs::foo
end
"#;

    /// Syntax error - missing end.
    pub const SYNTAX_ERROR_MISSING_END: &str = r#"proc broken
    push.1
"#;

    /// Unresolved reference.
    pub const UNRESOLVED_REF: &str = r#"proc foo
    exec.nonexistent
end
"#;

    /// Empty file.
    pub const EMPTY: &str = "";

    /// Procedure with procref.
    pub const WITH_PROCREF: &str = r#"proc target
    nop
end

proc caller
    procref.target
end
"#;
}

/// Helper to find a position in text where a needle occurs.
pub fn find_position(text: &str, needle: &str) -> tower_lsp::lsp_types::Position {
    let offset = text
        .rfind(needle)
        .unwrap_or_else(|| panic!("needle '{}' not found in text", needle));
    let mut line = 0u32;
    let mut col = 0u32;
    for (i, ch) in text.char_indices() {
        if i == offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }
    tower_lsp::lsp_types::Position::new(line, col)
}
