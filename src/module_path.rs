//! Module path resolution from file URIs.
//!
//! This module provides utilities for resolving file URIs to MASM module paths,
//! which are used during parsing to establish the fully-qualified names of symbols.

use std::path::Path;

use tower_lsp::lsp_types::Url;

use crate::LibraryPath;

/// Resolves module paths from file URIs using library configuration.
///
/// The resolver attempts to derive a module path by checking if the file
/// falls within any configured library root. If so, it constructs the path
/// relative to that root with the appropriate prefix.
pub struct ModulePathResolver<'a> {
    library_paths: &'a [LibraryPath],
}

impl<'a> ModulePathResolver<'a> {
    /// Create a new resolver with the given library paths.
    pub fn new(library_paths: &'a [LibraryPath]) -> Self {
        Self { library_paths }
    }

    /// Resolve a file URI to a module path.
    ///
    /// Attempts to match the file against configured library roots first,
    /// then falls back to using the file stem as a simple module name.
    pub fn resolve(&self, uri: &Url) -> Option<miden_assembly_syntax::ast::PathBuf> {
        if let Ok(fs_path) = uri.to_file_path() {
            for lib in self.library_paths {
                if fs_path.starts_with(&lib.root) {
                    if let Some(buf) = build_path_from_root(&fs_path, &lib.root, &lib.prefix) {
                        return Some(buf);
                    }
                }
            }
        }
        fallback_from_uri(uri)
    }
}

/// Build a module path from a file path relative to a library root.
///
/// Given a file at `/path/to/stdlib/crypto/hashes/sha256.masm` with root
/// `/path/to/stdlib` and prefix `std`, this produces `std::crypto::hashes::sha256`.
fn build_path_from_root(
    path: &Path,
    root: &Path,
    prefix: &str,
) -> Option<miden_assembly_syntax::ast::PathBuf> {
    let rel = path.strip_prefix(root).ok()?;
    let mut buf = miden_assembly_syntax::ast::PathBuf::default();
    if !prefix.is_empty() {
        buf.push(prefix);
    }
    let parts: Vec<_> = rel.iter().collect();
    for (i, comp) in parts.iter().enumerate() {
        let mut seg = comp.to_string_lossy();
        // Strip file extension from the last component
        if i == parts.len().saturating_sub(1) {
            if let Some(stripped) = seg.split('.').next() {
                seg = std::borrow::Cow::Owned(stripped.to_string());
            }
        }
        if seg.is_empty() {
            continue;
        }
        buf.push(seg.as_ref());
    }
    Some(buf)
}

/// Fallback module path resolution using just the file stem.
///
/// When a file doesn't fall within any library root, we use its filename
/// (without extension) as a simple module name.
fn fallback_from_uri(uri: &Url) -> Option<miden_assembly_syntax::ast::PathBuf> {
    let path = uri.path();
    let stem = Path::new(path)
        .file_stem()
        .and_then(|s| s.to_str())
        .filter(|s| !s.is_empty())?;
    let mut buf = miden_assembly_syntax::ast::PathBuf::default();
    buf.push(stem);
    Some(buf)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn build_path_from_root_basic() {
        let root = PathBuf::from("/libs/stdlib");
        let path = PathBuf::from("/libs/stdlib/crypto/sha256.masm");
        let result = build_path_from_root(&path, &root, "std");
        assert!(result.is_some());
        assert_eq!(result.unwrap().to_string(), "std::crypto::sha256");
    }

    #[test]
    fn build_path_from_root_no_prefix() {
        let root = PathBuf::from("/libs/stdlib");
        let path = PathBuf::from("/libs/stdlib/crypto/sha256.masm");
        let result = build_path_from_root(&path, &root, "");
        assert!(result.is_some());
        assert_eq!(result.unwrap().to_string(), "crypto::sha256");
    }

    #[test]
    fn build_path_from_root_nested() {
        let root = PathBuf::from("/libs");
        let path = PathBuf::from("/libs/crypto/hashes/sha256.masm");
        let result = build_path_from_root(&path, &root, "std");
        assert!(result.is_some());
        assert_eq!(result.unwrap().to_string(), "std::crypto::hashes::sha256");
    }

    #[test]
    fn build_path_from_root_not_under_root() {
        let root = PathBuf::from("/libs/stdlib");
        let path = PathBuf::from("/other/path/file.masm");
        let result = build_path_from_root(&path, &root, "std");
        assert!(result.is_none());
    }

    #[test]
    fn fallback_from_uri_simple() {
        let uri = Url::parse("file:///tmp/mymodule.masm").unwrap();
        let result = fallback_from_uri(&uri);
        assert!(result.is_some());
        assert_eq!(result.unwrap().to_string(), "mymodule");
    }

    #[test]
    fn fallback_from_uri_nested_path() {
        let uri = Url::parse("file:///home/user/projects/test.masm").unwrap();
        let result = fallback_from_uri(&uri);
        assert!(result.is_some());
        assert_eq!(result.unwrap().to_string(), "test");
    }

    #[test]
    fn resolver_uses_library_path() {
        let lib_root = PathBuf::from("/libs/stdlib");
        let library_paths = vec![LibraryPath {
            root: lib_root,
            prefix: "std".to_string(),
        }];
        let resolver = ModulePathResolver::new(&library_paths);

        let uri = Url::from_file_path("/libs/stdlib/crypto/hash.masm").unwrap();
        let result = resolver.resolve(&uri);
        assert!(result.is_some());
        assert_eq!(result.unwrap().to_string(), "std::crypto::hash");
    }

    #[test]
    fn resolver_falls_back_when_not_in_library() {
        let lib_root = PathBuf::from("/libs/stdlib");
        let library_paths = vec![LibraryPath {
            root: lib_root,
            prefix: "std".to_string(),
        }];
        let resolver = ModulePathResolver::new(&library_paths);

        let uri = Url::from_file_path("/other/path/myfile.masm").unwrap();
        let result = resolver.resolve(&uri);
        assert!(result.is_some());
        assert_eq!(result.unwrap().to_string(), "myfile");
    }

    #[test]
    fn resolver_with_empty_library_paths() {
        let library_paths: Vec<LibraryPath> = vec![];
        let resolver = ModulePathResolver::new(&library_paths);

        let uri = Url::from_file_path("/some/path/module.masm").unwrap();
        let result = resolver.resolve(&uri);
        assert!(result.is_some());
        assert_eq!(result.unwrap().to_string(), "module");
    }
}
