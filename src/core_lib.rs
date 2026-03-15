use std::{
    ffi::OsStr,
    path::{Path, PathBuf},
};

use crate::{LibraryPath, SymbolPath};

pub const DEFAULT_CORE_LIBRARY_PREFIX: &str = "miden::core";

const CORE_LIBRARY_PATH_FROM_REPO_ROOT: &[&str] = &["crates", "lib", "core", "asm"];
const CORE_LIBRARY_DIR_NAME: &str = "core";
const CORE_LIBRARY_ASM_DIR_NAME: &str = "asm";
const CORE_LIBRARY_PARENT_DIR_NAME: &str = "lib";

pub fn default_core_library_path(root: impl Into<PathBuf>) -> LibraryPath {
    LibraryPath {
        root: root.into(),
        prefix: DEFAULT_CORE_LIBRARY_PREFIX.to_string(),
    }
}

pub fn default_core_library_symbol_path(path: impl AsRef<str>) -> SymbolPath {
    SymbolPath::new(format!(
        "{}::{}",
        DEFAULT_CORE_LIBRARY_PREFIX,
        path.as_ref()
    ))
}

pub fn managed_core_library_root_from_manifest_dir(manifest_dir: &Path) -> PathBuf {
    core_library_root_from_repo_root(&manifest_dir.join(".codex").join("miden-vm"))
}

pub fn core_library_root_from_repo_root(repo_root: &Path) -> PathBuf {
    CORE_LIBRARY_PATH_FROM_REPO_ROOT
        .iter()
        .fold(repo_root.to_path_buf(), |path, component| {
            path.join(component)
        })
}

pub fn normalize_core_library_path(path: &Path) -> PathBuf {
    let normalized = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
    if is_core_library_asm_path(&normalized) {
        return normalized;
    }
    if is_core_library_path(&normalized) {
        return normalized.join(CORE_LIBRARY_ASM_DIR_NAME);
    }
    core_library_root_from_repo_root(&normalized)
}

pub fn discover_core_library_from(start: &Path) -> Option<PathBuf> {
    let normalized = start.canonicalize().unwrap_or_else(|_| start.to_path_buf());
    let mut probe = normalized;
    if probe.is_file() {
        probe = probe.parent()?.to_path_buf();
    }

    for ancestor in probe.ancestors() {
        let direct = core_library_root_from_repo_root(ancestor);
        if direct.is_dir() {
            return Some(direct);
        }

        let sibling = core_library_root_from_repo_root(&ancestor.join("miden-vm"));
        if sibling.is_dir() {
            return Some(sibling);
        }
    }

    None
}

fn is_core_library_path(path: &Path) -> bool {
    path.file_name() == Some(OsStr::new(CORE_LIBRARY_DIR_NAME))
        && path.parent().and_then(|parent| parent.file_name())
            == Some(OsStr::new(CORE_LIBRARY_PARENT_DIR_NAME))
}

fn is_core_library_asm_path(path: &Path) -> bool {
    path.file_name() == Some(OsStr::new(CORE_LIBRARY_ASM_DIR_NAME))
        && path.parent().and_then(|parent| parent.file_name())
            == Some(OsStr::new(CORE_LIBRARY_DIR_NAME))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn unique_temp_dir(name: &str) -> PathBuf {
        let stamp = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("time should move forward")
            .as_nanos();
        std::env::temp_dir().join(format!(
            "masm-lsp-core-lib-{name}-{}-{stamp}",
            std::process::id()
        ))
    }

    #[test]
    fn normalize_core_library_path_accepts_repo_root_core_and_asm() {
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
}
