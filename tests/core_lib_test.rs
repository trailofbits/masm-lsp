//! Integration tests for the Miden core library.
//!
//! These tests verify that the LSP can handle core library files without hanging
//! or consuming excessive memory. The broad coverage comes from discovering the
//! current `examples/core` tree directly rather than hardcoding a file inventory.

mod common;

use common::fixtures::load_example;
use common::harness::TestHarness;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::Duration;
use tokio::time::timeout;
use tower_lsp::lsp_types::{Position, Range};

/// Default timeout for core library file processing.
const CORE_LIB_TIMEOUT: Duration = Duration::from_secs(5);

fn core_examples_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("examples")
        .join("core")
}

fn collect_core_library_files(root: &Path, files: &mut Vec<String>) {
    let entries = fs::read_dir(root).expect("read core library directory");
    for entry in entries {
        let entry = entry.expect("read core library entry");
        let path = entry.path();
        if path.is_dir() {
            collect_core_library_files(&path, files);
            continue;
        }

        let is_masm = path
            .extension()
            .and_then(|ext| ext.to_str())
            .is_some_and(|ext| ext.eq_ignore_ascii_case("masm"));
        if !is_masm {
            continue;
        }

        let relative = path
            .strip_prefix(core_examples_dir())
            .expect("core library file should be under examples/core")
            .to_string_lossy()
            .replace('\\', "/");
        files.push(relative);
    }
}

fn core_library_files() -> Vec<String> {
    let mut files = Vec::new();
    collect_core_library_files(&core_examples_dir(), &mut files);
    files.sort();
    files
}

async fn run_core_library_file(relative_path: &str, decompile: bool) {
    let harness = TestHarness::new().await;
    if decompile {
        harness.enable_decompilation_hints().await;
    }

    let content = load_example(&format!("core/{}", relative_path));
    let filename = relative_path.rsplit('/').next().unwrap_or(relative_path);

    let result = timeout(CORE_LIB_TIMEOUT, async {
        let uri = harness.open_inline(filename, &content).await;
        tokio::task::yield_now().await;

        let hint_count = if decompile {
            let line_count = content.lines().count() as u32;
            let last_line_len = content.lines().last().map(|line| line.len()).unwrap_or(0) as u32;
            let range = Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: line_count,
                    character: last_line_len,
                },
            };
            harness.inlay_hints(&uri, range).await.len()
        } else {
            0
        };

        (uri, hint_count)
    })
    .await;

    match result {
        Ok((uri, hint_count)) => {
            let diags = harness.client.diagnostics_for(&uri).await;
            if !diags.is_empty() {
                eprintln!(
                    "Note: {} has {} diagnostics and {} inlay hints",
                    relative_path,
                    diags.len(),
                    hint_count
                );
            }
        }
        Err(_) => {
            panic!(
                "Timeout: Processing {} took longer than {:?}",
                relative_path, CORE_LIB_TIMEOUT
            );
        }
    }
}

#[tokio::test]
async fn core_lib_collections_smt() {
    run_core_library_file("collections/smt.masm", false).await;
}

#[tokio::test]
async fn core_lib_crypto_hashes_poseidon2() {
    run_core_library_file("crypto/hashes/poseidon2.masm", false).await;
}

#[tokio::test]
async fn core_lib_math_u64() {
    run_core_library_file("math/u64.masm", false).await;
}

#[tokio::test]
async fn core_lib_mem() {
    run_core_library_file("mem.masm", false).await;
}

#[tokio::test]
async fn core_lib_stark_verifier() {
    run_core_library_file("stark/verifier.masm", false).await;
}

#[tokio::test]
async fn core_lib_sys_vm_mod() {
    run_core_library_file("sys/vm/mod.masm", false).await;
}

#[tokio::test]
async fn core_lib_word() {
    run_core_library_file("word.masm", false).await;
}

#[tokio::test]
async fn core_lib_decompile_collections_smt() {
    run_core_library_file("collections/smt.masm", true).await;
}

#[tokio::test]
async fn core_lib_decompile_stark_verifier() {
    run_core_library_file("stark/verifier.masm", true).await;
}

#[tokio::test]
async fn core_lib_all_files_sequential() {
    let files = core_library_files();
    assert!(
        !files.is_empty(),
        "expected at least one core library example file"
    );

    let mut failed = Vec::new();
    let mut passed = 0;

    for file in &files {
        eprint!("Testing {}... ", file);

        let result = timeout(CORE_LIB_TIMEOUT, run_core_library_file(file, false)).await;
        match result {
            Ok(()) => {
                eprintln!("OK");
                passed += 1;
            }
            Err(_) => {
                eprintln!("TIMEOUT!");
                failed.push(file.clone());
            }
        }
    }

    eprintln!("\n=== Summary ===");
    eprintln!("Passed: {}/{}", passed, files.len());

    if !failed.is_empty() {
        eprintln!("Failed files:");
        for file in &failed {
            eprintln!("  - {}", file);
        }
        panic!("{} core library files timed out", failed.len());
    }
}

#[tokio::test]
async fn core_lib_decompile_all_files() {
    let files = core_library_files();
    assert!(
        !files.is_empty(),
        "expected at least one core library example file"
    );

    let mut failed = Vec::new();
    let mut passed = 0;

    for file in &files {
        eprint!("Decompiling {}... ", file);

        let result = timeout(CORE_LIB_TIMEOUT, run_core_library_file(file, true)).await;
        match result {
            Ok(()) => {
                eprintln!("OK");
                passed += 1;
            }
            Err(_) => {
                eprintln!("TIMEOUT!");
                failed.push(file.clone());
            }
        }
    }

    eprintln!("\n=== Decompilation Summary ===");
    eprintln!("Passed: {}/{}", passed, files.len());

    if !failed.is_empty() {
        eprintln!("Failed files:");
        for file in &failed {
            eprintln!("  - {}", file);
        }
        panic!(
            "{} core library files timed out during decompilation",
            failed.len()
        );
    }
}
