//! Integration tests for the Miden standard library.
//!
//! These tests verify that the LSP can handle all stdlib files without hanging
//! or consuming excessive memory. Each test opens a stdlib file and verifies
//! that parsing completes within a reasonable timeout.

mod common;

use common::fixtures::load_example;
use common::harness::TestHarness;
use std::time::Duration;
use tokio::time::timeout;

/// Default timeout for stdlib file processing (5 seconds should be plenty for any file)
const STDLIB_TIMEOUT: Duration = Duration::from_secs(5);

/// Helper to run a stdlib test with timeout
async fn test_stdlib_file(relative_path: &str) {
    let harness = TestHarness::new().await;
    let content = load_example(&format!("stdlib/{}", relative_path));

    // Extract filename for the test URI
    let filename = relative_path.rsplit('/').next().unwrap_or(relative_path);

    let result = timeout(STDLIB_TIMEOUT, async {
        let uri = harness.open_inline(filename, &content).await;
        tokio::task::yield_now().await;
        uri
    })
    .await;

    match result {
        Ok(uri) => {
            // File opened successfully within timeout
            // We don't require zero diagnostics since stdlib files may have
            // unresolved references to other stdlib modules
            let diags = harness.client.diagnostics_for(&uri).await;
            // Just log if there are diagnostics, don't fail
            if !diags.is_empty() {
                eprintln!(
                    "Note: {} has {} diagnostics (may include unresolved refs)",
                    relative_path,
                    diags.len()
                );
            }
        }
        Err(_) => {
            panic!(
                "TIMEOUT: Processing {} took longer than {:?}",
                relative_path, STDLIB_TIMEOUT
            );
        }
    }
}

// =============================================================================
// Collections module tests
// =============================================================================

#[tokio::test]
async fn stdlib_collections_sorted_array() {
    test_stdlib_file("collections/sorted_array.masm").await;
}

#[tokio::test]
async fn stdlib_collections_mmr() {
    test_stdlib_file("collections/mmr.masm").await;
}

#[tokio::test]
async fn stdlib_collections_smt() {
    test_stdlib_file("collections/smt.masm").await;
}

// =============================================================================
// Crypto hashes module tests
// =============================================================================

#[tokio::test]
async fn stdlib_crypto_hashes_sha256() {
    test_stdlib_file("crypto/hashes/sha256.masm").await;
}

#[tokio::test]
async fn stdlib_crypto_hashes_keccak256() {
    test_stdlib_file("crypto/hashes/keccak256.masm").await;
}

#[tokio::test]
async fn stdlib_crypto_hashes_blake3() {
    test_stdlib_file("crypto/hashes/blake3.masm").await;
}

#[tokio::test]
async fn stdlib_crypto_hashes_rpo() {
    test_stdlib_file("crypto/hashes/rpo.masm").await;
}

// =============================================================================
// Crypto DSA module tests
// =============================================================================

#[tokio::test]
async fn stdlib_crypto_dsa_ecdsa_secp256k1() {
    test_stdlib_file("crypto/dsa/ecdsa/secp256k1.masm").await;
}

#[tokio::test]
async fn stdlib_crypto_dsa_rpo_falcon512() {
    test_stdlib_file("crypto/dsa/rpo_falcon512.masm").await;
}

// =============================================================================
// Crypto ElGamal module tests
// =============================================================================

#[tokio::test]
async fn stdlib_crypto_elgamal_ecgfp5() {
    test_stdlib_file("crypto/elgamal_ecgfp5.masm").await;
}

// =============================================================================
// Math ecgfp5 module tests
// =============================================================================

#[tokio::test]
async fn stdlib_math_ecgfp5_base_field() {
    test_stdlib_file("math/ecgfp5/base_field.masm").await;
}

#[tokio::test]
async fn stdlib_math_ecgfp5_scalar_field() {
    test_stdlib_file("math/ecgfp5/scalar_field.masm").await;
}

#[tokio::test]
async fn stdlib_math_ecgfp5_group() {
    test_stdlib_file("math/ecgfp5/group.masm").await;
}

// =============================================================================
// Math secp256k1 module tests
// =============================================================================

#[tokio::test]
async fn stdlib_math_secp256k1_base_field() {
    test_stdlib_file("math/secp256k1/base_field.masm").await;
}

#[tokio::test]
async fn stdlib_math_secp256k1_scalar_field() {
    test_stdlib_file("math/secp256k1/scalar_field.masm").await;
}

#[tokio::test]
async fn stdlib_math_secp256k1_group() {
    test_stdlib_file("math/secp256k1/group.masm").await;
}

// =============================================================================
// Math module tests
// =============================================================================

#[tokio::test]
async fn stdlib_math_u64() {
    test_stdlib_file("math/u64.masm").await;
}

#[tokio::test]
async fn stdlib_math_u256() {
    test_stdlib_file("math/u256.masm").await;
}

// =============================================================================
// Memory module tests
// =============================================================================

#[tokio::test]
async fn stdlib_mem() {
    test_stdlib_file("mem.masm").await;
}

// =============================================================================
// PCS FRI module tests
// =============================================================================

#[tokio::test]
async fn stdlib_pcs_fri_frie2f4() {
    test_stdlib_file("pcs/fri/frie2f4.masm").await;
}

#[tokio::test]
async fn stdlib_pcs_fri_helper() {
    test_stdlib_file("pcs/fri/helper.masm").await;
}

// =============================================================================
// STARK module tests
// =============================================================================

#[tokio::test]
async fn stdlib_stark_constants() {
    test_stdlib_file("stark/constants.masm").await;
}

#[tokio::test]
async fn stdlib_stark_mod() {
    test_stdlib_file("stark/mod.masm").await;
}

#[tokio::test]
async fn stdlib_stark_utils() {
    test_stdlib_file("stark/utils.masm").await;
}

#[tokio::test]
async fn stdlib_stark_ood_frames() {
    test_stdlib_file("stark/ood_frames.masm").await;
}

#[tokio::test]
async fn stdlib_stark_random_coin() {
    test_stdlib_file("stark/random_coin.masm").await;
}

#[tokio::test]
async fn stdlib_stark_deep_queries() {
    test_stdlib_file("stark/deep_queries.masm").await;
}

#[tokio::test]
async fn stdlib_stark_public_inputs() {
    test_stdlib_file("stark/public_inputs.masm").await;
}

#[tokio::test]
async fn stdlib_stark_verifier() {
    test_stdlib_file("stark/verifier.masm").await;
}

// =============================================================================
// Sys module tests
// =============================================================================

#[tokio::test]
async fn stdlib_sys_mod() {
    test_stdlib_file("sys/mod.masm").await;
}

#[tokio::test]
async fn stdlib_sys_vm_mod() {
    test_stdlib_file("sys/vm/mod.masm").await;
}

#[tokio::test]
async fn stdlib_sys_vm_constraints_eval() {
    test_stdlib_file("sys/vm/constraints_eval.masm").await;
}

#[tokio::test]
async fn stdlib_sys_vm_ood_frames() {
    test_stdlib_file("sys/vm/ood_frames.masm").await;
}

#[tokio::test]
async fn stdlib_sys_vm_deep_queries() {
    test_stdlib_file("sys/vm/deep_queries.masm").await;
}

#[tokio::test]
async fn stdlib_sys_vm_public_inputs() {
    test_stdlib_file("sys/vm/public_inputs.masm").await;
}

// =============================================================================
// Word module tests
// =============================================================================

#[tokio::test]
async fn stdlib_word() {
    test_stdlib_file("word.masm").await;
}

// =============================================================================
// Batch test for all files (useful to find which file causes issues)
// =============================================================================

/// Lists all stdlib files for reference
const STDLIB_FILES: &[&str] = &[
    "collections/sorted_array.masm",
    "collections/mmr.masm",
    "collections/smt.masm",
    "crypto/hashes/sha256.masm",
    "crypto/hashes/keccak256.masm",
    "crypto/hashes/blake3.masm",
    "crypto/hashes/rpo.masm",
    "crypto/dsa/ecdsa/secp256k1.masm",
    "crypto/dsa/rpo_falcon512.masm",
    "crypto/elgamal_ecgfp5.masm",
    "math/ecgfp5/base_field.masm",
    "math/ecgfp5/scalar_field.masm",
    "math/ecgfp5/group.masm",
    "math/secp256k1/base_field.masm",
    "math/secp256k1/scalar_field.masm",
    "math/secp256k1/group.masm",
    "math/u64.masm",
    "math/u256.masm",
    "mem.masm",
    "pcs/fri/frie2f4.masm",
    "pcs/fri/helper.masm",
    "stark/constants.masm",
    "stark/mod.masm",
    "stark/utils.masm",
    "stark/ood_frames.masm",
    "stark/random_coin.masm",
    "stark/deep_queries.masm",
    "stark/public_inputs.masm",
    "stark/verifier.masm",
    "sys/mod.masm",
    "sys/vm/mod.masm",
    "sys/vm/constraints_eval.masm",
    "sys/vm/ood_frames.masm",
    "sys/vm/deep_queries.masm",
    "sys/vm/public_inputs.masm",
    "word.masm",
];

/// Test all stdlib files sequentially with progress reporting
#[tokio::test]
async fn stdlib_all_files_sequential() {
    let mut failed = Vec::new();
    let mut passed = 0;

    for file in STDLIB_FILES {
        eprint!("Testing {}... ", file);

        let harness = TestHarness::new().await;
        let content = match std::panic::catch_unwind(|| load_example(&format!("stdlib/{}", file))) {
            Ok(c) => c,
            Err(_) => {
                eprintln!("SKIP (file not found)");
                continue;
            }
        };

        let filename = file.rsplit('/').next().unwrap_or(file);

        let result = timeout(STDLIB_TIMEOUT, async {
            let uri = harness.open_inline(filename, &content).await;
            tokio::task::yield_now().await;
            uri
        })
        .await;

        match result {
            Ok(_) => {
                eprintln!("OK");
                passed += 1;
            }
            Err(_) => {
                eprintln!("TIMEOUT!");
                failed.push(*file);
            }
        }
    }

    eprintln!("\n=== Summary ===");
    eprintln!("Passed: {}/{}", passed, STDLIB_FILES.len());

    if !failed.is_empty() {
        eprintln!("Failed files:");
        for f in &failed {
            eprintln!("  - {}", f);
        }
        panic!("{} stdlib files timed out", failed.len());
    }
}

// =============================================================================
// Decompilation (inlay hints) tests
// These tests verify that the decompilation analysis doesn't hang or consume
// excessive memory when processing stdlib files.
// =============================================================================

use tower_lsp::lsp_types::{Position, Range};

/// Helper to run a stdlib file test with decompilation (inlay hints) enabled
async fn test_stdlib_file_with_decompilation(relative_path: &str) {
    let harness = TestHarness::new().await;
    harness.enable_decompilation_hints().await;

    let content = load_example(&format!("stdlib/{}", relative_path));
    let filename = relative_path.rsplit('/').next().unwrap_or(relative_path);

    let result = timeout(STDLIB_TIMEOUT, async {
        let uri = harness.open_inline(filename, &content).await;
        tokio::task::yield_now().await;

        // Count lines to create full document range
        let line_count = content.lines().count() as u32;
        let last_line_len = content.lines().last().map(|l| l.len()).unwrap_or(0) as u32;

        // Request inlay hints for the entire document
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

        let hints = harness.inlay_hints(&uri, range).await;
        (uri, hints.len())
    })
    .await;

    match result {
        Ok((uri, hint_count)) => {
            let diags = harness.client.diagnostics_for(&uri).await;
            eprintln!(
                "{}: {} inlay hints, {} diagnostics",
                relative_path,
                hint_count,
                diags.len()
            );
        }
        Err(_) => {
            panic!(
                "TIMEOUT: Decompilation of {} took longer than {:?}",
                relative_path, STDLIB_TIMEOUT
            );
        }
    }
}

/// Test decompilation on all stdlib files sequentially
#[tokio::test]
async fn stdlib_decompile_all_files() {
    let mut failed = Vec::new();
    let mut passed = 0;

    for file in STDLIB_FILES {
        eprint!("Decompiling {}... ", file);

        let harness = TestHarness::new().await;
        harness.enable_decompilation_hints().await;

        let content = match std::panic::catch_unwind(|| load_example(&format!("stdlib/{}", file))) {
            Ok(c) => c,
            Err(_) => {
                eprintln!("SKIP (file not found)");
                continue;
            }
        };

        let filename = file.rsplit('/').next().unwrap_or(file);

        let result = timeout(STDLIB_TIMEOUT, async {
            let uri = harness.open_inline(filename, &content).await;
            tokio::task::yield_now().await;

            // Count lines to create full document range
            let line_count = content.lines().count() as u32;
            let last_line_len = content.lines().last().map(|l| l.len()).unwrap_or(0) as u32;

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

            let hints = harness.inlay_hints(&uri, range).await;
            hints.len()
        })
        .await;

        match result {
            Ok(hint_count) => {
                eprintln!("OK ({} hints)", hint_count);
                passed += 1;
            }
            Err(_) => {
                eprintln!("TIMEOUT!");
                failed.push(*file);
            }
        }
    }

    eprintln!("\n=== Decompilation Summary ===");
    eprintln!("Passed: {}/{}", passed, STDLIB_FILES.len());

    if !failed.is_empty() {
        eprintln!("Failed files:");
        for f in &failed {
            eprintln!("  - {}", f);
        }
        panic!(
            "{} stdlib files timed out during decompilation",
            failed.len()
        );
    }
}

// Individual decompilation tests for selected complex files
// These are the most likely to cause hangs

#[tokio::test]
async fn stdlib_decompile_math_ecgfp5_group() {
    test_stdlib_file_with_decompilation("math/ecgfp5/group.masm").await;
}

#[tokio::test]
async fn stdlib_decompile_math_secp256k1_group() {
    test_stdlib_file_with_decompilation("math/secp256k1/group.masm").await;
}

#[tokio::test]
async fn stdlib_decompile_crypto_dsa_rpo_falcon512() {
    test_stdlib_file_with_decompilation("crypto/dsa/rpo_falcon512.masm").await;
}

#[tokio::test]
async fn stdlib_decompile_stark_verifier() {
    test_stdlib_file_with_decompilation("stark/verifier.masm").await;
}

#[tokio::test]
async fn stdlib_decompile_collections_smt() {
    test_stdlib_file_with_decompilation("collections/smt.masm").await;
}
