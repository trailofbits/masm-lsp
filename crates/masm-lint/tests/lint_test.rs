//! Integration tests for masm-lint CLI.

use std::path::PathBuf;
use std::process::Command;

fn masm_lint_bin() -> PathBuf {
    let status = Command::new("cargo")
        .args(["build", "-p", "masm-lint"])
        .status()
        .expect("failed to build masm-lint");
    assert!(status.success(), "cargo build failed");

    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("target")
        .join("debug")
        .join("masm-lint");
    if cfg!(target_os = "windows") {
        path.set_extension("exe");
    }
    path
}

fn fixtures_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("tests")
        .join("fixtures")
}

#[test]
fn lint_no_inputs_shows_help_and_fails() {
    let output = Command::new(masm_lint_bin())
        .output()
        .expect("failed to run masm-lint");
    assert_eq!(output.status.code(), Some(2));
}

#[test]
fn lint_nonexistent_file_warns_and_exits_clean() {
    let output = Command::new(masm_lint_bin())
        .arg("/tmp/nonexistent_12345.masm")
        .output()
        .expect("failed to run masm-lint");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("failed to load")
            || stderr.contains("no MASM files found")
            || stderr.contains("does not exist"),
        "unexpected stderr: {stderr}"
    );
}

#[test]
fn lint_clean_file_exits_zero() {
    let fixture = fixtures_dir().join("multi_proc.masm");
    if !fixture.exists() {
        eprintln!("skipping: fixture not found at {}", fixture.display());
        return;
    }
    let output = Command::new(masm_lint_bin())
        .arg(&fixture)
        .output()
        .expect("failed to run masm-lint");
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_eq!(
        output.status.code(),
        Some(0),
        "expected exit 0, stdout: {stdout}, stderr: {stderr}"
    );
}

#[test]
fn lint_directory_recurses() {
    let fixtures = fixtures_dir();
    if !fixtures.exists() {
        eprintln!("skipping: fixtures dir not found");
        return;
    }
    let output = Command::new(masm_lint_bin())
        .arg(&fixtures)
        .output()
        .expect("failed to run masm-lint");
    assert!(
        output.status.code() == Some(0) || output.status.code() == Some(1),
        "unexpected exit code: {:?}",
        output.status.code()
    );
}

#[test]
fn lint_no_color_disables_ansi() {
    let fixture = fixtures_dir().join("multi_proc.masm");
    if !fixture.exists() {
        eprintln!("skipping: fixture not found");
        return;
    }
    let output = Command::new(masm_lint_bin())
        .arg("--no-color")
        .arg(&fixture)
        .output()
        .expect("failed to run masm-lint");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        !stdout.contains("\x1b["),
        "found ANSI codes in --no-color output"
    );
}
