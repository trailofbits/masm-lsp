//! Criterion benchmarks for decompilation performance.
//!
//! These benchmarks measure the time to generate pseudocode hints for various
//! Miden assembly files.
//!
//! Run with: `cargo bench --bench decompilation`

use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use masm_lsp::decompiler::collect_decompilation_hints;
use miden_assembly_syntax::ast::ModuleKind;
use miden_assembly_syntax::{Parse, ParseOptions};
use miden_debug_types::{DefaultSourceManager, SourceLanguage, SourceManager};
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;
use tower_lsp::lsp_types::{Position, Range, Url};

/// Load a stdlib file from the examples directory
fn load_stdlib_file(relative_path: &str) -> Option<String> {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("examples");
    path.push("stdlib");
    path.push(relative_path);

    fs::read_to_string(&path).ok()
}

/// Parse a MASM source file and return the module along with its source manager
fn parse_masm(
    source: &str,
) -> Option<(
    Box<miden_assembly_syntax::ast::Module>,
    Arc<DefaultSourceManager>,
)> {
    let sources = Arc::new(DefaultSourceManager::default());
    let uri: miden_debug_types::Uri = "file:///benchmark.masm".parse().unwrap();
    sources.load(SourceLanguage::Masm, uri.clone(), source.to_string());

    let source_file = sources.get_by_uri(&uri)?;
    let opts = ParseOptions {
        kind: ModuleKind::Library,
        path: None,
        ..Default::default()
    };

    let module = source_file
        .parse_with_options(sources.as_ref(), opts)
        .ok()?;
    Some((module, sources))
}

/// Benchmark files that previously caused exponential blowup
fn bench_exponential_risk_files(c: &mut Criterion) {
    let mut group = c.benchmark_group("exponential_risk");

    // These files contain repeat.N { dup; mul } patterns that can cause
    // exponential blowup during abstract interpretation.
    let files = [
        ("base_field_ecgfp5", "math/ecgfp5/base_field.masm"),
        ("scalar_field_ecgfp5", "math/ecgfp5/scalar_field.masm"),
        ("group_ecgfp5", "math/ecgfp5/group.masm"),
        ("base_field_secp256k1", "math/secp256k1/base_field.masm"),
        ("scalar_field_secp256k1", "math/secp256k1/scalar_field.masm"),
        ("group_secp256k1", "math/secp256k1/group.masm"),
    ];

    for (name, path) in files {
        let source = match load_stdlib_file(path) {
            Some(s) => s,
            None => continue,
        };

        let (module, sources) = match parse_masm(&source) {
            Some(m) => m,
            None => continue,
        };

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

        let uri = Url::parse("file:///benchmark.masm").unwrap();

        group.throughput(Throughput::Bytes(source.len() as u64));
        group.bench_with_input(BenchmarkId::from_parameter(name), &(), |b, _| {
            b.iter(|| {
                collect_decompilation_hints(
                    &module,
                    &sources,
                    &uri,
                    &visible_range,
                    4,
                    &source,
                    None,
                )
            })
        });
    }

    group.finish();
}

/// Benchmark a variety of stdlib files
fn bench_stdlib_files(c: &mut Criterion) {
    let mut group = c.benchmark_group("stdlib");

    let files = [
        ("sha256", "crypto/hashes/sha256.masm"),
        ("blake3", "crypto/hashes/blake3.masm"),
        ("rpo_falcon512", "crypto/dsa/rpo_falcon512.masm"),
        ("smt", "collections/smt.masm"),
        ("u64", "math/u64.masm"),
        ("u256", "math/u256.masm"),
    ];

    for (name, path) in files {
        let source = match load_stdlib_file(path) {
            Some(s) => s,
            None => continue,
        };

        let (module, sources) = match parse_masm(&source) {
            Some(m) => m,
            None => continue,
        };

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

        let uri = Url::parse("file:///benchmark.masm").unwrap();

        group.throughput(Throughput::Bytes(source.len() as u64));
        group.bench_with_input(BenchmarkId::from_parameter(name), &(), |b, _| {
            b.iter(|| {
                collect_decompilation_hints(
                    &module,
                    &sources,
                    &uri,
                    &visible_range,
                    4,
                    &source,
                    None,
                )
            })
        });
    }

    group.finish();
}

criterion_group!(benches, bench_exponential_risk_files, bench_stdlib_files,);

criterion_main!(benches);
