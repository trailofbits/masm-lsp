//! Per-procedure decompilation probe for `examples/stdlib/math/ecgfp5/base_field.masm`.
//!
//! This uses the real parser (`miden-assembly-syntax`) to load the full module,
//! then slices out each procedure by span and runs decompilation on that slice
//! to see which ones are expensive or hang. Ignored by default; run with:
//! `cargo test ecgfp5_base_field_probe -- --nocapture --ignored`

use std::time::Instant;

mod common;

use masm_lsp::decompiler::collect_decompilation_hints;
use miden_assembly_syntax::ast::ModuleKind;
use miden_assembly_syntax::{Parse, ParseOptions};
use miden_debug_types::{DefaultSourceManager, SourceLanguage, SourceManager, Spanned};
use tower_lsp::lsp_types::{Position, Range, Url};

use crate::common::fixtures::load_example_module;

#[test]
#[ignore = "manual profiling of ecgfp5 base_field procedures"]
fn ecgfp5_base_field_probe() {
    let (content, _sm, module) =
        load_example_module("stdlib/math/ecgfp5/base_field.masm").expect("load base_field");
    let content = std::sync::Arc::new(content);

    let full_uri = Url::parse("file:///probe/base_field.masm").unwrap();

    eprintln!("Found {} procedures", module.procedures().count());
    for proc in module.procedures() {
        eprintln!("starting {}", proc.name());
        let byte_range = proc.span().into_range();
        let start = byte_range.start as usize;
        let end = byte_range.end as usize;
        let slice = content[start..end].to_string();
        let name = proc.name().to_string();
        let url = full_uri.clone();
        let content_for_thread = content.clone();
        let start_line_offset = content[..start].chars().filter(|&c| c == '\n').count() as u32;

        let (tx, rx) = std::sync::mpsc::channel();
        std::thread::spawn(move || {
            // Reuse the full module but keep visible range tight to this proc.
            let pm = DefaultSourceManager::default();
            let uri = miden_debug_types::Uri::from("file:///probe/base_field_full.masm");
            pm.load(
                SourceLanguage::Masm,
                uri.clone(),
                (*content_for_thread).clone(),
            );
            let sf = match pm.get_by_uri(&uri) {
                Some(f) => f,
                None => {
                    let _ = tx.send(Err(format!("{name}: source load failed")));
                    return;
                }
            };

            let opts = ParseOptions {
                kind: ModuleKind::Library,
                path: None,
                ..Default::default()
            };
            let full_module = match sf.parse_with_options(&pm, opts) {
                Ok(m) => m,
                Err(err) => {
                    let _ = tx.send(Err(format!("{name}: parse failed in full module: {err}")));
                    return;
                }
            };

        let line_offset = start_line_offset;
        let line_count = slice.lines().count() as u32;
        let visible_range = Range {
            start: Position {
                line: line_offset,
                character: 0,
            },
            end: Position {
                line: line_offset + line_count,
                character: 0,
            },
        };

            let start_time = Instant::now();
            let result = collect_decompilation_hints(
                &full_module,
                &pm,
                &url,
                &visible_range,
                4,
                &content_for_thread,
                None,
            );
            let elapsed = start_time.elapsed();

            let _ = tx.send(Ok((elapsed, result.hints.len(), result.diagnostics.len())));
        });

        match rx.recv_timeout(std::time::Duration::from_secs(10)) {
            Ok(Ok((elapsed, hints, diags))) => {
                eprintln!(
                    "{:>20}: {:>8?}  hints={}  diags={}",
                    proc.name(),
                    elapsed,
                    hints,
                    diags
                );
            }
            Ok(Err(msg)) => {
                eprintln!("{msg}");
            }
            Err(_) => {
                eprintln!("{:>20}: TIMEOUT", proc.name());
            }
        }
    }
}
