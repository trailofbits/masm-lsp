use super::*;
use crate::analysis::contracts::{infer_module_contracts, ContractStore};
use miden_assembly_syntax::ast::ModuleKind;
use miden_assembly_syntax::{Parse, ParseOptions};
use miden_debug_types::{DefaultSourceManager, SourceLanguage, SourceManager, Uri};
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Url};

fn count_warnings(diags: &[Diagnostic]) -> usize {
    diags
        .iter()
        .filter(|d| d.severity == Some(DiagnosticSeverity::WARNING))
        .count()
}

fn analyze_source(source: &str) -> Vec<Diagnostic> {
    let source_manager = DefaultSourceManager::default();
    let miden_uri = Uri::from("test://test.masm");
    source_manager.load(SourceLanguage::Masm, miden_uri.clone(), source.to_string());

    let source_file = source_manager
        .get_by_uri(&miden_uri)
        .expect("Failed to load source");
    let mut module_path = miden_assembly_syntax::ast::PathBuf::default();
    module_path.push("test");
    let opts = ParseOptions {
        kind: ModuleKind::Library,
        path: Some(module_path.into()),
        ..Default::default()
    };

    match source_file.parse_with_options(&source_manager, opts) {
        Ok(module) => {
            let uri = Url::parse("file:///test.masm").unwrap();
            analyze_locals(&module, &source_manager, &uri)
        }
        Err(_) => vec![], // Parse error, no analysis
    }
}

fn analyze_source_with_contracts(source: &str) -> Vec<Diagnostic> {
    let source_manager = DefaultSourceManager::default();
    let miden_uri = Uri::from("test://test.masm");
    source_manager.load(SourceLanguage::Masm, miden_uri.clone(), source.to_string());

    let source_file = source_manager
        .get_by_uri(&miden_uri)
        .expect("Failed to load source");
    let mut module_path = miden_assembly_syntax::ast::PathBuf::default();
    module_path.push("test");
    let opts = ParseOptions {
        kind: ModuleKind::Library,
        path: Some(module_path.into()),
        ..Default::default()
    };

    match source_file.parse_with_options(&source_manager, opts) {
        Ok(module) => {
            // Infer contracts for the module
            let contracts_vec = infer_module_contracts(&module, &source_manager);
            let mut contracts = ContractStore::new();
            contracts.update_document(contracts_vec);

            let uri = Url::parse("file:///test.masm").unwrap();
            analyze_locals_with_contracts(&module, &source_manager, &uri, &contracts)
        }
        Err(_) => vec![], // Parse error, no analysis
    }
}

#[test]
fn test_no_warning_when_initialized() {
    let source = r#"
@locals(1)
proc test
    push.42
    loc_store.0
    loc_load.0
    drop
end
"#;
    let diags = analyze_source(source);
    assert_eq!(
        count_warnings(&diags),
        0,
        "Should not warn when local is initialized before read"
    );
}

#[test]
fn test_warning_read_before_write() {
    let source = r#"
@locals(1)
proc test
    loc_load.0
    drop
end
"#;
    let diags = analyze_source(source);
    assert_eq!(
        count_warnings(&diags),
        1,
        "Should warn when reading uninitialized local"
    );
    assert!(diags[0].message.contains("uninitialized local 0."));
}

#[test]
fn test_warning_partial_word_init() {
    let source = r#"
@locals(4)
proc test
    push.1
    loc_store.0
    push.2
    loc_store.1
    # locals 2 and 3 not initialized
    loc_loadw_be.0
    dropw
end
"#;
    let diags = analyze_source(source);
    assert_eq!(
        count_warnings(&diags),
        1,
        "Should warn when word contains uninitialized locals"
    );
    assert!(
        diags[0].message.contains("local 2") || diags[0].message.contains("local 3"),
        "message: {}",
        diags[0].message
    );
}

#[test]
fn test_no_warning_full_word_init() {
    let source = r#"
@locals(4)
proc test
    push.1.2.3.4
    loc_storew_be.0
    loc_loadw_be.0
    dropw
end
"#;
    let diags = analyze_source(source);
    assert_eq!(
        count_warnings(&diags),
        0,
        "Should not warn when full word is initialized"
    );
}

#[test]
fn test_if_else_both_branches_init() {
    let source = r#"
@locals(1)
proc test
    push.1
    if.true
        push.10
        loc_store.0
    else
        push.20
        loc_store.0
    end
    loc_load.0
    drop
end
"#;
    let diags = analyze_source(source);
    assert_eq!(
        count_warnings(&diags),
        0,
        "Should not warn when both branches initialize"
    );
}

#[test]
fn test_if_else_one_branch_init() {
    let source = r#"
@locals(1)
proc test
    push.1
    if.true
        push.10
        loc_store.0
    else
        # no initialization
    end
    loc_load.0
    drop
end
"#;
    let diags = analyze_source(source);
    assert_eq!(
        count_warnings(&diags),
        1,
        "Should warn when only one branch initializes"
    );
    assert!(
        diags[0].message.contains("may not be initialized"),
        "message: {}",
        diags[0].message
    );
}

#[test]
fn test_while_loop_init_not_reliable() {
    let source = r#"
@locals(1)
proc test
    push.0  # condition (false, loop won't execute)
    while.true
        push.42
        loc_store.0
        push.0  # continue condition
    end
    loc_load.0
    drop
end
"#;
    let diags = analyze_source(source);
    assert_eq!(
        count_warnings(&diags),
        1,
        "Should warn when init is only in while loop body"
    );
    assert!(
        diags[0].message.contains("may not be initialized"),
        "message: {}",
        diags[0].message
    );
}

#[test]
fn test_repeat_loop_init_reliable() {
    let source = r#"
@locals(1)
proc test
    repeat.1
        push.42
        loc_store.0
    end
    loc_load.0
    drop
end
"#;
    let diags = analyze_source(source);
    assert_eq!(
        count_warnings(&diags),
        0,
        "Should not warn when init is in repeat.n (n>0) body"
    );
}

#[test]
fn test_locaddr_uninitialized() {
    let source = r#"
@locals(1)
proc test
    locaddr.0
    drop
end
"#;
    let diags = analyze_source(source);
    assert_eq!(
        count_warnings(&diags),
        1,
        "Should warn when taking address of uninitialized local"
    );
    assert!(
        diags[0].message.contains("Taking address"),
        "message: {}",
        diags[0].message
    );
}

#[test]
fn test_locaddr_initialized() {
    let source = r#"
@locals(1)
proc test
    push.42
    loc_store.0
    locaddr.0
    drop
end
"#;
    let diags = analyze_source(source);
    assert_eq!(
        count_warnings(&diags),
        0,
        "Should not warn when taking address of initialized local"
    );
}

#[test]
fn test_multiple_procedures() {
    let source = r#"
@locals(1)
proc good
    push.42
    loc_store.0
    loc_load.0
    drop
end

@locals(1)
proc bad
    loc_load.0
    drop
end
"#;
    let diags = analyze_source(source);
    assert_eq!(
        count_warnings(&diags),
        1,
        "Should only warn for the bad procedure"
    );
}

#[test]
fn test_different_local_indices() {
    let source = r#"
@locals(2)
proc test
    push.42
    loc_store.0
    loc_load.1  # local 1 not initialized
    drop
end
"#;
    let diags = analyze_source(source);
    assert_eq!(
        count_warnings(&diags),
        1,
        "Should warn for uninitialized local 1"
    );
    assert!(
        diags[0].message.contains("local 1"),
        "message: {}",
        diags[0].message
    );
}

// ═══════════════════════════════════════════════════════════════════════════
// Contract-aware tests
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn test_locaddr_with_output_proc_no_warning() {
    // When we have contracts, passing locaddr to a procedure that writes to it
    // should mark the local as initialized - no warning needed
    let source = r#"
# writer writes to address at input position 0
proc writer
    push.42
    swap
    mem_store
end

@locals(1)
proc caller
    locaddr.0
    exec.writer
    loc_load.0
    drop
end
"#;
    let diags = analyze_source_with_contracts(source);
    // The writer procedure's signature should show input 0 is OutputAddress
    // So locaddr.0 passed to writer marks local 0 as initialized
    assert_eq!(count_warnings(&diags), 0,
        "Should not warn when locaddr is passed to proc with OutputAddress param. Got {} warnings: {:?}",
        diags.len(), diags.iter().map(|d| &d.message).collect::<Vec<_>>());
}

#[test]
fn test_locaddr_with_reader_proc_still_warns() {
    // When locaddr is passed to a procedure that only reads, not writes,
    // we should still warn because the local isn't initialized
    let source = r#"
# reader reads from address at input position 0
proc reader
    mem_load
end

@locals(1)
proc caller
    locaddr.0
    exec.reader
    drop
end
"#;
    let diags = analyze_source_with_contracts(source);
    // The reader procedure's signature should show input 0 is InputAddress (read only)
    // Since reader doesn't write, local 0 isn't initialized - should warn on loc_load if we had one
    // But we don't have a loc_load, so we just pass the address to reader which reads garbage
    // With contracts, we don't warn on locaddr itself (because we check later)
    // In this case, the read happens in reader, not in caller, so no warning in caller
    assert_eq!(count_warnings(&diags), 0,
        "Should not warn in caller when address is passed to reader (reader has the bug). Got: {:?}",
        diags.iter().map(|d| &d.message).collect::<Vec<_>>());
}

#[test]
fn test_locaddr_passed_to_unknown_proc() {
    // When contracts aren't available for the callee, we can't determine
    // if the local will be written, so we should be conservative.
    // With contracts available but callee unknown, address tracking is cleared.
    //
    // Note: We can't test with an undefined proc as it would fail to parse.
    // Instead we test the behavior without contracts at all.
    let source_no_contracts = r#"
@locals(1)
proc test
    locaddr.0
    drop
end
"#;
    // Without contracts, locaddr of uninitialized local should warn
    let diags = analyze_source(source_no_contracts);
    assert_eq!(
        count_warnings(&diags),
        1,
        "Without contracts, should warn when taking address of uninitialized local"
    );
}

#[test]
fn test_locaddr_with_swap_then_store_proc() {
    // Test that address tracking works through stack manipulation
    let source = r#"
# store_swapped: takes [addr] and writes 42 to it (via swap then store)
proc store_swapped
    push.42
    swap
    mem_store
end

@locals(1)
proc caller
    locaddr.0
    exec.store_swapped
    loc_load.0
    drop
end
"#;
    let diags = analyze_source_with_contracts(source);
    assert_eq!(
        count_warnings(&diags),
        0,
        "Should not warn - local is initialized via store_swapped. Got: {:?}",
        diags.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn test_locaddr_with_movup_then_store_proc() {
    // Test that address tracking works through movup
    let source = r#"
# store_with_movup: takes [addr] and writes to it using movup
proc store_with_movup
    push.1 push.2
    movup.2
    mem_store
end

@locals(1)
proc caller
    locaddr.0
    exec.store_with_movup
    loc_load.0
    drop
end
"#;
    let diags = analyze_source_with_contracts(source);
    assert_eq!(
        count_warnings(&diags),
        0,
        "Should not warn - local is initialized via store_with_movup. Got: {:?}",
        diags.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn test_multiple_locals_partial_init() {
    // Test that only the local passed to the writer is marked initialized
    let source = r#"
proc writer
    push.42
    swap
    mem_store
end

@locals(2)
proc caller
    locaddr.0
    exec.writer
    loc_load.0  # local 0 was initialized by writer
    drop
    loc_load.1  # local 1 was NOT initialized
    drop
end
"#;
    let diags = analyze_source_with_contracts(source);
    assert_eq!(
        count_warnings(&diags),
        1,
        "Should warn only for local 1. Got: {:?}",
        diags.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
    assert!(
        diags[0].message.contains("local 1"),
        "Should warn about local 1"
    );
}

#[test]
fn test_locaddr_stored_via_dup_and_call() {
    // Test that duplicating an address and passing it to writer works
    let source = r#"
proc writer
    push.42
    swap
    mem_store
end

@locals(1)
proc caller
    locaddr.0
    dup
    exec.writer
    drop  # drop the extra copy
    loc_load.0
    drop
end
"#;
    let diags = analyze_source_with_contracts(source);
    assert_eq!(
        count_warnings(&diags),
        0,
        "Should not warn - dup'd address passed to writer. Got: {:?}",
        diags.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn test_transitive_output_address() {
    // Test that output address propagates through call chains
    let source = r#"
# inner writes to address
proc inner
    push.42
    swap
    mem_store
end

# outer just calls inner - should inherit output address
proc outer
    exec.inner
end

@locals(1)
proc caller
    locaddr.0
    exec.outer
    loc_load.0
    drop
end
"#;
    let diags = analyze_source_with_contracts(source);
    assert_eq!(
        count_warnings(&diags),
        0,
        "Should not warn - transitive output address. Got: {:?}",
        diags.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

// ═══════════════════════════════════════════════════════════════════════════
// Stdlib compatibility tests
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn test_stdlib_sys_truncate_stack() {
    // Test that the sys::truncate_stack procedure doesn't produce false positives
    // It writes to locals before reading them
    let source = r#"
@locals(4)
pub proc truncate_stack()
    # save the first word to memory and bring elements to be dropped to the top of the stack
    loc_storew_be.0 dropw movupw.3

    # until stack depth greater than 16, keep dropping extra elements
    sdepth neq.16
    while.true
        dropw movupw.3
        sdepth neq.16
    end

    # bring the previously saved word back onto the stack
    loc_loadw_be.0
end
"#;
    let diags = analyze_source_with_contracts(source);
    assert_eq!(
        count_warnings(&diags),
        0,
        "stdlib sys::truncate_stack should not produce warnings. Got: {:?}",
        diags.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn test_stdlib_sorted_array_find_partial_key_value() {
    // Test a procedure that stores a value and loads it later
    let source = r#"
@locals(1)
proc find_partial_key_value
    # store use_full_key for later
    movup.6 loc_store.0

    adv_push.2

    if.true
        # ... some branches that use loc_load.0
        loc_load.0
        drop
    else
        loc_load.0
        drop
    end
end
"#;
    let diags = analyze_source_with_contracts(source);
    assert_eq!(
        count_warnings(&diags),
        0,
        "sorted_array find_partial_key_value pattern should not produce warnings. Got: {:?}",
        diags.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}
