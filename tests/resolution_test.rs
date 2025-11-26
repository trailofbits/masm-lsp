//! Integration tests for symbol resolution.
//!
//! These tests verify that goto definition and symbol resolution work correctly
//! across various scenarios.

mod common;

use common::fixtures::{find_position, inline};
use common::harness::TestHarness;
use tower_lsp::lsp_types::Position;

#[tokio::test]
async fn resolve_local_proc_definition() {
    let harness = TestHarness::new().await;
    let content = inline::SIMPLE_EXECUTABLE;
    let uri = harness.open_inline("local.masm", content).await;

    // Position cursor on "foo" in "exec.foo"
    let pos = find_position(content, "foo");
    let result = harness.goto_definition(&uri, pos).await;

    assert!(result.is_some(), "expected to find definition");
    let loc = result.unwrap();
    assert_eq!(loc.uri, uri);
    // Definition should be on line 0 where "proc foo" is declared
    assert_eq!(loc.range.start.line, 0);
}

#[tokio::test]
async fn resolve_proc_call_via_call_instruction() {
    let harness = TestHarness::new().await;
    let content = r#"proc foo
    push.1
end

proc bar
    call.foo
end
"#;
    let uri = harness.open_inline("call_inst.masm", content).await;

    let pos = find_position(content, "foo");
    let result = harness.goto_definition(&uri, pos).await;

    assert!(result.is_some(), "expected to find definition");
    let loc = result.unwrap();
    assert_eq!(loc.range.start.line, 0);
}

#[tokio::test]
async fn resolve_procref_target() {
    let harness = TestHarness::new().await;
    let content = inline::WITH_PROCREF;
    let uri = harness.open_inline("procref.masm", content).await;

    let pos = find_position(content, "target");
    let result = harness.goto_definition(&uri, pos).await;

    assert!(result.is_some(), "expected to find definition");
    let loc = result.unwrap();
    assert_eq!(loc.range.start.line, 0);
}

#[tokio::test]
async fn resolve_multiple_calls_to_same_proc() {
    let harness = TestHarness::new().await;
    let content = inline::MULTI_CALL;
    let uri = harness.open_inline("multi.masm", content).await;

    // First exec.f
    let pos = find_position(content, "exec.f");
    let result = harness.goto_definition(&uri, pos).await;

    assert!(result.is_some(), "expected to find definition");
    let loc = result.unwrap();
    assert_eq!(loc.range.start.line, 0);
}

#[tokio::test]
async fn resolve_nested_proc_call() {
    let harness = TestHarness::new().await;
    let content = r#"proc inner
    nop
end

proc outer
    if.true
        exec.inner
    end
end
"#;
    let uri = harness.open_inline("nested.masm", content).await;

    let pos = find_position(content, "inner");
    let result = harness.goto_definition(&uri, pos).await;

    assert!(result.is_some(), "expected to find definition");
    let loc = result.unwrap();
    assert_eq!(loc.range.start.line, 0);
}

#[tokio::test]
async fn resolve_cross_file_reference() {
    let harness = TestHarness::new().await;

    // Open definition file
    let def_uri = harness.open_inline("defs.masm", inline::CROSS_MODULE_DEF).await;

    // Open caller file
    let caller_content = inline::CROSS_MODULE_CALL;
    let caller_uri = harness.open_inline("caller.masm", caller_content).await;

    // Position on ::defs::foo
    let pos = find_position(caller_content, "::defs::foo");
    let result = harness.goto_definition(&caller_uri, pos).await;

    assert!(result.is_some(), "expected to find cross-file definition");
    let loc = result.unwrap();
    assert_eq!(loc.uri, def_uri);
}

#[tokio::test]
async fn resolve_at_proc_definition_returns_self() {
    let harness = TestHarness::new().await;
    let content = inline::SIMPLE_PROC;
    let uri = harness.open_inline("self.masm", content).await;

    // Position cursor on "foo" in "proc foo"
    let pos = Position::new(0, 5); // "proc foo" - position on 'f' of foo
    let result = harness.goto_definition(&uri, pos).await;

    assert!(result.is_some(), "expected to find definition");
    let loc = result.unwrap();
    // Should point to itself
    assert_eq!(loc.range.start.line, 0);
}

#[tokio::test]
async fn find_references_to_local_proc() {
    let harness = TestHarness::new().await;
    let content = inline::MULTI_CALL;
    let uri = harness.open_inline("refs.masm", content).await;

    // Position on the call site (exec.f) rather than the declaration
    let pos = find_position(content, "exec.f");
    let refs = harness.find_references(&uri, pos, true).await;

    // Should find definition + 3 calls
    assert!(
        refs.len() >= 4,
        "expected at least 4 references, got {}",
        refs.len()
    );
}

#[tokio::test]
async fn find_references_without_declaration() {
    let harness = TestHarness::new().await;
    let content = inline::MULTI_CALL;
    let uri = harness.open_inline("refs_no_decl.masm", content).await;

    // Position on the call site (exec.f)
    let pos = find_position(content, "exec.f");
    let refs = harness.find_references(&uri, pos, false).await;

    // Should find only calls (3), not the definition
    assert!(
        refs.len() >= 3,
        "expected at least 3 references, got {}",
        refs.len()
    );
}

#[tokio::test]
async fn find_references_cross_file() {
    let harness = TestHarness::new().await;

    // Open definition file
    let def_uri = harness.open_inline("defs.masm", inline::CROSS_MODULE_DEF).await;

    // Open two caller files
    let caller1 = harness.open_inline("caller1.masm", inline::CROSS_MODULE_CALL).await;
    let caller2 = harness.open_inline("caller2.masm", inline::CROSS_MODULE_CALL).await;

    // Find references from definition
    let pos = find_position(inline::CROSS_MODULE_DEF, "foo");
    let refs = harness.find_references(&def_uri, pos, true).await;

    // Should find definition + at least 2 call sites
    assert!(
        refs.len() >= 3,
        "expected at least 3 references, got {}",
        refs.len()
    );

    // Verify references are from multiple files
    let uris: std::collections::HashSet<_> = refs.iter().map(|r| &r.uri).collect();
    assert!(
        uris.contains(&def_uri),
        "expected definition URI in references"
    );
    assert!(
        uris.contains(&caller1) || uris.contains(&caller2),
        "expected at least one caller URI in references"
    );
}

#[tokio::test]
async fn workspace_symbol_partial_match() {
    let harness = TestHarness::new().await;
    let content = r#"proc calculate_sum
    add
end

proc calculate_product
    mul
end
"#;
    let _uri = harness.open_inline("calc.masm", content).await;

    let symbols = harness.workspace_symbols("calc").await;
    assert!(
        symbols.len() >= 2,
        "expected at least 2 symbols matching 'calc', got {}",
        symbols.len()
    );
}

#[tokio::test]
async fn workspace_symbol_empty_query_returns_all() {
    let harness = TestHarness::new().await;
    let content = r#"proc foo
    nop
end

proc bar
    nop
end
"#;
    let _uri = harness.open_inline("all.masm", content).await;

    let symbols = harness.workspace_symbols("").await;
    // Should return all symbols (at least foo and bar)
    assert!(
        symbols.len() >= 2,
        "expected at least 2 symbols, got {}",
        symbols.len()
    );
}

#[tokio::test]
async fn references_do_not_cross_modules_with_same_name() {
    let harness = TestHarness::new().await;

    let text_a = r#"proc foo
    push.1
end

proc call_a
    exec.foo
end
"#;

    let text_b = r#"proc foo
    push.2
end

proc call_b
    exec.foo
end
"#;

    let uri_a = harness.open_inline("foo_a.masm", text_a).await;
    let uri_b = harness.open_inline("foo_b.masm", text_b).await;

    tokio::task::yield_now().await;

    // Find references to foo from module A - should only include module A
    let pos_a = find_position(text_a, "foo");
    let refs_a = harness.find_references(&uri_a, pos_a, true).await;

    assert!(
        refs_a.iter().all(|loc| loc.uri == uri_a),
        "references from module A should not include module B"
    );
    assert_eq!(
        refs_a.len(),
        2,
        "expected exactly 2 references in module A (definition + 1 call), got {}",
        refs_a.len()
    );

    // Find references to foo from module B - should only include module B
    let pos_b = find_position(text_b, "foo");
    let refs_b = harness.find_references(&uri_b, pos_b, true).await;

    assert!(
        refs_b.iter().all(|loc| loc.uri == uri_b),
        "references from module B should not include module A"
    );
    assert_eq!(
        refs_b.len(),
        2,
        "expected exactly 2 references in module B (definition + 1 call), got {}",
        refs_b.len()
    );
}

#[tokio::test]
async fn goto_definition_resolves_to_local_proc_not_same_named_in_other_module() {
    let harness = TestHarness::new().await;

    // Module A has foo that pushes 1
    let text_a = r#"proc foo
    push.1
end

proc caller_a
    exec.foo
end
"#;

    // Module B has foo that pushes 2
    let text_b = r#"proc foo
    push.2
end

proc caller_b
    exec.foo
end
"#;

    let uri_a = harness.open_inline("mod_a.masm", text_a).await;
    let uri_b = harness.open_inline("mod_b.masm", text_b).await;

    tokio::task::yield_now().await;

    // Goto definition from exec.foo in module A should go to module A's foo
    let pos_call_a = find_position(text_a, "exec.foo");
    let def_a = harness.goto_definition(&uri_a, pos_call_a).await;

    assert!(def_a.is_some(), "expected to find definition from module A");
    let loc_a = def_a.unwrap();
    assert_eq!(
        loc_a.uri, uri_a,
        "definition should be in module A, not module B"
    );
    assert_eq!(loc_a.range.start.line, 0, "definition should be on line 0");

    // Goto definition from exec.foo in module B should go to module B's foo
    let pos_call_b = find_position(text_b, "exec.foo");
    let def_b = harness.goto_definition(&uri_b, pos_call_b).await;

    assert!(def_b.is_some(), "expected to find definition from module B");
    let loc_b = def_b.unwrap();
    assert_eq!(
        loc_b.uri, uri_b,
        "definition should be in module B, not module A"
    );
    assert_eq!(loc_b.range.start.line, 0, "definition should be on line 0");
}

#[tokio::test]
async fn qualified_cross_module_reference_resolves_correctly() {
    let harness = TestHarness::new().await;

    // Definition module
    let def_text = r#"proc helper
    push.42
end
"#;

    // Caller module uses fully qualified path
    let caller_text = r#"proc main
    exec.::defs::helper
end
"#;

    let def_uri = harness.open_inline("defs.masm", def_text).await;
    let caller_uri = harness.open_inline("caller.masm", caller_text).await;

    tokio::task::yield_now().await;

    // Goto definition from qualified call should find the definition in other module
    let pos = find_position(caller_text, "helper");
    let def = harness.goto_definition(&caller_uri, pos).await;

    assert!(
        def.is_some(),
        "expected to find definition via qualified path"
    );
    let loc = def.unwrap();
    assert_eq!(
        loc.uri, def_uri,
        "qualified path should resolve to definition module"
    );

    // Find references from definition should include the qualified call
    let def_pos = find_position(def_text, "helper");
    let refs = harness.find_references(&def_uri, def_pos, true).await;

    assert!(
        refs.len() >= 2,
        "expected at least 2 references (def + qualified call), got {}",
        refs.len()
    );
    assert!(
        refs.iter().any(|r| r.uri == def_uri),
        "references should include definition"
    );
    assert!(
        refs.iter().any(|r| r.uri == caller_uri),
        "references should include qualified call site"
    );
}

#[tokio::test]
async fn goto_definition_on_use_statement_navigates_to_module() {
    let harness = TestHarness::new().await;

    // Create a module that will be imported
    // The module path for "utils.masm" will be ::utils (fallback uses file stem)
    let lib_text = r#"proc helper
    push.42
end
"#;

    // Create a module that references the library module path in a use statement.
    // The use statement references the actual module path that gets indexed.
    let caller_text = r#"use ::utils

proc main
    exec.helper
end
"#;

    // Open the library module (path will be ::utils based on file stem)
    let lib_uri = harness.open_inline("utils.masm", lib_text).await;
    let caller_uri = harness.open_inline("caller.masm", caller_text).await;

    tokio::task::yield_now().await;

    // Position cursor on "::utils" in the use statement
    let pos = find_position(caller_text, "::utils");
    let def = harness.goto_definition(&caller_uri, pos).await;

    assert!(
        def.is_some(),
        "expected to find module definition from use statement"
    );
    let loc = def.unwrap();
    assert_eq!(
        loc.uri, lib_uri,
        "use statement should navigate to the module file"
    );
}
