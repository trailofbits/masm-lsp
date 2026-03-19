//! DMASM LSP services — rename, references, hover, document symbols.

use tower_lsp::lsp_types::{
    DocumentSymbol, Hover, HoverContents, Location, MarkupContent, MarkupKind, Position, Range,
    SymbolKind, TextEdit, Url, WorkspaceEdit,
};

use std::collections::HashMap;

use super::parser::{DmasmDocument, DmasmProcedure, VariableOccurrence};

/// Prepare a rename at the given position.
///
/// Returns the range of the variable token at the cursor, or `None` if the
/// cursor is not on a renameable variable.
pub fn prepare_rename(doc: &DmasmDocument, position: Position) -> Option<Range> {
    let proc = find_procedure_at(doc, position)?;
    let var = find_variable_at(proc, position)?;
    Some(var.range)
}

/// Compute a rename edit for the variable at the given position.
///
/// All occurrences of the variable within the same procedure are renamed.
/// Returns `None` if the cursor is not on a variable.
pub fn rename(
    doc: &DmasmDocument,
    uri: &Url,
    position: Position,
    new_name: &str,
) -> Option<WorkspaceEdit> {
    let proc = find_procedure_at(doc, position)?;
    let var = find_variable_at(proc, position)?;
    let old_name = &var.name;

    let edits: Vec<TextEdit> = proc
        .variables
        .iter()
        .filter(|v| v.name == *old_name)
        .map(|v| TextEdit {
            range: v.range,
            new_text: new_name.to_string(),
        })
        .collect();

    if edits.is_empty() {
        return None;
    }

    let mut changes = HashMap::new();
    changes.insert(uri.clone(), edits);
    Some(WorkspaceEdit {
        changes: Some(changes),
        ..Default::default()
    })
}

/// Find all references to the variable at the given position within the same procedure.
pub fn references(doc: &DmasmDocument, uri: &Url, position: Position) -> Option<Vec<Location>> {
    let proc = find_procedure_at(doc, position)?;
    let var = find_variable_at(proc, position)?;
    let old_name = &var.name;

    let locations: Vec<Location> = proc
        .variables
        .iter()
        .filter(|v| v.name == *old_name)
        .map(|v| Location {
            uri: uri.clone(),
            range: v.range,
        })
        .collect();

    if locations.is_empty() {
        None
    } else {
        Some(locations)
    }
}

/// Provide hover information for a variable at the given position.
///
/// Shows the variable type if it is a parameter with a declared type.
pub fn hover(doc: &DmasmDocument, position: Position) -> Option<Hover> {
    let proc = find_procedure_at(doc, position)?;
    let var = find_variable_at(proc, position)?;

    // Look up the variable in the procedure's parameter list to find its type.
    let param = proc.parameters.iter().find(|p| p.name == var.name)?;
    let type_name = param.type_name.as_deref()?;

    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: format!("```dmasm\n{}: {type_name}\n```", var.name),
        }),
        range: Some(var.range),
    })
}

/// Build document symbols (one per procedure) for a DMASM file.
pub fn document_symbols(doc: &DmasmDocument) -> Vec<DocumentSymbol> {
    doc.procedures
        .iter()
        .map(|proc| {
            #[allow(deprecated)]
            DocumentSymbol {
                name: proc.name.clone(),
                detail: None,
                kind: SymbolKind::FUNCTION,
                tags: None,
                deprecated: None,
                range: proc.range,
                selection_range: proc.name_range,
                children: None,
            }
        })
        .collect()
}

/// Find the procedure that contains the given position.
fn find_procedure_at(
    doc: &DmasmDocument,
    position: Position,
) -> Option<&DmasmProcedure> {
    doc.procedures.iter().find(|proc| {
        position.line >= proc.range.start.line && position.line <= proc.range.end.line
    })
}

/// Find the variable occurrence at the given position within a procedure.
///
/// Uses exclusive end (standard LSP range convention).
fn find_variable_at(
    proc: &DmasmProcedure,
    position: Position,
) -> Option<&VariableOccurrence> {
    proc.variables.iter().find(|v| {
        v.range.start.line == position.line
            && position.character >= v.range.start.character
            && position.character < v.range.end.character
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fixture(name: &str) -> String {
        let path = format!("{}/tests/fixtures/{name}", env!("CARGO_MANIFEST_DIR"));
        std::fs::read_to_string(path).expect("fixture file should exist")
    }

    fn parse_fixture(name: &str) -> DmasmDocument {
        super::super::parser::parse_dmasm(&fixture(name))
    }

    #[test]
    fn prepare_rename_returns_range_when_on_variable() {
        let doc = parse_fixture("simple.dmasm");
        // v_0 in parameter list of add_and_square (line 2).
        // "proc add_and_square(" = 20 chars, so v_0 starts at column 20.
        let result = prepare_rename(&doc, Position::new(2, 20));
        assert!(result.is_some(), "should find v_0 in parameter list");
    }

    #[test]
    fn prepare_rename_returns_none_outside_variable() {
        let doc = parse_fixture("simple.dmasm");
        // Position on "proc" keyword.
        let result = prepare_rename(&doc, Position::new(2, 0));
        assert!(result.is_none(), "should not find variable on proc keyword");
    }

    #[test]
    fn rename_changes_all_occurrences_in_procedure() {
        let doc = parse_fixture("multi_var.dmasm");
        let uri = Url::parse("file:///test.dmasm").unwrap();
        // Rename v_0 in multi_use procedure.
        let proc = &doc.procedures[2];
        let v0_param = &proc.parameters[0];
        let pos = Position::new(v0_param.range.start.line, v0_param.range.start.character);

        let edit = rename(&doc, &uri, pos, "input").expect("rename should succeed");
        let edits = edit.changes.unwrap();
        let file_edits = &edits[&uri];

        // All v_0 occurrences should be renamed.
        let v0_count = proc.variables.iter().filter(|v| v.name == "v_0").count();
        assert_eq!(
            file_edits.len(),
            v0_count,
            "should rename all occurrences of v_0"
        );
        for edit in file_edits {
            assert_eq!(edit.new_text, "input");
        }
    }

    #[test]
    fn references_finds_all_occurrences() {
        let doc = parse_fixture("multi_var.dmasm");
        let uri = Url::parse("file:///test.dmasm").unwrap();
        let proc = &doc.procedures[2]; // multi_use
        let v0_param = &proc.parameters[0];
        let pos = Position::new(v0_param.range.start.line, v0_param.range.start.character);

        let refs = references(&doc, &uri, pos).expect("should find references");
        let v0_count = proc.variables.iter().filter(|v| v.name == "v_0").count();
        assert_eq!(refs.len(), v0_count);
    }

    #[test]
    fn hover_shows_type_for_parameter() {
        let doc = parse_fixture("simple.dmasm");
        // v_0 in add_and_square is Felt.
        let proc = &doc.procedures[0];
        let v0_param = &proc.parameters[0];
        let pos = Position::new(v0_param.range.start.line, v0_param.range.start.character);

        let hover = hover(&doc, pos).expect("hover should return info");
        match hover.contents {
            HoverContents::Markup(content) => {
                assert!(content.value.contains("Felt"));
                assert!(content.value.contains("v_0"));
            }
            _ => panic!("expected markup content"),
        }
    }

    #[test]
    fn hover_returns_none_for_non_parameter_variable() {
        let doc = parse_fixture("simple.dmasm");
        // v_2 in add_and_square is computed, not a parameter.
        let proc = &doc.procedures[0];
        let v2 = proc
            .variables
            .iter()
            .find(|v| v.name == "v_2")
            .expect("v_2 should exist");
        let pos = Position::new(v2.range.start.line, v2.range.start.character);

        assert!(
            hover(&doc, pos).is_none(),
            "non-parameter should have no hover"
        );
    }

    #[test]
    fn document_symbols_lists_all_procedures() {
        let doc = parse_fixture("simple.dmasm");
        let symbols = document_symbols(&doc);
        assert_eq!(symbols.len(), 2);
        assert_eq!(symbols[0].name, "add_and_square");
        assert_eq!(symbols[1].name, "check_bound");
        assert!(symbols.iter().all(|s| s.kind == SymbolKind::FUNCTION));
    }
}
