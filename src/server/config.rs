//! Configuration extraction from LSP settings.
//!
//! This module handles parsing configuration values from JSON settings
//! received via `didChangeConfiguration` notifications.

use crate::core_lib::default_core_library_path;
use crate::{DecompilationConfig, InlayHintType, LibraryPath};

/// Extract the code lens stack effect toggle from LSP settings.
///
/// Expects settings in the format:
/// ```json
/// { "masm-lsp": { "codeLens": { "stackEffects": true } } }
/// ```
pub fn extract_code_lens_stack_effects(settings: &serde_json::Value) -> Option<bool> {
    settings
        .get("masm-lsp")
        .and_then(|v| v.get("codeLens"))
        .and_then(|v| v.get("stackEffects"))
        .and_then(|v| v.as_bool())
}

/// Extract library paths from LSP settings.
///
/// Supports two formats:
/// 1. Simple string array: `["path/to/lib"]` (uses the default core-library prefix)
/// 2. Object array: `[{ "path": "path/to/lib", "prefix": "mylib" }]`
///
/// Expects settings in the format:
/// ```json
/// { "masm-lsp": { "libraryPaths": [...] } }
/// ```
pub fn extract_library_paths(settings: &serde_json::Value) -> Option<Vec<LibraryPath>> {
    let arr = settings
        .get("masm-lsp")
        .and_then(|v| v.get("libraryPaths"))
        .and_then(|v| v.as_array())?;

    let mut out = Vec::new();
    for entry in arr {
        match entry {
            serde_json::Value::String(path) => {
                out.push(default_core_library_path(path));
            }
            serde_json::Value::Object(map) => {
                let Some(path_val) = map.get("path").and_then(|v| v.as_str()) else {
                    continue;
                };
                let prefix = map
                    .get("prefix")
                    .and_then(|v| v.as_str())
                    .unwrap_or(crate::core_lib::DEFAULT_CORE_LIBRARY_PREFIX);
                out.push(LibraryPath {
                    root: path_val.into(),
                    prefix: prefix.to_string(),
                });
            }
            _ => {}
        }
    }

    if out.is_empty() {
        None
    } else {
        Some(out)
    }
}

/// Extract the inlay hint type from LSP settings.
///
/// Expects settings in the format:
/// ```json
/// { "masm-lsp": { "inlayHints": { "type": "decompilation" } } }
/// ```
///
/// Valid values for `type`:
/// - `"decompilation"` - Show decompiled pseudocode (default)
/// - `"description"` - Show instruction descriptions
/// - `"none"` - Disable inlay hints
pub fn extract_inlay_hint_type(settings: &serde_json::Value) -> Option<InlayHintType> {
    let hint_type = settings
        .get("masm-lsp")
        .and_then(|v| v.get("inlayHints"))
        .and_then(|v| v.get("type"))
        .and_then(|v| v.as_str())?;

    match hint_type.to_lowercase().as_str() {
        "decompilation" => Some(InlayHintType::Decompilation),
        "description" => Some(InlayHintType::Description),
        "none" | "disabled" | "off" => Some(InlayHintType::None),
        _ => None,
    }
}

/// Extract the decompilation optimization configuration from LSP settings.
///
/// Expects settings in the format:
/// ```json
/// { "masm-lsp": { "decompilation": { "expressionPropagation": true, "deadCodeElimination": false, "simplification": true } } }
/// ```
///
/// Individual flags that are absent or non-boolean are left at their default (`true`).
/// Returns `None` when the `"decompilation"` key is missing entirely, signalling
/// that the caller should keep its current configuration.
pub fn extract_decompilation_config(settings: &serde_json::Value) -> Option<DecompilationConfig> {
    let section = settings
        .get("masm-lsp")
        .and_then(|v| v.get("decompilation"))?;

    let mut config = DecompilationConfig::default();

    if let Some(v) = section.get("expressionPropagation").and_then(|v| v.as_bool()) {
        config.expression_propagation = v;
    }
    if let Some(v) = section
        .get("deadCodeElimination")
        .and_then(|v| v.as_bool())
    {
        config.dead_code_elimination = v;
    }
    if let Some(v) = section.get("simplification").and_then(|v| v.as_bool()) {
        config.simplification = v;
    }

    Some(config)
}

/// Format a comment header describing which decompiler optimizations are enabled.
pub fn format_decompilation_config_header(config: &DecompilationConfig) -> String {
    fn status(enabled: bool) -> &'static str {
        if enabled {
            "enabled"
        } else {
            "disabled"
        }
    }

    format!(
        "# expression propagation: {}\n# dead-code elimination: {}\n# simplification: {}",
        status(config.expression_propagation),
        status(config.dead_code_elimination),
        status(config.simplification),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::InlayHintType;
    use serde_json::json;

    #[test]
    fn extract_code_lens_stack_effects_true() {
        let settings = json!({
            "masm-lsp": {
                "codeLens": {
                    "stackEffects": true
                }
            }
        });
        assert_eq!(extract_code_lens_stack_effects(&settings), Some(true));
    }

    #[test]
    fn extract_code_lens_stack_effects_false() {
        let settings = json!({
            "masm-lsp": {
                "codeLens": {
                    "stackEffects": false
                }
            }
        });
        assert_eq!(extract_code_lens_stack_effects(&settings), Some(false));
    }

    #[test]
    fn extract_library_paths_string_array() {
        let settings = json!({
            "masm-lsp": {
                "libraryPaths": ["/path/to/lib"]
            }
        });
        let paths = extract_library_paths(&settings).unwrap();
        assert_eq!(paths.len(), 1);
        assert_eq!(paths[0].root.to_str().unwrap(), "/path/to/lib");
        assert_eq!(
            paths[0].prefix,
            crate::core_lib::DEFAULT_CORE_LIBRARY_PREFIX
        );
    }

    #[test]
    fn extract_library_paths_object_array() {
        let settings = json!({
            "masm-lsp": {
                "libraryPaths": [
                    { "path": "/path/to/lib", "prefix": "mylib" }
                ]
            }
        });
        let paths = extract_library_paths(&settings).unwrap();
        assert_eq!(paths.len(), 1);
        assert_eq!(paths[0].root.to_str().unwrap(), "/path/to/lib");
        assert_eq!(paths[0].prefix, "mylib");
    }

    #[test]
    fn extract_library_paths_mixed() {
        let settings = json!({
            "masm-lsp": {
                "libraryPaths": [
                    "/simple/path",
                    { "path": "/object/path", "prefix": "custom" }
                ]
            }
        });
        let paths = extract_library_paths(&settings).unwrap();
        assert_eq!(paths.len(), 2);
        assert_eq!(
            paths[0].prefix,
            crate::core_lib::DEFAULT_CORE_LIBRARY_PREFIX
        );
        assert_eq!(paths[1].prefix, "custom");
    }

    #[test]
    fn extract_library_paths_empty_returns_none() {
        let settings = json!({
            "masm-lsp": {
                "libraryPaths": []
            }
        });
        assert!(extract_library_paths(&settings).is_none());
    }

    #[test]
    fn extract_inlay_hint_type_decompilation() {
        let settings = json!({
            "masm-lsp": {
                "inlayHints": {
                    "type": "decompilation"
                }
            }
        });
        assert_eq!(
            extract_inlay_hint_type(&settings),
            Some(InlayHintType::Decompilation)
        );
    }

    #[test]
    fn extract_inlay_hint_type_description() {
        let settings = json!({
            "masm-lsp": {
                "inlayHints": {
                    "type": "description"
                }
            }
        });
        assert_eq!(
            extract_inlay_hint_type(&settings),
            Some(InlayHintType::Description)
        );
    }

    #[test]
    fn extract_inlay_hint_type_none() {
        let settings = json!({
            "masm-lsp": {
                "inlayHints": {
                    "type": "none"
                }
            }
        });
        assert_eq!(
            extract_inlay_hint_type(&settings),
            Some(InlayHintType::None)
        );
    }

    #[test]
    fn extract_inlay_hint_type_disabled_alias() {
        let settings = json!({
            "masm-lsp": {
                "inlayHints": {
                    "type": "disabled"
                }
            }
        });
        assert_eq!(
            extract_inlay_hint_type(&settings),
            Some(InlayHintType::None)
        );
    }

    #[test]
    fn extract_inlay_hint_type_case_insensitive() {
        let settings = json!({
            "masm-lsp": {
                "inlayHints": {
                    "type": "DECOMPILATION"
                }
            }
        });
        assert_eq!(
            extract_inlay_hint_type(&settings),
            Some(InlayHintType::Decompilation)
        );
    }

    #[test]
    fn extract_inlay_hint_type_missing_returns_none() {
        let settings = json!({});
        assert_eq!(extract_inlay_hint_type(&settings), None);
    }

    #[test]
    fn extract_inlay_hint_type_invalid_value_returns_none() {
        let settings = json!({
            "masm-lsp": {
                "inlayHints": {
                    "type": "invalid"
                }
            }
        });
        assert_eq!(extract_inlay_hint_type(&settings), None);
    }

    #[test]
    fn extract_inlay_hint_type_empty_hints_returns_none() {
        let settings = json!({
            "masm-lsp": {
                "inlayHints": {}
            }
        });
        assert_eq!(extract_inlay_hint_type(&settings), None);
    }

    #[test]
    fn extract_decompilation_config_all_flags() {
        let settings = json!({
            "masm-lsp": {
                "decompilation": {
                    "expressionPropagation": false,
                    "deadCodeElimination": false,
                    "simplification": false
                }
            }
        });
        let config = extract_decompilation_config(&settings).unwrap();
        assert!(!config.expression_propagation);
        assert!(!config.dead_code_elimination);
        assert!(!config.simplification);
    }

    #[test]
    fn extract_decompilation_config_partial_flags() {
        let settings = json!({
            "masm-lsp": {
                "decompilation": {
                    "deadCodeElimination": false
                }
            }
        });
        let config = extract_decompilation_config(&settings).unwrap();
        assert!(config.expression_propagation);
        assert!(!config.dead_code_elimination);
        assert!(config.simplification);
    }

    #[test]
    fn extract_decompilation_config_empty_section_returns_defaults() {
        let settings = json!({
            "masm-lsp": {
                "decompilation": {}
            }
        });
        let config = extract_decompilation_config(&settings).unwrap();
        assert!(config.expression_propagation);
        assert!(config.dead_code_elimination);
        assert!(config.simplification);
    }

    #[test]
    fn extract_decompilation_config_missing_returns_none() {
        let settings = json!({});
        assert!(extract_decompilation_config(&settings).is_none());
    }

    #[test]
    fn extract_decompilation_config_ignores_non_bool_values() {
        let settings = json!({
            "masm-lsp": {
                "decompilation": {
                    "expressionPropagation": "yes",
                    "deadCodeElimination": 1
                }
            }
        });
        let config = extract_decompilation_config(&settings).unwrap();
        assert!(config.expression_propagation);
        assert!(config.dead_code_elimination);
    }

    #[test]
    fn format_header_all_enabled() {
        let config = DecompilationConfig::default();
        let header = format_decompilation_config_header(&config);
        assert_eq!(
            header,
            "# expression propagation: enabled\n\
             # dead-code elimination: enabled\n\
             # simplification: enabled"
        );
    }

    #[test]
    fn format_header_mixed() {
        let config = DecompilationConfig::default()
            .with_expression_propagation(false)
            .with_dead_code_elimination(true)
            .with_simplification(false);
        let header = format_decompilation_config_header(&config);
        assert_eq!(
            header,
            "# expression propagation: disabled\n\
             # dead-code elimination: enabled\n\
             # simplification: disabled"
        );
    }
}
