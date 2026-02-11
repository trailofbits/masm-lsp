//! Configuration extraction from LSP settings.
//!
//! This module handles parsing configuration values from JSON settings
//! received via `didChangeConfiguration` notifications.

use crate::{InlayHintType, LibraryPath};

/// Extract the code lens stack effect toggle from LSP settings.
///
/// Expects settings in the format:
/// ```json
/// { "masm-lsp": { "codeLens": { "stackEffects": true } } }
/// ```
pub fn extract_code_lens_stack_effects(settings: &serde_json::Value) -> Option<bool> {
    settings
        .get("masm-lsp")
        .or_else(|| settings.get("masm"))
        .and_then(|v| v.get("codeLens"))
        .and_then(|v| v.get("stackEffects"))
        .and_then(|v| v.as_bool())
}

/// Extract the inlay hint tab padding count from LSP settings.
///
/// Expects settings in the format:
/// ```json
/// { "masm": { "inlayHint": { "tabPadding": 2 } } }
/// ```
pub fn extract_tab_count(settings: &serde_json::Value) -> Option<usize> {
    settings
        .get("masm")
        .and_then(|v| v.get("inlayHint"))
        .and_then(|v| v.get("tabPadding"))
        .and_then(|v| v.as_u64())
        .and_then(|v| usize::try_from(v).ok())
        .filter(|v| *v > 0)
}

/// Extract library paths from LSP settings.
///
/// Supports two formats:
/// 1. Simple string array: `["path/to/lib"]` (uses "std" prefix)
/// 2. Object array: `[{ "path": "path/to/lib", "prefix": "mylib" }]`
///
/// Expects settings in the format:
/// ```json
/// { "masm": { "libraryPaths": [...] } }
/// ```
pub fn extract_library_paths(settings: &serde_json::Value) -> Option<Vec<LibraryPath>> {
    let arr = settings
        .get("masm")
        .and_then(|v| v.get("libraryPaths"))
        .and_then(|v| v.as_array())?;

    let mut out = Vec::new();
    for entry in arr {
        match entry {
            serde_json::Value::String(path) => {
                out.push(LibraryPath {
                    root: path.into(),
                    prefix: "std".to_string(),
                });
            }
            serde_json::Value::Object(map) => {
                let Some(path_val) = map.get("path").and_then(|v| v.as_str()) else {
                    continue;
                };
                let prefix = map.get("prefix").and_then(|v| v.as_str()).unwrap_or("std");
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
/// { "masm": { "inlayHints": { "type": "decompilation" } } }
/// ```
///
/// Valid values for `type`:
/// - `"decompilation"` - Show decompiled pseudocode (default)
/// - `"description"` - Show instruction descriptions
/// - `"stack_effect"` - Show instruction stack effects
/// - `"none"` - Disable inlay hints
pub fn extract_inlay_hint_type(settings: &serde_json::Value) -> Option<InlayHintType> {
    let hint_type = settings
        .get("masm")
        .and_then(|v| v.get("inlayHints"))
        .and_then(|v| v.get("type"))
        .and_then(|v| v.as_str())?;

    match hint_type.to_lowercase().as_str() {
        "decompilation" => Some(InlayHintType::Decompilation),
        "description" => Some(InlayHintType::Description),
        "stack_effect" => Some(InlayHintType::StackEffect),
        "none" | "disabled" | "off" => Some(InlayHintType::None),
        _ => None,
    }
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
    fn extract_tab_count_valid() {
        let settings = json!({
            "masm": {
                "inlayHint": {
                    "tabPadding": 4
                }
            }
        });
        assert_eq!(extract_tab_count(&settings), Some(4));
    }

    #[test]
    fn extract_tab_count_zero_returns_none() {
        let settings = json!({
            "masm": {
                "inlayHint": {
                    "tabPadding": 0
                }
            }
        });
        assert_eq!(extract_tab_count(&settings), None);
    }

    #[test]
    fn extract_tab_count_missing_returns_none() {
        let settings = json!({});
        assert_eq!(extract_tab_count(&settings), None);
    }

    #[test]
    fn extract_library_paths_string_array() {
        let settings = json!({
            "masm": {
                "libraryPaths": ["/path/to/lib"]
            }
        });
        let paths = extract_library_paths(&settings).unwrap();
        assert_eq!(paths.len(), 1);
        assert_eq!(paths[0].root.to_str().unwrap(), "/path/to/lib");
        assert_eq!(paths[0].prefix, "std");
    }

    #[test]
    fn extract_library_paths_object_array() {
        let settings = json!({
            "masm": {
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
            "masm": {
                "libraryPaths": [
                    "/simple/path",
                    { "path": "/object/path", "prefix": "custom" }
                ]
            }
        });
        let paths = extract_library_paths(&settings).unwrap();
        assert_eq!(paths.len(), 2);
        assert_eq!(paths[0].prefix, "std");
        assert_eq!(paths[1].prefix, "custom");
    }

    #[test]
    fn extract_library_paths_empty_returns_none() {
        let settings = json!({
            "masm": {
                "libraryPaths": []
            }
        });
        assert!(extract_library_paths(&settings).is_none());
    }

    #[test]
    fn extract_inlay_hint_type_decompilation() {
        let settings = json!({
            "masm": {
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
            "masm": {
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
    fn extract_inlay_hint_type_stack_effect() {
        let settings = json!({
            "masm": {
                "inlayHints": {
                    "type": "stack_effect"
                }
            }
        });
        assert_eq!(
            extract_inlay_hint_type(&settings),
            Some(InlayHintType::StackEffect)
        );
    }

    #[test]
    fn extract_inlay_hint_type_none() {
        let settings = json!({
            "masm": {
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
            "masm": {
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
            "masm": {
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
            "masm": {
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
            "masm": {
                "inlayHints": {}
            }
        });
        assert_eq!(extract_inlay_hint_type(&settings), None);
    }
}
