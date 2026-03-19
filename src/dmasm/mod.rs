//! Support for decompiled MASM (DMASM) files.

pub mod parser;
pub mod services;

use tower_lsp::lsp_types::Url;

/// Distinguishes between MASM source files and decompiled MASM (DMASM) files.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DocumentLanguage {
    /// Standard Miden Assembly source.
    Masm,
    /// Decompiled Miden Assembly pseudocode.
    Dmasm,
}

/// Detect the document language from its URI based on file extension.
///
/// Files ending in `.dmasm` are treated as decompiled output.
/// Everything else (including `.masm` and unknown extensions) is treated as MASM.
pub fn detect_language(uri: &Url) -> DocumentLanguage {
    let path = uri.path();
    if path.ends_with(".dmasm") {
        DocumentLanguage::Dmasm
    } else {
        DocumentLanguage::Masm
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn detect_masm_file() {
        let uri = Url::parse("file:///project/foo.masm").unwrap();
        assert_eq!(detect_language(&uri), DocumentLanguage::Masm);
    }

    #[test]
    fn detect_dmasm_file() {
        let uri = Url::parse("file:///project/foo.dmasm").unwrap();
        assert_eq!(detect_language(&uri), DocumentLanguage::Dmasm);
    }

    #[test]
    fn detect_unknown_extension_defaults_to_masm() {
        let uri = Url::parse("file:///project/foo.txt").unwrap();
        assert_eq!(detect_language(&uri), DocumentLanguage::Masm);
    }

    #[test]
    fn detect_untitled_dmasm() {
        let uri = Url::parse("untitled:///project/foo.dmasm").unwrap();
        assert_eq!(detect_language(&uri), DocumentLanguage::Dmasm);
    }
}
