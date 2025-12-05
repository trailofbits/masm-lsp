use std::fmt;

use miden_assembly_syntax::ast::Module;

/// A typed wrapper for fully-qualified symbol paths.
///
/// Symbol paths follow the format `::module::submodule::name` and represent
/// the fully-qualified identifier for a procedure, constant, or module.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SymbolPath(String);

impl SymbolPath {
    /// Create a new SymbolPath from a string.
    pub fn new(path: impl Into<String>) -> Self {
        Self(path.into())
    }

    /// Build a fully-qualified path for an item in a module.
    pub fn from_module_and_name(module: &Module, name: &str) -> Self {
        let mut buf = module.path().to_path_buf();
        buf.push(name);
        Self(buf.to_string())
    }

    /// Get the path as a string slice.
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Get the last segment of the path (the symbol name).
    ///
    /// For path `::std::crypto::sha256::hash`, returns `"hash"`.
    pub fn name(&self) -> &str {
        self.0.rsplit("::").next().unwrap_or(&self.0)
    }

    /// Get the module path (everything before the last segment).
    ///
    /// For path `::std::crypto::sha256::hash`, returns `Some("::std::crypto::sha256")`.
    pub fn module_path(&self) -> Option<&str> {
        self.0.rsplit_once("::").map(|(prefix, _)| prefix)
    }

    /// Iterate over path segments.
    ///
    /// For path `::std::crypto::sha256::hash`, yields `["std", "crypto", "sha256", "hash"]`.
    pub fn segments(&self) -> impl Iterator<Item = &str> {
        self.0
            .trim_start_matches("::")
            .split("::")
            .filter(|s| !s.is_empty())
    }

    /// Check if this path ends with the given suffix.
    pub fn ends_with(&self, suffix: &str) -> bool {
        self.0.ends_with(suffix)
    }

    /// Check if this path ends with another SymbolPath as a suffix.
    pub fn ends_with_path(&self, other: &SymbolPath) -> bool {
        self.0.ends_with(&other.0)
    }

    /// Check if the name (last segment) matches the given string.
    pub fn name_matches(&self, name: &str) -> bool {
        self.name() == name
    }

    /// Convert into the inner String.
    pub fn into_inner(self) -> String {
        self.0
    }
}

impl fmt::Display for SymbolPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<String> for SymbolPath {
    fn from(s: String) -> Self {
        Self(s)
    }
}

impl From<&str> for SymbolPath {
    fn from(s: &str) -> Self {
        Self(s.to_string())
    }
}

impl AsRef<str> for SymbolPath {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_name_extraction() {
        let path = SymbolPath::new("::std::crypto::sha256::hash");
        assert_eq!(path.name(), "hash");
    }

    #[test]
    fn test_module_path() {
        let path = SymbolPath::new("::std::crypto::sha256::hash");
        assert_eq!(path.module_path(), Some("::std::crypto::sha256"));
    }

    #[test]
    fn test_segments() {
        let path = SymbolPath::new("::std::crypto::sha256::hash");
        let segments: Vec<_> = path.segments().collect();
        assert_eq!(segments, vec!["std", "crypto", "sha256", "hash"]);
    }

    #[test]
    fn test_ends_with() {
        let path = SymbolPath::new("::std::crypto::sha256::hash");
        assert!(path.ends_with("hash"));
        assert!(path.ends_with("sha256::hash"));
        assert!(path.ends_with("::sha256::hash"));
        assert!(!path.ends_with("foo"));
    }

    #[test]
    fn test_name_matches() {
        let path = SymbolPath::new("::std::crypto::sha256::hash");
        assert!(path.name_matches("hash"));
        assert!(!path.name_matches("sha256"));
    }

    #[test]
    fn test_simple_path() {
        let path = SymbolPath::new("foo");
        assert_eq!(path.name(), "foo");
        assert_eq!(path.module_path(), None);
    }

    #[test]
    fn test_display() {
        let path = SymbolPath::new("::std::crypto::hash");
        assert_eq!(format!("{}", path), "::std::crypto::hash");
    }
}

#[cfg(test)]
mod proptests {
    use super::*;
    use proptest::prelude::*;

    // Strategy to generate valid path segments (lowercase alphanumeric + underscore)
    fn segment_strategy() -> impl Strategy<Value = String> {
        "[a-z][a-z0-9_]{0,15}".prop_map(|s| s.to_string())
    }

    // Strategy to generate multi-segment paths like "foo::bar::baz"
    fn path_segments_strategy() -> impl Strategy<Value = Vec<String>> {
        prop::collection::vec(segment_strategy(), 1..=5)
    }

    proptest! {
        /// The name() method should always return the last segment of the path.
        #[test]
        fn name_is_last_segment(segments in path_segments_strategy()) {
            let path_str = format!("::{}", segments.join("::"));
            let sp = SymbolPath::new(&path_str);
            let expected_name = segments.last().unwrap();
            prop_assert_eq!(sp.name(), expected_name.as_str());
        }

        /// Display should return the same string used to construct the path.
        #[test]
        fn roundtrip_display(segments in path_segments_strategy()) {
            let path_str = format!("::{}", segments.join("::"));
            let sp = SymbolPath::new(&path_str);
            prop_assert_eq!(sp.to_string(), path_str);
        }

        /// A path should always end with itself.
        #[test]
        fn ends_with_self(segments in path_segments_strategy()) {
            let path_str = format!("::{}", segments.join("::"));
            let sp = SymbolPath::new(&path_str);
            prop_assert!(sp.ends_with(&path_str));
        }

        /// A path should end with its name (last segment).
        #[test]
        fn ends_with_name(segments in path_segments_strategy()) {
            let path_str = format!("::{}", segments.join("::"));
            let sp = SymbolPath::new(&path_str);
            let name = segments.last().unwrap();
            prop_assert!(sp.ends_with(name));
        }

        /// The segments iterator should return all segments in order.
        #[test]
        fn segments_match_input(segments in path_segments_strategy()) {
            let path_str = format!("::{}", segments.join("::"));
            let sp = SymbolPath::new(&path_str);
            let collected: Vec<_> = sp.segments().map(String::from).collect();
            prop_assert_eq!(collected, segments);
        }

        /// module_path() should return everything except the last segment.
        #[test]
        fn module_path_excludes_name(segments in path_segments_strategy()) {
            prop_assume!(segments.len() > 1);
            let path_str = format!("::{}", segments.join("::"));
            let sp = SymbolPath::new(&path_str);

            let expected_module = format!("::{}", segments[..segments.len() - 1].join("::"));
            prop_assert_eq!(sp.module_path(), Some(expected_module.as_str()));
        }

        /// name_matches should return true for the exact name.
        #[test]
        fn name_matches_exact(segments in path_segments_strategy()) {
            let path_str = format!("::{}", segments.join("::"));
            let sp = SymbolPath::new(&path_str);
            let name = segments.last().unwrap();
            prop_assert!(sp.name_matches(name));
        }

        /// as_str and as_ref should return the same string.
        #[test]
        fn as_str_equals_as_ref(segments in path_segments_strategy()) {
            let path_str = format!("::{}", segments.join("::"));
            let sp = SymbolPath::new(&path_str);
            prop_assert_eq!(sp.as_str(), sp.as_ref());
        }
    }
}
