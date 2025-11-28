//! Contract storage for workspace-wide procedure contracts.
//!
//! This module provides the `ContractStore` type for storing and querying
//! procedure contracts across an entire workspace.

use std::collections::HashMap;

use crate::symbol_path::SymbolPath;

use super::types::ProcContract;

// ═══════════════════════════════════════════════════════════════════════════
// Contract Store
// ═══════════════════════════════════════════════════════════════════════════

/// Storage for procedure contracts across the workspace.
#[derive(Clone, Debug, Default)]
pub struct ContractStore {
    /// Inferred contracts by procedure path
    contracts: HashMap<SymbolPath, ProcContract>,
    /// Index by short name for fast lookups
    by_name: HashMap<String, Vec<SymbolPath>>,
}

impl ContractStore {
    pub fn new() -> Self {
        Self::default()
    }

    /// Update contracts for a document.
    pub fn update_document(&mut self, contracts: Vec<ProcContract>) {
        for contract in contracts {
            let path = contract.path.clone();
            let name = path.name().to_string();

            // Update name index - remove old entry first
            self.by_name
                .entry(name.clone())
                .or_default()
                .retain(|p| p != &path);
            self.by_name
                .entry(name)
                .or_default()
                .push(path.clone());

            self.contracts.insert(path, contract);
        }
    }

    /// Remove contracts for a document.
    pub fn remove_document(&mut self, paths: &[SymbolPath]) {
        for path in paths {
            self.contracts.remove(path);
            let name = path.name().to_string();
            if let Some(entries) = self.by_name.get_mut(&name) {
                entries.retain(|p| p != path);
            }
        }
    }

    /// Get contract by exact path.
    pub fn get(&self, path: &SymbolPath) -> Option<&ProcContract> {
        self.contracts.get(path)
    }

    /// Get contract by short name (last segment).
    pub fn get_by_name(&self, name: &str) -> Option<&ProcContract> {
        self.by_name
            .get(name)?
            .first()
            .and_then(|path| self.contracts.get(path))
    }

    /// Get contract by path suffix.
    pub fn get_by_suffix(&self, suffix: &str) -> Option<&ProcContract> {
        let name = suffix.rsplit("::").next()?;
        self.by_name
            .get(name)?
            .iter()
            .find(|p| p.ends_with(suffix))
            .and_then(|path| self.contracts.get(path))
    }

    /// Get contract by exact path string (e.g., "module::proc_name").
    ///
    /// This is more precise than `get_by_suffix` as it requires the full path to match,
    /// avoiding ambiguity when multiple modules have procedures with the same suffix.
    pub fn get_by_path(&self, path_str: &str) -> Option<&ProcContract> {
        let name = path_str.rsplit("::").next()?;
        self.by_name
            .get(name)?
            .iter()
            .find(|p| p.as_ref() == path_str)
            .and_then(|path| self.contracts.get(path))
    }

    /// Get all contracts (for iteration).
    pub fn iter(&self) -> impl Iterator<Item = (&SymbolPath, &ProcContract)> {
        self.contracts.iter()
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Tests
// ═══════════════════════════════════════════════════════════════════════════

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::contracts::types::{StackEffect, ValidationBehavior};

    #[test]
    fn test_contract_store_basic() {
        let mut store = ContractStore::new();
        let path = SymbolPath::new("::test::my_proc");
        let contract = ProcContract {
            path: path.clone(),
            validates: ValidationBehavior::ValidatesU32,
            uses_u32_ops: true,
            reads_advice: false,
            uses_merkle_ops: false,
            stack_effect: StackEffect::Unknown,
            definition_range: None,
        };

        store.update_document(vec![contract]);

        assert!(store.get(&path).is_some());
        assert!(store.get_by_name("my_proc").is_some());
    }

    #[test]
    fn test_contract_store_by_suffix() {
        let mut store = ContractStore::new();
        let path = SymbolPath::new("::std::math::u64::add");
        let contract = ProcContract {
            path: path.clone(),
            validates: ValidationBehavior::None,
            uses_u32_ops: true,
            reads_advice: false,
            uses_merkle_ops: false,
            stack_effect: StackEffect::Unknown,
            definition_range: None,
        };

        store.update_document(vec![contract]);

        assert!(store.get_by_suffix("u64::add").is_some());
        assert!(store.get_by_suffix("add").is_some());
        assert!(store.get_by_suffix("math::u64::add").is_some());
    }
}
