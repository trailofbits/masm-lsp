use std::collections::HashMap;

use super::ids::{SsaId, SsaValue, VarKind};

/// A phi node that merges multiple SSA values at a control flow merge point.
///
/// At display time, all values in a phi node use the same display name.
#[derive(Debug, Clone)]
pub struct PhiNode {
    /// The SSA ID that represents the merged value.
    pub result: SsaId,
    /// The SSA IDs of the values being merged.
    pub operands: Vec<SsaId>,
}

impl PhiNode {
    /// Create a new phi node.
    pub fn new(result: SsaId, operands: Vec<SsaId>) -> Self {
        Self { result, operands }
    }
}

/// Context for SSA-based decompilation.
///
/// Tracks all SSA values and phi nodes, and provides methods to resolve
/// which values should share the same display name.
#[derive(Debug)]
pub struct SsaContext {
    /// All SSA values created during analysis.
    pub values: HashMap<SsaId, SsaValue>,
    /// All phi nodes created at merge points.
    pub phi_nodes: Vec<PhiNode>,
    /// Counter for generating unique SSA IDs.
    pub next_id: usize,
    /// Counter for generating local variable indices.
    next_local_index: usize,
    /// Maps SSA IDs to their canonical display name (computed during resolution).
    resolved_names: HashMap<SsaId, String>,
}

impl SsaContext {
    /// Create a new SSA context.
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            phi_nodes: Vec::new(),
            next_id: 0,
            next_local_index: 0,
            resolved_names: HashMap::new(),
        }
    }

    /// Create a new SSA value for a procedure argument.
    pub fn new_argument(&mut self, index: usize) -> SsaId {
        let id = SsaId::new(self.next_id);
        self.next_id += 1;
        let value = SsaValue::argument(id, index);
        self.values.insert(id, value);
        id
    }

    /// Create a new SSA value for a local/intermediate variable.
    pub fn new_local(&mut self) -> SsaId {
        let id = SsaId::new(self.next_id);
        self.next_id += 1;
        let index = self.next_local_index;
        self.next_local_index += 1;
        let value = SsaValue::local(id, index);
        self.values.insert(id, value);
        id
    }

    /// Create a new SSA value for a return value.
    pub fn new_return(&mut self, index: usize) -> SsaId {
        let id = SsaId::new(self.next_id);
        self.next_id += 1;
        let value = SsaValue::return_val(id, index);
        self.values.insert(id, value);
        id
    }

    /// Add a phi node that merges multiple values.
    ///
    /// The `result` is the SSA ID for the merged value (typically a new local).
    /// The `operands` are the SSA IDs being merged.
    pub fn add_phi(&mut self, result: SsaId, operands: Vec<SsaId>) {
        self.phi_nodes.push(PhiNode::new(result, operands));
    }

    /// Reclassify an SSA value as a return and update its display name.
    pub fn set_return(&mut self, id: SsaId, index: usize) {
        if let Some(val) = self.values.get_mut(&id) {
            let was_argument = matches!(val.kind, VarKind::Argument(_));
            val.kind = VarKind::Return(index);
            // Preserve argument names for returned arguments; otherwise use r_i.
            if !was_argument {
                val.display_name = VarKind::Return(index).display_name();
            }
        }
    }

    /// Get the SSA value for an ID.
    pub fn get_value(&self, id: SsaId) -> Option<&SsaValue> {
        self.values.get(&id)
    }

    /// Resolve phi relationships and compute canonical display names.
    ///
    /// After calling this, `get_display_name` will return consistent names
    /// for all values that are phi-related.
    pub fn resolve_names(&mut self) {
        // Build equivalence classes using union-find
        let mut parent: HashMap<SsaId, SsaId> = HashMap::new();

        // Initialize each value as its own parent
        for &id in self.values.keys() {
            parent.insert(id, id);
        }

        // Find with path compression
        fn find(parent: &mut HashMap<SsaId, SsaId>, id: SsaId) -> SsaId {
            let p = *parent.get(&id).unwrap_or(&id);
            if p != id {
                let root = find(parent, p);
                parent.insert(id, root);
                root
            } else {
                id
            }
        }

        // Union two sets
        fn union(parent: &mut HashMap<SsaId, SsaId>, a: SsaId, b: SsaId) {
            let root_a = find(parent, a);
            let root_b = find(parent, b);
            if root_a != root_b {
                // Prefer the smaller ID as the root (earlier values are usually arguments)
                if root_a.0 < root_b.0 {
                    parent.insert(root_b, root_a);
                } else {
                    parent.insert(root_a, root_b);
                }
            }
        }

        // Process phi nodes to build equivalence classes
        for phi in &self.phi_nodes {
            // The result and all operands are in the same equivalence class
            for &operand in &phi.operands {
                union(&mut parent, phi.result, operand);
            }
        }

        // Resolve names: each equivalence class gets the name of its canonical member
        // The canonical member is the one with the smallest ID (typically an argument or
        // the first definition)
        for &id in self.values.keys() {
            let root = find(&mut parent, id);
            let canonical_name = self
                .values
                .get(&root)
                .map(|v| v.display_name.clone())
                .unwrap_or_else(|| format!("?_{}", id.0));
            self.resolved_names.insert(id, canonical_name);
        }
    }

    /// Get the display name for an SSA value.
    ///
    /// Call `resolve_names()` first to ensure phi relationships are resolved.
    pub fn get_display_name(&self, id: SsaId) -> &str {
        self.resolved_names
            .get(&id)
            .map(|s| s.as_str())
            .or_else(|| self.values.get(&id).map(|v| v.display_name.as_str()))
            .unwrap_or("?")
    }

    /// Get the number of values created (for estimating next argument index).
    pub fn value_count(&self) -> usize {
        self.values.len()
    }

    /// Count how many arguments have been discovered.
    pub fn argument_count(&self) -> usize {
        self.values
            .values()
            .filter(|v| matches!(v.kind, VarKind::Argument(_)))
            .count()
    }

    /// Current next local index (useful for detecting locals created inside loops).
    pub fn next_local_index(&self) -> usize {
        self.next_local_index
    }

    /// Collect display names for return values ordered by return index.
    ///
    /// `resolve_names` should be called before invoking this to ensure
    /// phi-related names are stable.
    pub fn return_names(&self) -> Vec<String> {
        let mut pairs: Vec<(usize, String)> = self
            .values
            .iter()
            .filter_map(|(_, v)| match v.kind {
                VarKind::Return(idx) => Some((idx, self.get_display_name(v.id).to_string())),
                _ => None,
            })
            .collect();
        pairs.sort_by_key(|(idx, _)| *idx);
        pairs.into_iter().map(|(_, name)| name).collect()
    }

    /// Find the smallest argument index in the phi-equivalence class of `id`.
    pub fn argument_index_via_phi(&self, id: SsaId) -> Option<usize> {
        let mut parent: HashMap<SsaId, SsaId> = HashMap::new();
        for &val in self.values.keys() {
            parent.insert(val, val);
        }

        fn find(parent: &mut HashMap<SsaId, SsaId>, id: SsaId) -> SsaId {
            let p = *parent.get(&id).unwrap_or(&id);
            if p != id {
                let root = find(parent, p);
                parent.insert(id, root);
                root
            } else {
                id
            }
        }

        fn union(parent: &mut HashMap<SsaId, SsaId>, a: SsaId, b: SsaId) {
            let root_a = find(parent, a);
            let root_b = find(parent, b);
            if root_a != root_b {
                if root_a.0 < root_b.0 {
                    parent.insert(root_b, root_a);
                } else {
                    parent.insert(root_a, root_b);
                }
            }
        }

        for phi in &self.phi_nodes {
            for &op in &phi.operands {
                union(&mut parent, phi.result, op);
            }
        }

        let mut root_arg: HashMap<SsaId, usize> = HashMap::new();
        let mut parent_copy = parent.clone();
        for (&sid, val) in self.values.iter() {
            if let VarKind::Argument(idx) = val.kind {
                let root = find(&mut parent_copy, sid);
                root_arg
                    .entry(root)
                    .and_modify(|e| *e = (*e).min(idx))
                    .or_insert(idx);
            }
        }

        let root = {
            let mut tmp = parent.clone();
            find(&mut tmp, id)
        };
        root_arg.get(&root).copied()
    }

    /// Check if two SSA values should have the same display name.
    pub fn same_variable(&self, a: SsaId, b: SsaId) -> bool {
        self.get_display_name(a) == self.get_display_name(b)
    }
}

impl Default for SsaContext {
    fn default() -> Self {
        Self::new()
    }
}
