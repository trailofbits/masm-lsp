//! Call graph construction and topological ordering for contract inference.
//!
//! This module provides infrastructure for analyzing procedure call dependencies
//! and computing an optimal analysis order. By processing procedures in topological
//! order (leaves first), we can analyze each procedure exactly once with full
//! knowledge of all its callees' contracts.
//!
//! # Algorithm
//!
//! Uses Tarjan's algorithm for strongly connected component (SCC) detection,
//! which naturally produces a reverse topological order. SCCs represent
//! mutually recursive procedures that must be handled specially.

use std::collections::{HashMap, HashSet};

use miden_assembly_syntax::ast::{
    visit::{self, Visit},
    Instruction, InvocationTarget, Module, Op, Procedure,
};

use crate::symbol_path::SymbolPath;
use crate::symbol_resolution;

// ═══════════════════════════════════════════════════════════════════════════════
// Call Graph
// ═══════════════════════════════════════════════════════════════════════════════

/// A call graph representing procedure dependencies within a module.
#[derive(Debug, Default)]
pub struct CallGraph {
    /// Adjacency list: procedure path -> procedures it calls (within this module)
    calls: HashMap<SymbolPath, Vec<SymbolPath>>,
    /// All procedure paths in the module
    procedures: Vec<SymbolPath>,
}

impl CallGraph {
    /// Build a call graph from a module's procedures.
    ///
    /// Only tracks calls to procedures defined within the same module.
    /// External calls (stdlib, other modules) are not included since
    /// their contracts are already known.
    pub fn from_module(module: &Module) -> Self {
        // Build set of local procedure paths for filtering
        let local_paths: HashSet<SymbolPath> = module
            .procedures()
            .map(|proc| SymbolPath::from_module_and_name(module, proc.name().as_str()))
            .collect();

        let procedures: Vec<SymbolPath> = local_paths.iter().cloned().collect();

        // Create resolver for this module
        let resolver = symbol_resolution::create_resolver(module);

        // Build call edges
        let mut calls: HashMap<SymbolPath, Vec<SymbolPath>> = HashMap::new();

        for proc in module.procedures() {
            let caller_path = SymbolPath::from_module_and_name(module, proc.name().as_str());
            let call_targets = CallCollector::collect_from_procedure(proc);

            // Resolve each call target and keep only those that resolve to local procedures
            let callees: Vec<SymbolPath> = call_targets
                .iter()
                .filter_map(|target| {
                    let resolved = resolver.resolve_target(target)?;
                    // Only include if it's a local procedure
                    if local_paths.contains(&resolved) {
                        Some(resolved)
                    } else {
                        None
                    }
                })
                .collect();

            calls.insert(caller_path, callees);
        }

        Self { calls, procedures }
    }

    /// Get the procedures called by a given procedure.
    pub fn callees(&self, proc: &SymbolPath) -> &[SymbolPath] {
        self.calls.get(proc).map(|v| v.as_slice()).unwrap_or(&[])
    }

    /// Get all procedures in the call graph.
    pub fn procedures(&self) -> &[SymbolPath] {
        &self.procedures
    }

    /// Compute topological order with SCC detection using Tarjan's algorithm.
    ///
    /// Returns procedures grouped into [`TopologicalNode`]s:
    /// - `Single`: A procedure with no cycles (can be analyzed independently)
    /// - `Cycle`: A set of mutually recursive procedures (form an SCC)
    ///
    /// The returned order is such that for any procedure P that calls Q,
    /// Q's node appears before P's node in the list (unless they're in the same SCC).
    pub fn topological_order(&self) -> Vec<TopologicalNode> {
        let mut state = TarjanState::new(&self.procedures);

        for proc in &self.procedures {
            if !state.visited.contains_key(proc) {
                self.tarjan_visit(proc, &mut state);
            }
        }

        // Tarjan produces SCCs in reverse topological order (sinks/leaves first),
        // which is exactly what we want for contract inference
        state.result
    }

    /// Tarjan's SCC algorithm - recursive visit
    fn tarjan_visit(&self, v: &SymbolPath, state: &mut TarjanState) {
        let index = state.index;
        state.indices.insert(v.clone(), index);
        state.low_links.insert(v.clone(), index);
        state.index += 1;
        state.stack.push(v.clone());
        state.on_stack.insert(v.clone(), true);
        state.visited.insert(v.clone(), true);

        // Visit all callees
        for w in self.callees(v) {
            if !state.visited.contains_key(w) {
                // Successor w has not yet been visited; recurse on it
                self.tarjan_visit(w, state);
                let low_w = *state.low_links.get(w).unwrap_or(&usize::MAX);
                let low_v = *state.low_links.get(v).unwrap();
                state.low_links.insert(v.clone(), low_v.min(low_w));
            } else if *state.on_stack.get(w).unwrap_or(&false) {
                // Successor w is in stack and hence in the current SCC
                let idx_w = *state.indices.get(w).unwrap_or(&usize::MAX);
                let low_v = *state.low_links.get(v).unwrap();
                state.low_links.insert(v.clone(), low_v.min(idx_w));
            }
        }

        // If v is a root node, pop the stack and generate an SCC
        let low_v = *state.low_links.get(v).unwrap();
        let idx_v = *state.indices.get(v).unwrap();

        if low_v == idx_v {
            let mut scc = Vec::new();
            loop {
                let w = state.stack.pop().unwrap();
                state.on_stack.insert(w.clone(), false);
                scc.push(w.clone());
                if &w == v {
                    break;
                }
            }

            // Convert SCC to TopologicalNode
            let node = if scc.len() == 1 && !self.callees(&scc[0]).contains(&scc[0]) {
                // Single node with no self-loop
                TopologicalNode::Single(scc.into_iter().next().unwrap())
            } else {
                // Multiple nodes or self-loop = cycle
                TopologicalNode::Cycle(scc)
            };

            state.result.push(node);
        }
    }
}

/// State for Tarjan's SCC algorithm
struct TarjanState {
    index: usize,
    indices: HashMap<SymbolPath, usize>,
    low_links: HashMap<SymbolPath, usize>,
    on_stack: HashMap<SymbolPath, bool>,
    visited: HashMap<SymbolPath, bool>,
    stack: Vec<SymbolPath>,
    result: Vec<TopologicalNode>,
}

impl TarjanState {
    fn new(procedures: &[SymbolPath]) -> Self {
        let capacity = procedures.len();
        Self {
            index: 0,
            indices: HashMap::with_capacity(capacity),
            low_links: HashMap::with_capacity(capacity),
            on_stack: HashMap::with_capacity(capacity),
            visited: HashMap::with_capacity(capacity),
            stack: Vec::with_capacity(capacity),
            result: Vec::with_capacity(capacity),
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// Topological Node
// ═══════════════════════════════════════════════════════════════════════════════

/// A node in the topological ordering of procedures.
#[derive(Debug, Clone)]
pub enum TopologicalNode {
    /// A single procedure with no cycles involving it.
    /// Can be analyzed once all its callees are analyzed.
    Single(SymbolPath),

    /// A strongly connected component of mutually recursive procedures.
    /// These procedures call each other (directly or transitively) forming a cycle.
    /// They need special handling (e.g., mark as Unknown or iterative refinement).
    Cycle(Vec<SymbolPath>),
}

impl TopologicalNode {
    /// Get all procedure paths in this node.
    pub fn paths(&self) -> Vec<&SymbolPath> {
        match self {
            TopologicalNode::Single(path) => vec![path],
            TopologicalNode::Cycle(paths) => paths.iter().collect(),
        }
    }

    /// Check if this is a cycle (SCC with multiple procedures or self-recursion).
    pub fn is_cycle(&self) -> bool {
        matches!(self, TopologicalNode::Cycle(_))
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// Call Collector
// ═══════════════════════════════════════════════════════════════════════════════

/// Collects procedure call targets from a procedure body.
pub struct CallCollector {
    calls: Vec<InvocationTarget>,
}

impl CallCollector {
    /// Collect all procedure call targets from a procedure.
    pub fn collect_from_procedure(proc: &Procedure) -> Vec<InvocationTarget> {
        let mut collector = Self { calls: Vec::new() };
        let _ = visit::visit_procedure(&mut collector, proc);
        collector.calls
    }
}

impl Visit for CallCollector {
    fn visit_op(&mut self, op: &Op) -> std::ops::ControlFlow<()> {
        if let Op::Inst(inst) = op {
            match inst.inner() {
                Instruction::Exec(target)
                | Instruction::Call(target)
                | Instruction::SysCall(target) => {
                    // Skip MAST roots - they can't be resolved symbolically
                    if !matches!(target, InvocationTarget::MastRoot(_)) {
                        self.calls.push(target.clone());
                    }
                }
                _ => {}
            }
        }
        visit::visit_op(self, op)
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// Tests
// ═══════════════════════════════════════════════════════════════════════════════

#[cfg(test)]
mod tests {
    use super::*;

    fn make_graph(edges: &[(&str, &[&str])]) -> CallGraph {
        let procedures: Vec<SymbolPath> = edges.iter().map(|(name, _)| SymbolPath::new(*name)).collect();

        let calls: HashMap<SymbolPath, Vec<SymbolPath>> = edges
            .iter()
            .map(|(name, callees)| {
                (
                    SymbolPath::new(*name),
                    callees.iter().map(|c| SymbolPath::new(*c)).collect(),
                )
            })
            .collect();

        CallGraph { calls, procedures }
    }

    #[test]
    fn test_linear_chain() {
        // a -> b -> c (c is leaf)
        let graph = make_graph(&[
            ("a", &["b"]),
            ("b", &["c"]),
            ("c", &[]),
        ]);

        let order = graph.topological_order();

        // Should be: c, b, a (leaves first)
        assert_eq!(order.len(), 3);
        assert!(matches!(&order[0], TopologicalNode::Single(p) if p.as_ref() == "c"));
        assert!(matches!(&order[1], TopologicalNode::Single(p) if p.as_ref() == "b"));
        assert!(matches!(&order[2], TopologicalNode::Single(p) if p.as_ref() == "a"));
    }

    #[test]
    fn test_diamond() {
        //   a
        //  / \
        // b   c
        //  \ /
        //   d
        let graph = make_graph(&[
            ("a", &["b", "c"]),
            ("b", &["d"]),
            ("c", &["d"]),
            ("d", &[]),
        ]);

        let order = graph.topological_order();

        // d must come first, then b and c (order doesn't matter), then a
        assert_eq!(order.len(), 4);

        let names: Vec<&str> = order
            .iter()
            .filter_map(|n| match n {
                TopologicalNode::Single(p) => Some(p.as_ref()),
                _ => None,
            })
            .collect();

        // d before b and c, which are before a
        let pos_a = names.iter().position(|&n| n == "a").unwrap();
        let pos_b = names.iter().position(|&n| n == "b").unwrap();
        let pos_c = names.iter().position(|&n| n == "c").unwrap();
        let pos_d = names.iter().position(|&n| n == "d").unwrap();

        assert!(pos_d < pos_b);
        assert!(pos_d < pos_c);
        assert!(pos_b < pos_a);
        assert!(pos_c < pos_a);
    }

    #[test]
    fn test_simple_cycle() {
        // a -> b -> a (mutual recursion)
        let graph = make_graph(&[
            ("a", &["b"]),
            ("b", &["a"]),
        ]);

        let order = graph.topological_order();

        // Should be one cycle containing both a and b
        assert_eq!(order.len(), 1);
        assert!(matches!(&order[0], TopologicalNode::Cycle(paths) if paths.len() == 2));
    }

    #[test]
    fn test_self_recursion() {
        // a calls itself
        let graph = make_graph(&[
            ("a", &["a"]),
        ]);

        let order = graph.topological_order();

        // Should be a cycle with just a
        assert_eq!(order.len(), 1);
        assert!(matches!(&order[0], TopologicalNode::Cycle(paths) if paths.len() == 1));
    }

    #[test]
    fn test_cycle_with_entry() {
        // entry -> a -> b -> a (entry calls into a cycle)
        let graph = make_graph(&[
            ("entry", &["a"]),
            ("a", &["b"]),
            ("b", &["a"]),
        ]);

        let order = graph.topological_order();

        // Should be: cycle(a,b), then entry
        assert_eq!(order.len(), 2);
        assert!(matches!(&order[0], TopologicalNode::Cycle(_)));
        assert!(matches!(&order[1], TopologicalNode::Single(p) if p.as_ref() == "entry"));
    }

    #[test]
    fn test_independent_procedures() {
        // No calls between procedures
        let graph = make_graph(&[
            ("a", &[]),
            ("b", &[]),
            ("c", &[]),
        ]);

        let order = graph.topological_order();

        // All are singles (order doesn't matter)
        assert_eq!(order.len(), 3);
        for node in &order {
            assert!(matches!(node, TopologicalNode::Single(_)));
        }
    }
}
