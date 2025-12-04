//! Unified instruction semantics facade backed by generated metadata.
//!
//! Provides a single source of truth for stack effects and stack op
//! classification, with small manual overrides for dynamic counts.

use miden_assembly_syntax::ast::{Immediate, Instruction};

use super::static_effect::StackOp;

// Generated semantics map derived from documentation plus overrides.
include!(concat!(env!("OUT_DIR"), "/instruction_map.rs"));

/// Basic, structural semantics of an instruction.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct InstructionSemantics {
    /// Number of elements popped from the stack.
    pub pops: usize,
    /// Number of elements pushed to the stack.
    pub pushes: usize,
    /// Classification of the effect.
    pub kind: SemanticsKind,
}

impl InstructionSemantics {
    /// Net change in stack depth (pushes - pops).
    pub fn net(&self) -> i32 {
        self.pushes as i32 - self.pops as i32
    }
}

/// Classification of an instruction's effect.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SemanticsKind {
    /// Pure stack manipulation (reorder/dup/drop).
    StackOp(StackOp),
    /// Fixed pop/push counts.
    Static,
    /// Dynamic/unknown effect.
    Dynamic,
}

/// Get coarse semantics for an instruction.
///
/// This is intentionally conservative: anything without a known static effect
/// is marked `None` so callers can treat it as dynamic.
pub fn semantics_of(inst: &Instruction) -> Option<InstructionSemantics> {
    if let Some(manual) = manual_semantics(inst) {
        return Some(manual);
    }

    // Fast-path manual/data overrides before looking at docs.
    if let Some(override_sem) = override_by_name(inst) {
        return Some(override_sem);
    }

    // Treat dynamic calls as unknown even if docs list shapes.
    match inst {
        Instruction::DynExec
        | Instruction::DynCall
        | Instruction::Exec(_)
        | Instruction::Call(_)
        | Instruction::SysCall(_) => return None,
        _ => {}
    }

    semantics_from_docs(inst)
}

#[cfg(test)]
mod tests {
    use super::*;
    use miden_assembly_syntax::ast::{Immediate, Instruction};
    use miden_assembly_syntax::{
        ast::{Module, Op},
        Parse, ParseOptions,
    };
    use miden_debug_types::DefaultSourceManager;
    use std::collections::BTreeSet;

    fn count_tokens(shape: &str) -> usize {
        shape
            .trim_matches(['[', ']'])
            .split(',')
            .filter(|s| {
                let t = s.trim();
                !t.is_empty() && t != "..." && t.to_ascii_lowercase() != "stack"
            })
            .count()
    }

    #[test]
    fn semantics_for_basic_arithmetic() {
        let sem = semantics_of(&Instruction::Add).expect("add semantics");
        assert_eq!(sem.pops, 2);
        assert_eq!(sem.pushes, 1);
        assert_eq!(sem.net(), -1);
        assert!(matches!(sem.kind, SemanticsKind::Static));
    }

    #[test]
    fn semantics_for_stack_op_dup() {
        let sem = semantics_of(&Instruction::Dup1).expect("dup semantics");
        assert_eq!(sem.pops, 0);
        assert_eq!(sem.pushes, 1);
        match sem.kind {
            SemanticsKind::StackOp(StackOp::Dup(1)) => {}
            other => panic!("unexpected kind: {:?}", other),
        }
    }

    #[test]
    fn semantics_for_dynamic_call_is_none() {
        assert!(semantics_of(&Instruction::DynExec).is_none());
    }

    #[test]
    fn doc_semantics_cover_all_doc_entries() {
        for (name, _) in INSTRUCTION_MAP.entries() {
            assert!(
                INSTRUCTION_SEMANTICS.get(*name).is_some(),
                "missing doc semantics for {name}"
            );
        }
    }

    #[test]
    fn doc_semantics_counts_match_shapes() {
        for (name, info) in INSTRUCTION_MAP.entries() {
            let pops = count_tokens(info.stack_input);
            let pushes = count_tokens(info.stack_output);
            if let Some(entry) = INSTRUCTION_SEMANTICS.get(*name) {
                assert_eq!(
                    entry.pops, pops,
                    "doc-derived pops mismatch for {} ({} vs {})",
                    name, entry.pops, pops
                );
                assert_eq!(
                    entry.pushes, pushes,
                    "doc-derived pushes mismatch for {} ({} vs {})",
                    name, entry.pushes, pushes
                );
            }
        }
    }

    #[test]
    fn semantics_cover_parsable_docs() {
        let mut parsed = 0usize;
        let mut skipped: BTreeSet<&'static str> = BTreeSet::new();
        let mut missing: BTreeSet<&'static str> = BTreeSet::new();

        for (name, info) in INSTRUCTION_MAP.entries() {
            if is_control_flow_doc_name(name) {
                continue;
            }
            if let Some(inst) = parse_instruction_from_docs(name, info) {
                parsed += 1;
                if matches!(
                    inst,
                    Instruction::DynExec
                        | Instruction::DynCall
                        | Instruction::Exec(_)
                        | Instruction::Call(_)
                        | Instruction::SysCall(_)
                ) {
                    continue;
                }
                if semantics_of(&inst).is_none() {
                    missing.insert(*name);
                }
            } else {
                skipped.insert(*name);
            }
        }

        assert!(parsed > 0, "no instructions parsed; skipped: {:?}", skipped);
        assert!(
            missing.is_empty(),
            "missing semantics for instructions: {:?}",
            missing
        );
    }

    #[test]
    fn manual_overrides_are_applied() {
        // Doc map says push 1 for mem_loadw_be; override should report 4.
        let doc = semantics_from_docs(&Instruction::MemLoadWBe).expect("doc sem");
        assert_eq!(doc.pushes, 1, "doc pushes should reflect raw doc tokens");
        let sem = semantics_of(&Instruction::MemLoadWBe).expect("semantics");
        assert_eq!((sem.pops, sem.pushes), (5, 4));

        // Immediate variants should use the .a override (4 pops instead of 5).
        let sem = semantics_of(&Instruction::MemStoreWBeImm(Immediate::from(0u32))).expect("sem");
        assert_eq!((sem.pops, sem.pushes), (4, 0));

        // HMerge doc tokens are too small; override should reflect full word hashing.
        let doc = semantics_from_docs(&Instruction::HMerge).expect("doc sem");
        assert_eq!(doc.pops, 2);
        let sem = semantics_of(&Instruction::HMerge).expect("semantics");
        assert_eq!((sem.pops, sem.pushes), (8, 4));

        // Conditional word swap/drop should use full word counts.
        let sem = semantics_of(&Instruction::CSwapW).expect("semantics");
        assert_eq!((sem.pops, sem.pushes), (9, 8));
        let sem = semantics_of(&Instruction::CDropW).expect("semantics");
        assert_eq!((sem.pops, sem.pushes), (9, 4));

        // Word drop and u32 tests/asserts are corrected by overrides.
        assert_eq!(
            semantics_of(&Instruction::DropW).map(|s| (s.pops, s.pushes)),
            Some((4, 0))
        );
        assert_eq!(
            semantics_of(&Instruction::U32Test).map(|s| (s.pops, s.pushes)),
            Some((0, 1))
        );
        assert_eq!(
            semantics_of(&Instruction::U32AssertW).map(|s| (s.pops, s.pushes)),
            Some((0, 0))
        );

        // Log/emit/reversew are effect-free despite doc tokens.
        assert_eq!(
            semantics_of(&Instruction::LogPrecompile).map(|s| (s.pops, s.pushes)),
            Some((8, 12))
        );
        assert_eq!(
            semantics_of(&Instruction::Emit).map(|s| (s.pops, s.pushes)),
            Some((0, 0))
        );
        assert_eq!(
            semantics_of(&Instruction::Reversew).map(|s| (s.pops, s.pushes)),
            Some((0, 0))
        );

        // Immediate store should pick the `.a` doc entry rather than the base shape.
        assert_eq!(
            semantics_of(&Instruction::MemStoreImm(Immediate::from(0u32)))
                .map(|s| (s.pops, s.pushes)),
            Some((1, 0))
        );
    }

    #[test]
    fn every_override_roundtrips_through_parser() {
        let mut parsed = 0usize;
        let mut skipped: BTreeSet<&'static str> = BTreeSet::new();

        for (name, entry) in INSTRUCTION_SEMANTICS_OVERRIDES.entries() {
            if let Some(inst) = parse_instruction_from_text(name) {
                parsed += 1;
                let sem = semantics_of(&inst)
                    .unwrap_or_else(|| panic!("no semantics for override {} ({inst:?})", name));
                assert_eq!(
                    (sem.pops, sem.pushes),
                    (entry.pops, entry.pushes),
                    "override mismatch for {} ({inst:?})",
                    name
                );
            } else {
                skipped.insert(*name);
            }
        }

        assert!(parsed > 0, "no overrides parsed; skipped: {:?}", skipped);
    }

    #[test]
    fn override_table_is_loaded() {
        // The override map is generated at build time; spot-check a couple of entries.
        assert!(INSTRUCTION_SEMANTICS_OVERRIDES.get("hmerge").is_some());
        assert!(INSTRUCTION_SEMANTICS_OVERRIDES.get("mem_stream").is_some());
    }

    fn parse_instruction_from_docs(
        name: &'static str,
        _info: &InstructionInfo,
    ) -> Option<Instruction> {
        let inst_text = normalize_instruction_text(name)?;
        parse_instruction_from_text(&inst_text)
    }

    fn parse_instruction_from_text(name: &str) -> Option<Instruction> {
        let inst_text = normalize_instruction_text(name)?;
        let module_src = format!("proc _test\n    {inst_text}\nend\n");

        let sources = DefaultSourceManager::default();
        // Default to library parsing to allow plain `proc` bodies.
        let parsed = module_src
            .parse_with_options(&sources, ParseOptions::for_library())
            .ok()?;
        let module: Module = (*parsed).clone();

        let first_proc = module.procedures().next()?;
        match first_proc.body().iter().next()? {
            Op::Inst(span) => Some(span.clone().into_inner()),
            _ => None,
        }
    }

    /// Adjust doc identifiers so they can be parsed by the MASM parser.
    ///
    /// - Adds placeholder immediates/targets where required.
    /// - Replaces `.a` suffixes with `.0`.
    fn normalize_instruction_text(name: &str) -> Option<String> {
        // Flow-control ops are not Instruction variants.
        if is_control_flow_doc_name(name) {
            return None;
        }

        // Replace `.a` helper suffix with a concrete value.
        let mut text = name.replace(".a", ".0");

        // Add required targets for call-like ops.
        if matches!(text.as_str(), "exec" | "call" | "syscall" | "procref") {
            text.push_str(".foo");
        }

        // Locals require an index.
        if text.starts_with("loc_") && !text.contains('.') {
            text.push_str(".0");
        }

        // Advice push requires a count.
        if text == "adv_push" {
            text.push_str(".1");
        }

        // Push requires a value.
        if text == "push" {
            text.push_str(".0");
        }

        Some(text)
    }

    fn is_control_flow_doc_name(name: &str) -> bool {
        matches!(name, "if.true" | "if.false" | "repeat" | "while.true")
    }
}

fn semantics_from_docs(inst: &Instruction) -> Option<InstructionSemantics> {
    let name = inst.to_string();
    let base = name.split('.').next()?;
    let alt_a = format!("{base}.a");
    let entry = INSTRUCTION_SEMANTICS
        .get(&name)
        .or_else(|| INSTRUCTION_SEMANTICS.get(alt_a.as_str()))
        .or_else(|| INSTRUCTION_SEMANTICS.get(base))?;

    let kind = StackOp::of(inst)
        .map(SemanticsKind::StackOp)
        .unwrap_or(SemanticsKind::Static);

    Some(InstructionSemantics {
        pops: entry.pops,
        pushes: entry.pushes,
        kind,
    })
}

/// Manual overrides for instructions whose documentation stack shapes are incomplete.
fn override_by_name(inst: &Instruction) -> Option<InstructionSemantics> {
    // Handle immediate variants that map to ".a" overrides
    match inst {
        Instruction::MemLoadWBeImm(_)
        | Instruction::MemLoadWLeImm(_)
        | Instruction::MemStoreWBeImm(_)
        | Instruction::MemStoreWLeImm(_) => {
            let base = inst
                .to_string()
                .split('.')
                .next()
                .unwrap_or_default()
                .to_string();
            let alt_a = format!("{base}.a");
            let entry = INSTRUCTION_SEMANTICS_OVERRIDES
                .get(alt_a.as_str())
                .or_else(|| INSTRUCTION_SEMANTICS_OVERRIDES.get(base.as_str()));

            if let Some(entry) = entry {
                let kind = StackOp::of(inst)
                    .map(SemanticsKind::StackOp)
                    .unwrap_or(SemanticsKind::Static);
                return Some(InstructionSemantics {
                    pops: entry.pops,
                    pushes: entry.pushes,
                    kind,
                });
            }
        }
        _ => {}
    }

    let name = inst.to_string();
    let base = name.split('.').next().unwrap_or(name.as_str());
    let alt_a = format!("{base}.a");

    let entry = INSTRUCTION_SEMANTICS_OVERRIDES
        .get(name.as_str())
        .or_else(|| INSTRUCTION_SEMANTICS_OVERRIDES.get(alt_a.as_str()))
        .or_else(|| INSTRUCTION_SEMANTICS_OVERRIDES.get(base))?;

    let kind = StackOp::of(inst)
        .map(SemanticsKind::StackOp)
        .unwrap_or(SemanticsKind::Static);

    Some(InstructionSemantics {
        pops: entry.pops,
        pushes: entry.pushes,
        kind,
    })
}

fn manual_semantics(inst: &Instruction) -> Option<InstructionSemantics> {
    use Instruction::*;

    // Stack operations: preserve reorder semantics but counts are known.
    if let Some(op) = StackOp::of(inst) {
        let (pops, pushes) = match op {
            StackOp::Dup(_) => (0, 1),
            StackOp::DupW(_) => (0, 4),
            _ => (0, 0),
        };
        return Some(InstructionSemantics {
            pops,
            pushes,
            kind: SemanticsKind::StackOp(op),
        });
    }

    // Immediate arithmetic
    let static_counts = match inst {
        AddImm(_) | SubImm(_) | MulImm(_) | DivImm(_) | ExpImm(_) => Some((1, 1)),
        EqImm(_) | NeqImm(_) => Some((1, 1)),

        U32WrappingAddImm(_) | U32OverflowingAddImm(_) => Some((1, 1)),
        U32WrappingSubImm(_) | U32OverflowingSubImm(_) => Some((1, 1)),
        U32WrappingMulImm(_) | U32OverflowingMulImm(_) => Some((1, 1)),
        U32DivImm(_) | U32ModImm(_) => Some((1, 1)),
        U32DivModImm(_) => Some((1, 2)),
        U32ShlImm(_) | U32ShrImm(_) | U32RotlImm(_) | U32RotrImm(_) => Some((1, 1)),

        // Advice push with dynamic count
        AdvPush(imm) => {
            return match imm {
                Immediate::Value(v) => Some(InstructionSemantics {
                    pops: 0,
                    pushes: v.into_inner() as usize,
                    kind: SemanticsKind::Static,
                }),
                _ => None,
            }
        }

        // STARK Horner evaluations (base/ext) are stack-neutral (16 -> 16)
        HornerBase | HornerExt => Some((16, 16)),

        // System events are effect-free
        SysEvent(_) => Some((0, 0)),

        _ => None,
    };

    static_counts.map(|(pops, pushes)| InstructionSemantics {
        pops,
        pushes,
        kind: SemanticsKind::Static,
    })
}
