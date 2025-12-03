//! Tests for stack effect handling.

use super::*;
use miden_assembly_syntax::ast::ModuleKind;
use miden_assembly_syntax::{Parse, ParseOptions};
use miden_debug_types::{DefaultSourceManager, SourceLanguage, SourceManager, Uri};

/// Helper to parse a procedure and return the analysis state after running
fn analyze_proc(source: &str) -> AnalysisState {
    let source_manager = DefaultSourceManager::default();
    let uri = Uri::from("test://test.masm");
    let full_source = format!("proc test_proc\n{}\nend", source);
    source_manager.load(SourceLanguage::Masm, uri.clone(), full_source);

    let source_file = source_manager
        .get_by_uri(&uri)
        .expect("Failed to load source");
    let mut module_path = miden_assembly_syntax::ast::PathBuf::default();
    module_path.push("test");
    let opts = ParseOptions {
        kind: ModuleKind::Library,
        path: Some(module_path.into()),
        ..Default::default()
    };
    let module = source_file
        .parse_with_options(&source_manager, opts)
        .expect("Failed to parse MASM");

    let mut state = AnalysisState::new("test_proc".to_string());

    // Walk the procedure body and apply effects
    use miden_assembly_syntax::ast::visit::{self, Visit};
    use std::ops::ControlFlow;

    struct EffectApplier<'a> {
        state: &'a mut AnalysisState,
    }

    impl<'a> Visit for EffectApplier<'a> {
        fn visit_inst(&mut self, inst: &miden_debug_types::Span<Instruction>) -> ControlFlow<()> {
            apply_effect(inst.inner(), self.state, inst.span());
            ControlFlow::Continue(())
        }
    }

    let mut applier = EffectApplier { state: &mut state };
    let _ = visit::visit_module(&mut applier, &module);

    state
}

// ═══════════════════════════════════════════════════════════════════════
// P0: Push value extraction tests
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn test_push_extracts_actual_value() {
    let state = analyze_proc("push.42");

    assert_eq!(state.stack.depth(), 1);
    let top = state.stack.peek(0).unwrap();
    assert_eq!(top.bounds, Bounds::Const(42));
}

#[test]
fn test_push_large_value() {
    let state = analyze_proc("push.1000000");

    assert_eq!(state.stack.depth(), 1);
    let top = state.stack.peek(0).unwrap();
    assert_eq!(top.bounds, Bounds::Const(1000000));
}

#[test]
fn test_push_multiple_values() {
    let state = analyze_proc("push.10 push.20 push.30");

    assert_eq!(state.stack.depth(), 3);
    // Stack order: 30 is on top (position 0), 20 at position 1, 10 at position 2
    assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(30));
    assert_eq!(state.stack.peek(1).unwrap().bounds, Bounds::Const(20));
    assert_eq!(state.stack.peek(2).unwrap().bounds, Bounds::Const(10));
}

#[test]
fn test_push_zero() {
    let state = analyze_proc("push.0");

    assert_eq!(state.stack.depth(), 1);
    let top = state.stack.peek(0).unwrap();
    assert_eq!(top.bounds, Bounds::Const(0));
}

#[test]
fn test_push_u32_max() {
    let state = analyze_proc("push.4294967295"); // u32::MAX

    assert_eq!(state.stack.depth(), 1);
    let top = state.stack.peek(0).unwrap();
    assert_eq!(top.bounds, Bounds::Const(4294967295));
}

// ═══════════════════════════════════════════════════════════════════════
// Stack manipulation tests
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn test_dup_preserves_bounds() {
    let state = analyze_proc("push.42 dup.0");

    assert_eq!(state.stack.depth(), 2);
    assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(42));
    assert_eq!(state.stack.peek(1).unwrap().bounds, Bounds::Const(42));
}

#[test]
fn test_swap_preserves_bounds() {
    let state = analyze_proc("push.10 push.20 swap");

    assert_eq!(state.stack.depth(), 2);
    assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(10));
    assert_eq!(state.stack.peek(1).unwrap().bounds, Bounds::Const(20));
}

#[test]
fn test_drop_removes_top() {
    let state = analyze_proc("push.10 push.20 drop");

    assert_eq!(state.stack.depth(), 1);
    assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(10));
}

#[test]
fn test_movup_preserves_bounds() {
    let state = analyze_proc("push.1 push.2 push.3 movup.2");

    // After movup.2: [1, 3, 2] -> [1] moves to top
    assert_eq!(state.stack.depth(), 3);
    assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(1));
    assert_eq!(state.stack.peek(1).unwrap().bounds, Bounds::Const(3));
    assert_eq!(state.stack.peek(2).unwrap().bounds, Bounds::Const(2));
}

// ═══════════════════════════════════════════════════════════════════════
// Advice operation tests
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn test_adv_push_creates_untrusted() {
    let state = analyze_proc("adv_push.1");

    assert_eq!(state.stack.depth(), 1);
    let top = state.stack.peek(0).unwrap();
    assert!(top.origin.is_untrusted());
    assert!(!top.is_validated());
}

#[test]
fn test_adv_push_multiple() {
    let state = analyze_proc("adv_push.3");

    assert_eq!(state.stack.depth(), 3);
    for i in 0..3 {
        let elem = state.stack.peek(i).unwrap();
        assert!(elem.origin.is_untrusted());
    }
}

// ═══════════════════════════════════════════════════════════════════════
// Validation tests
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn test_u32assert_validates_top() {
    let state = analyze_proc("adv_push.1 u32assert");

    assert_eq!(state.stack.depth(), 1);
    let top = state.stack.peek(0).unwrap();
    assert!(top.is_validated());
    assert!(top.bounds.is_u32());
}

#[test]
fn test_u32assert2_validates_top_two() {
    let state = analyze_proc("adv_push.2 u32assert2");

    assert_eq!(state.stack.depth(), 2);
    assert!(state.stack.peek(0).unwrap().is_validated());
    assert!(state.stack.peek(1).unwrap().is_validated());
}

// ═══════════════════════════════════════════════════════════════════════
// P0: u32 operation stack effect tests
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn test_u32_test_pushes_bool() {
    // u32test doesn't pop, just pushes a bool
    let state = analyze_proc("push.42 u32test");

    assert_eq!(state.stack.depth(), 2); // original value + bool result
    assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Bool);
    assert_eq!(state.stack.peek(1).unwrap().bounds, Bounds::Const(42));
}

#[test]
fn test_u32_split_pop1_push2() {
    // u32split pops 1, pushes 2 (hi, lo)
    let state = analyze_proc("push.1 u32split");

    assert_eq!(state.stack.depth(), 2);
    assert!(state.stack.peek(0).unwrap().bounds.is_u32()); // hi
    assert!(state.stack.peek(1).unwrap().bounds.is_u32()); // lo
}

#[test]
fn test_u32_wrapping_add_pop2_push1() {
    let state = analyze_proc("push.10 push.20 u32wrapping_add");

    assert_eq!(state.stack.depth(), 1);
    assert!(state.stack.peek(0).unwrap().bounds.is_u32());
}

#[test]
fn test_u32_wrapping_add_imm_pop1_push1() {
    let state = analyze_proc("push.10 u32wrapping_add.5");

    assert_eq!(state.stack.depth(), 1);
    assert!(state.stack.peek(0).unwrap().bounds.is_u32());
}

#[test]
fn test_u32_wrapping_sub_pop2_push1() {
    let state = analyze_proc("push.10 push.3 u32wrapping_sub");

    assert_eq!(state.stack.depth(), 1);
    assert!(state.stack.peek(0).unwrap().bounds.is_u32());
}

#[test]
fn test_u32_overflowing_add_pop2_push2() {
    // u32overflowing_add pops 2, pushes 2 (result, overflow)
    let state = analyze_proc("push.10 push.20 u32overflowing_add");

    assert_eq!(state.stack.depth(), 2);
    // Overflow flag on top, result below
    assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Bool); // overflow
    assert!(state.stack.peek(1).unwrap().bounds.is_u32()); // result
}

#[test]
fn test_u32_overflowing_add3_pop3_push2() {
    // u32overflowing_add3 pops 3, pushes 2 (result, carry)
    let state = analyze_proc("push.1 push.2 push.3 u32overflowing_add3");

    assert_eq!(state.stack.depth(), 2);
    assert!(state.stack.peek(0).unwrap().bounds.is_u32()); // carry
    assert!(state.stack.peek(1).unwrap().bounds.is_u32()); // result
}

#[test]
fn test_u32_overflowing_sub_pop2_push2() {
    // u32overflowing_sub pops 2, pushes 2 (result, borrow flag)
    let state = analyze_proc("push.10 push.3 u32overflowing_sub");

    assert_eq!(state.stack.depth(), 2);
    assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Bool); // borrow flag
    assert!(state.stack.peek(1).unwrap().bounds.is_u32()); // result
}

#[test]
fn test_u32_overflowing_mul_pop2_push2_flag_on_top() {
    // u32overflowing_mul pops 2, pushes 2 (result, high bits)
    let state = analyze_proc("push.2 push.3 u32overflowing_mul");

    assert_eq!(state.stack.depth(), 2);
    assert!(state.stack.peek(0).unwrap().bounds.is_u32()); // high bits
    assert!(state.stack.peek(1).unwrap().bounds.is_u32()); // result
}

#[test]
fn test_u32_wrapping_add3_pop3_push1() {
    // u32wrapping_add3 pops 3, pushes 1
    let state = analyze_proc("push.1 push.2 push.3 u32wrapping_add3");

    assert_eq!(state.stack.depth(), 1);
    assert!(state.stack.peek(0).unwrap().bounds.is_u32());
}

#[test]
fn test_u32_overflowing_madd_overflow_on_top() {
    // u32overflowing_madd pops 3, pushes 2 (overflow, result)
    let state = analyze_proc("push.1 push.2 push.3 u32overflowing_madd");

    assert_eq!(state.stack.depth(), 2);
    assert!(state.stack.peek(0).unwrap().bounds.is_u32()); // overflow/upper bits
    assert!(state.stack.peek(1).unwrap().bounds.is_u32()); // result
}

#[test]
fn test_u32_divmod_pop2_push2() {
    // u32divmod pops 2, pushes 2 (remainder, quotient)
    let state = analyze_proc("push.10 push.3 u32divmod");

    assert_eq!(state.stack.depth(), 2);
    assert!(state.stack.peek(0).unwrap().bounds.is_u32()); // remainder
    assert!(state.stack.peek(1).unwrap().bounds.is_u32()); // quotient
}

#[test]
fn test_u32_divmod_imm_pop1_push2() {
    // u32divmod.N pops 1, pushes 2
    let state = analyze_proc("push.10 u32divmod.3");

    assert_eq!(state.stack.depth(), 2);
    assert!(state.stack.peek(0).unwrap().bounds.is_u32()); // remainder
    assert!(state.stack.peek(1).unwrap().bounds.is_u32()); // quotient
}

#[test]
fn test_u32_not_pop1_push1() {
    let state = analyze_proc("push.255 u32not");

    assert_eq!(state.stack.depth(), 1);
    assert!(state.stack.peek(0).unwrap().bounds.is_u32());
}

#[test]
fn test_u32_shl_pop2_push1() {
    let state = analyze_proc("push.1 push.4 u32shl");

    assert_eq!(state.stack.depth(), 1);
    assert!(state.stack.peek(0).unwrap().bounds.is_u32());
}

#[test]
fn test_u32_shl_imm_pop1_push1() {
    let state = analyze_proc("push.1 u32shl.4");

    assert_eq!(state.stack.depth(), 1);
    assert!(state.stack.peek(0).unwrap().bounds.is_u32());
}

#[test]
fn test_u32_popcnt_pop1_push1() {
    let state = analyze_proc("push.255 u32popcnt");

    assert_eq!(state.stack.depth(), 1);
    assert!(state.stack.peek(0).unwrap().bounds.is_u32());
}

#[test]
fn test_u32_lt_pop2_push_bool() {
    let state = analyze_proc("push.10 push.20 u32lt");

    assert_eq!(state.stack.depth(), 1);
    assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Bool);
}

#[test]
fn test_u32_min_pop2_push1() {
    let state = analyze_proc("push.10 push.20 u32min");

    assert_eq!(state.stack.depth(), 1);
    assert!(state.stack.peek(0).unwrap().bounds.is_u32());
}

#[test]
fn test_u32_cast_pop1_push1() {
    let state = analyze_proc("push.1000000000000 u32cast");

    assert_eq!(state.stack.depth(), 1);
    assert!(state.stack.peek(0).unwrap().bounds.is_u32());
}

// ═══════════════════════════════════════════════════════════════════════
// P1: Word operations stack effect tests
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn test_mem_loadw_be_pop1_push4() {
    let state = analyze_proc("push.1000 mem_loadw_be");

    assert_eq!(state.stack.depth(), 4);
    // All 4 values should be from memory
    for i in 0..4 {
        assert!(matches!(
            state.stack.peek(i).unwrap().origin,
            crate::analysis::types::ValueOrigin::Memory
        ));
    }
}

#[test]
fn test_mem_storew_be_pop5() {
    let state = analyze_proc("push.1 push.2 push.3 push.4 push.1000 mem_storew_be");

    // 5 values pushed, 5 popped (addr + 4 values)
    assert_eq!(state.stack.depth(), 0);
}

#[test]
fn test_loc_load_push1() {
    let state = analyze_proc("loc_load.0");

    assert_eq!(state.stack.depth(), 1);
}

#[test]
fn test_loc_loadw_be_push4() {
    let state = analyze_proc("loc_loadw_be.0");

    assert_eq!(state.stack.depth(), 4);
}

#[test]
fn test_loc_store_pop1() {
    let state = analyze_proc("push.42 loc_store.0");

    assert_eq!(state.stack.depth(), 0);
}

#[test]
fn test_loc_storew_be_pop4() {
    let state = analyze_proc("push.1 push.2 push.3 push.4 loc_storew_be.0");

    assert_eq!(state.stack.depth(), 0);
}

#[test]
fn test_swapw_preserves_bounds() {
    // Push 8 values: [1,2,3,4] and [5,6,7,8]
    let state = analyze_proc("push.1 push.2 push.3 push.4 push.5 push.6 push.7 push.8 swapw");

    assert_eq!(state.stack.depth(), 8);
    // After swapw.1: word at 0-3 swapped with word at 4-7
    // Original: top=[8,7,6,5,4,3,2,1] (8 on top)
    // After: top=[4,3,2,1,8,7,6,5]
    assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(4));
    assert_eq!(state.stack.peek(1).unwrap().bounds, Bounds::Const(3));
    assert_eq!(state.stack.peek(2).unwrap().bounds, Bounds::Const(2));
    assert_eq!(state.stack.peek(3).unwrap().bounds, Bounds::Const(1));
    assert_eq!(state.stack.peek(4).unwrap().bounds, Bounds::Const(8));
    assert_eq!(state.stack.peek(5).unwrap().bounds, Bounds::Const(7));
}

#[test]
fn test_reversew_reverses_word() {
    let state = analyze_proc("push.1 push.2 push.3 push.4 reversew");

    assert_eq!(state.stack.depth(), 4);
    // Original: [4,3,2,1] (4 on top)
    // After reversew: [1,2,3,4]
    assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(1));
    assert_eq!(state.stack.peek(1).unwrap().bounds, Bounds::Const(2));
    assert_eq!(state.stack.peek(2).unwrap().bounds, Bounds::Const(3));
    assert_eq!(state.stack.peek(3).unwrap().bounds, Bounds::Const(4));
}

#[test]
fn test_dupw_pushes_4() {
    let state = analyze_proc("push.1 push.2 push.3 push.4 dupw.0");

    // dupw.0 duplicates the top word (positions 0-3)
    // Initial stack: [4, 3, 2, 1] (4 on top)
    // After dupw.0: [4, 3, 2, 1, 4, 3, 2, 1]
    assert_eq!(state.stack.depth(), 8);
    // Top word (duplicated): positions 0-3
    assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(4));
    assert_eq!(state.stack.peek(1).unwrap().bounds, Bounds::Const(3));
    assert_eq!(state.stack.peek(2).unwrap().bounds, Bounds::Const(2));
    assert_eq!(state.stack.peek(3).unwrap().bounds, Bounds::Const(1));
    // Original word: positions 4-7
    assert_eq!(state.stack.peek(4).unwrap().bounds, Bounds::Const(4));
    assert_eq!(state.stack.peek(5).unwrap().bounds, Bounds::Const(3));
    assert_eq!(state.stack.peek(6).unwrap().bounds, Bounds::Const(2));
    assert_eq!(state.stack.peek(7).unwrap().bounds, Bounds::Const(1));
}

#[test]
fn test_dropw_pops_4() {
    let state = analyze_proc("push.1 push.2 push.3 push.4 push.5 dropw");

    // 5 pushed, 4 dropped = 1 remaining
    assert_eq!(state.stack.depth(), 1);
    assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(1));
}

#[test]
fn test_padw_pushes_4_zeros() {
    let state = analyze_proc("padw");

    assert_eq!(state.stack.depth(), 4);
    for i in 0..4 {
        assert_eq!(state.stack.peek(i).unwrap().bounds, Bounds::Const(0));
    }
}

// ═══════════════════════════════════════════════════════════════════════
// P1: Bounds propagation through arithmetic tests
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn test_add_propagates_const_bounds() {
    let state = analyze_proc("push.10 push.5 add");

    assert_eq!(state.stack.depth(), 1);
    // 10 + 5 = 15
    assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(15));
}

#[test]
fn test_sub_propagates_const_bounds() {
    let state = analyze_proc("push.10 push.3 sub");

    assert_eq!(state.stack.depth(), 1);
    // 10 - 3 = 7
    assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(7));
}

#[test]
fn test_mul_propagates_const_bounds() {
    let state = analyze_proc("push.7 push.6 mul");

    assert_eq!(state.stack.depth(), 1);
    // 7 * 6 = 42
    assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(42));
}

#[test]
fn test_div_propagates_const_bounds() {
    let state = analyze_proc("push.42 push.6 div");

    assert_eq!(state.stack.depth(), 1);
    // 42 / 6 = 7
    assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(7));
}

#[test]
fn test_add_imm_propagates_bounds() {
    let state = analyze_proc("push.10 add.5");

    assert_eq!(state.stack.depth(), 1);
    // 10 + 5 = 15
    assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(15));
}

#[test]
fn test_sub_imm_propagates_bounds() {
    let state = analyze_proc("push.10 sub.3");

    assert_eq!(state.stack.depth(), 1);
    // 10 - 3 = 7
    assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(7));
}

#[test]
fn test_incr_via_add1_propagates_bounds() {
    // incr is add.1 in Miden syntax
    let state = analyze_proc("push.41 add.1");

    assert_eq!(state.stack.depth(), 1);
    // 41 + 1 = 42
    assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(42));
}

#[test]
fn test_chained_arithmetic_propagates() {
    // Test: (5 + 3) * 2 = 16
    let state = analyze_proc("push.5 push.3 add push.2 mul");

    assert_eq!(state.stack.depth(), 1);
    assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(16));
}

#[test]
fn test_eq_const_produces_bool() {
    let state = analyze_proc("push.5 push.5 eq");

    assert_eq!(state.stack.depth(), 1);
    // 5 == 5 -> 1
    assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(1));
}

#[test]
fn test_neq_const_produces_bool() {
    let state = analyze_proc("push.5 push.3 neq");

    assert_eq!(state.stack.depth(), 1);
    // 5 != 3 -> 1
    assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(1));
}

#[test]
fn test_lt_const_produces_bool() {
    let state = analyze_proc("push.3 push.5 lt");

    assert_eq!(state.stack.depth(), 1);
    // 3 < 5 -> 1
    assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(1));
}

// ═══════════════════════════════════════════════════════════════════════
// P2/P3: Additional instruction tests
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn test_hash_pop4_push4() {
    let state = analyze_proc("push.1 push.2 push.3 push.4 hash");

    assert_eq!(state.stack.depth(), 4);
}

#[test]
fn test_hmerge_pop8_push4() {
    let state = analyze_proc("push.1 push.2 push.3 push.4 push.5 push.6 push.7 push.8 hmerge");

    assert_eq!(state.stack.depth(), 4);
}

#[test]
fn test_sdepth_pushes_u32() {
    let state = analyze_proc("sdepth");

    assert_eq!(state.stack.depth(), 1);
    assert!(state.stack.peek(0).unwrap().bounds.is_u32());
}

#[test]
fn test_assert_pops_condition() {
    let state = analyze_proc("push.1 assert");

    assert_eq!(state.stack.depth(), 0);
}

#[test]
fn test_assertz_pops_value() {
    let state = analyze_proc("push.0 assertz");

    assert_eq!(state.stack.depth(), 0);
}

#[test]
fn test_eqw_pop8_push_bool() {
    let state = analyze_proc("push.1 push.2 push.3 push.4 push.1 push.2 push.3 push.4 eqw");

    assert_eq!(state.stack.depth(), 1);
    assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Bool);
}

#[test]
fn test_nop_no_effect() {
    let state = analyze_proc("push.42 nop");

    assert_eq!(state.stack.depth(), 1);
    assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Const(42));
}

#[test]
fn test_pow2_pop1_push1() {
    let state = analyze_proc("push.5 pow2");

    assert_eq!(state.stack.depth(), 1);
    // pow2 result is in Field range
    assert_eq!(state.stack.peek(0).unwrap().bounds, Bounds::Field);
}

#[test]
fn test_ilog2_pop1_push_u32() {
    let state = analyze_proc("push.256 ilog2");

    assert_eq!(state.stack.depth(), 1);
    assert!(state.stack.peek(0).unwrap().bounds.is_u32());
}
