//! Trait and implementation for converting instructions to descriptions.
//!
//! This module provides the `ToDescription` trait which generates contextual
//! description strings for Miden assembly instructions, extracting parameter values
//! to create more informative descriptions than static lookup tables.

use miden_assembly_syntax::ast::{Immediate, Instruction};
use miden_assembly_syntax::parser::PushValue;
use miden_assembly_syntax::Felt;

/// Trait for converting an instruction to its description text.
pub trait ToDescription {
    /// Returns the description text for this instruction, or `None` if no dynamic description is available.
    ///
    /// When `None` is returned, callers should fall back to static description lookup.
    fn to_description(&self) -> Option<String>;
}

impl ToDescription for Instruction {
    fn to_description(&self) -> Option<String> {
        match self {
            // === Stack position: movup ===
            Instruction::MovUp2 => Some(format!("Moves the {} stack item to the top.", ordinal(2))),
            Instruction::MovUp3 => Some(format!("Moves the {} stack item to the top.", ordinal(3))),
            Instruction::MovUp4 => Some(format!("Moves the {} stack item to the top.", ordinal(4))),
            Instruction::MovUp5 => Some(format!("Moves the {} stack item to the top.", ordinal(5))),
            Instruction::MovUp6 => Some(format!("Moves the {} stack item to the top.", ordinal(6))),
            Instruction::MovUp7 => Some(format!("Moves the {} stack item to the top.", ordinal(7))),
            Instruction::MovUp8 => Some(format!("Moves the {} stack item to the top.", ordinal(8))),
            Instruction::MovUp9 => Some(format!("Moves the {} stack item to the top.", ordinal(9))),
            Instruction::MovUp10 => {
                Some(format!("Moves the {} stack item to the top.", ordinal(10)))
            }
            Instruction::MovUp11 => {
                Some(format!("Moves the {} stack item to the top.", ordinal(11)))
            }
            Instruction::MovUp12 => {
                Some(format!("Moves the {} stack item to the top.", ordinal(12)))
            }
            Instruction::MovUp13 => {
                Some(format!("Moves the {} stack item to the top.", ordinal(13)))
            }
            Instruction::MovUp14 => {
                Some(format!("Moves the {} stack item to the top.", ordinal(14)))
            }
            Instruction::MovUp15 => {
                Some(format!("Moves the {} stack item to the top.", ordinal(15)))
            }

            // === Stack position: movupw ===
            Instruction::MovUpW2 => {
                Some(format!("Moves the {} stack word to the top.", ordinal(2)))
            }
            Instruction::MovUpW3 => {
                Some(format!("Moves the {} stack word to the top.", ordinal(3)))
            }

            // === Stack position: movdn ===
            Instruction::MovDn2 => Some(format!(
                "Moves the top stack item to the {} position.",
                ordinal(2)
            )),
            Instruction::MovDn3 => Some(format!(
                "Moves the top stack item to the {} position.",
                ordinal(3)
            )),
            Instruction::MovDn4 => Some(format!(
                "Moves the top stack item to the {} position.",
                ordinal(4)
            )),
            Instruction::MovDn5 => Some(format!(
                "Moves the top stack item to the {} position.",
                ordinal(5)
            )),
            Instruction::MovDn6 => Some(format!(
                "Moves the top stack item to the {} position.",
                ordinal(6)
            )),
            Instruction::MovDn7 => Some(format!(
                "Moves the top stack item to the {} position.",
                ordinal(7)
            )),
            Instruction::MovDn8 => Some(format!(
                "Moves the top stack item to the {} position.",
                ordinal(8)
            )),
            Instruction::MovDn9 => Some(format!(
                "Moves the top stack item to the {} position.",
                ordinal(9)
            )),
            Instruction::MovDn10 => Some(format!(
                "Moves the top stack item to the {} position.",
                ordinal(10)
            )),
            Instruction::MovDn11 => Some(format!(
                "Moves the top stack item to the {} position.",
                ordinal(11)
            )),
            Instruction::MovDn12 => Some(format!(
                "Moves the top stack item to the {} position.",
                ordinal(12)
            )),
            Instruction::MovDn13 => Some(format!(
                "Moves the top stack item to the {} position.",
                ordinal(13)
            )),
            Instruction::MovDn14 => Some(format!(
                "Moves the top stack item to the {} position.",
                ordinal(14)
            )),
            Instruction::MovDn15 => Some(format!(
                "Moves the top stack item to the {} position.",
                ordinal(15)
            )),

            // === Stack position: movdnw ===
            Instruction::MovDnW2 => Some(format!(
                "Moves the top stack word to the {} position.",
                ordinal(2)
            )),
            Instruction::MovDnW3 => Some(format!(
                "Moves the top stack word to the {} position.",
                ordinal(3)
            )),

            // === Stack position: dup ===
            Instruction::Dup0 => Some("Pushes a copy of the top stack item.".into()),
            Instruction::Dup1 => Some(format!("Pushes a copy of the {} stack item.", ordinal(1))),
            Instruction::Dup2 => Some(format!("Pushes a copy of the {} stack item.", ordinal(2))),
            Instruction::Dup3 => Some(format!("Pushes a copy of the {} stack item.", ordinal(3))),
            Instruction::Dup4 => Some(format!("Pushes a copy of the {} stack item.", ordinal(4))),
            Instruction::Dup5 => Some(format!("Pushes a copy of the {} stack item.", ordinal(5))),
            Instruction::Dup6 => Some(format!("Pushes a copy of the {} stack item.", ordinal(6))),
            Instruction::Dup7 => Some(format!("Pushes a copy of the {} stack item.", ordinal(7))),
            Instruction::Dup8 => Some(format!("Pushes a copy of the {} stack item.", ordinal(8))),
            Instruction::Dup9 => Some(format!("Pushes a copy of the {} stack item.", ordinal(9))),
            Instruction::Dup10 => Some(format!("Pushes a copy of the {} stack item.", ordinal(10))),
            Instruction::Dup11 => Some(format!("Pushes a copy of the {} stack item.", ordinal(11))),
            Instruction::Dup12 => Some(format!("Pushes a copy of the {} stack item.", ordinal(12))),
            Instruction::Dup13 => Some(format!("Pushes a copy of the {} stack item.", ordinal(13))),
            Instruction::Dup14 => Some(format!("Pushes a copy of the {} stack item.", ordinal(14))),
            Instruction::Dup15 => Some(format!("Pushes a copy of the {} stack item.", ordinal(15))),

            // === Stack position: dupw ===
            Instruction::DupW0 => Some("Pushes a copy of the top stack word.".into()),
            Instruction::DupW1 => Some(format!("Pushes a copy of the {} stack word.", ordinal(1))),
            Instruction::DupW2 => Some(format!("Pushes a copy of the {} stack word.", ordinal(2))),
            Instruction::DupW3 => Some(format!("Pushes a copy of the {} stack word.", ordinal(3))),

            // === Stack position: swap ===
            Instruction::Swap1 => Some("Swaps the top two stack items.".into()),
            Instruction::Swap2 => Some(format!("Swaps the top item with the {} item.", ordinal(2))),
            Instruction::Swap3 => Some(format!("Swaps the top item with the {} item.", ordinal(3))),
            Instruction::Swap4 => Some(format!("Swaps the top item with the {} item.", ordinal(4))),
            Instruction::Swap5 => Some(format!("Swaps the top item with the {} item.", ordinal(5))),
            Instruction::Swap6 => Some(format!("Swaps the top item with the {} item.", ordinal(6))),
            Instruction::Swap7 => Some(format!("Swaps the top item with the {} item.", ordinal(7))),
            Instruction::Swap8 => Some(format!("Swaps the top item with the {} item.", ordinal(8))),
            Instruction::Swap9 => Some(format!("Swaps the top item with the {} item.", ordinal(9))),
            Instruction::Swap10 => {
                Some(format!("Swaps the top item with the {} item.", ordinal(10)))
            }
            Instruction::Swap11 => {
                Some(format!("Swaps the top item with the {} item.", ordinal(11)))
            }
            Instruction::Swap12 => {
                Some(format!("Swaps the top item with the {} item.", ordinal(12)))
            }
            Instruction::Swap13 => {
                Some(format!("Swaps the top item with the {} item.", ordinal(13)))
            }
            Instruction::Swap14 => {
                Some(format!("Swaps the top item with the {} item.", ordinal(14)))
            }
            Instruction::Swap15 => {
                Some(format!("Swaps the top item with the {} item.", ordinal(15)))
            }

            // === Stack position: swapw ===
            Instruction::SwapW1 => Some("Swaps the top two stack words.".into()),
            Instruction::SwapW2 => {
                Some(format!("Swaps the top word with the {} word.", ordinal(2)))
            }
            Instruction::SwapW3 => {
                Some(format!("Swaps the top word with the {} word.", ordinal(3)))
            }

            // === Push instructions ===
            Instruction::Push(imm) => Some(format!(
                "Pushes {} onto the stack.",
                format_push_immediate(imm)
            )),
            Instruction::PushFeltList(values) => Some(format!(
                "Pushes the field elements {} onto the stack.",
                format_felt_list(values)
            )),

            // === Immediate arithmetic ===
            Instruction::AddImm(v) => {
                Some(format!("c = (a + {}) mod p.", format_felt_immediate(v)))
            }
            Instruction::SubImm(v) => {
                Some(format!("c = (a - {}) mod p.", format_felt_immediate(v)))
            }
            Instruction::MulImm(v) => {
                Some(format!("c = (a * {}) mod p.", format_felt_immediate(v)))
            }
            Instruction::DivImm(v) => Some(format!(
                "c = (a * {}^-1) mod p. Fails if b = 0.",
                format_felt_immediate(v)
            )),
            Instruction::ExpImm(v) => Some(format!("c = a^{}.", format_felt_immediate(v))),
            Instruction::EqImm(v) => Some(format!("Compare if a = {}.", format_felt_immediate(v))),
            Instruction::NeqImm(v) => {
                Some(format!("Compare if a != {}.", format_felt_immediate(v)))
            }

            // === U32 immediate operations ===
            Instruction::U32WrappingAddImm(v) => {
                Some(format!("c = (a + {}) mod 2^32.", format_u32_immediate(v)))
            }
            Instruction::U32OverflowingAddImm(v) => Some(format!(
                "c = (a + {}) mod 2^32, with overflow flag.",
                format_u32_immediate(v)
            )),
            Instruction::U32WrappingSubImm(v) => {
                Some(format!("c = (a - {}) mod 2^32.", format_u32_immediate(v)))
            }
            Instruction::U32OverflowingSubImm(v) => Some(format!(
                "c = (a - {}) mod 2^32, with borrow flag.",
                format_u32_immediate(v)
            )),
            Instruction::U32WrappingMulImm(v) => {
                Some(format!("c = (a * {}) mod 2^32.", format_u32_immediate(v)))
            }
            Instruction::U32OverflowingMulImm(v) => Some(format!(
                "c = (a * {}) mod 2^32, d = high bits.",
                format_u32_immediate(v)
            )),
            Instruction::U32DivImm(v) => Some(format!(
                "c = floor(a / {}). Fails if b = 0.",
                format_u32_immediate(v)
            )),
            Instruction::U32ModImm(v) => Some(format!(
                "c = a mod {}. Fails if b = 0.",
                format_u32_immediate(v)
            )),
            Instruction::U32DivModImm(v) => Some(format!(
                "c = floor(a / {}), d = a mod {}. Fails if b = 0.",
                format_u32_immediate(v),
                format_u32_immediate(v)
            )),
            Instruction::U32ShlImm(v) => {
                Some(format!("c = (a * 2^{}) mod 2^32.", format_u8_immediate(v)))
            }
            Instruction::U32ShrImm(v) => {
                Some(format!("c = floor(a / 2^{}).", format_u8_immediate(v)))
            }
            Instruction::U32RotlImm(v) => {
                Some(format!("Rotate left by {} bits.", format_u8_immediate(v)))
            }
            Instruction::U32RotrImm(v) => {
                Some(format!("Rotate right by {} bits.", format_u8_immediate(v)))
            }

            // === Memory operations ===
            Instruction::MemLoadImm(addr) => Some(format!(
                "v = mem[{}]. Pushes element from memory.",
                format_u32_immediate(addr)
            )),
            Instruction::MemLoadWBeImm(addr) => Some(format!(
                "A = mem[{}..{}+3]. Loads big-endian word.",
                format_u32_immediate(addr),
                format_u32_immediate(addr)
            )),
            Instruction::MemLoadWLeImm(addr) => Some(format!(
                "A = mem[{}..{}+3]. Loads little-endian word.",
                format_u32_immediate(addr),
                format_u32_immediate(addr)
            )),
            Instruction::MemStoreImm(addr) => Some(format!(
                "mem[{}] = v. Stores element to memory.",
                format_u32_immediate(addr)
            )),
            Instruction::MemStoreWBeImm(addr) => Some(format!(
                "mem[{}..{}+3] = A. Stores big-endian word.",
                format_u32_immediate(addr),
                format_u32_immediate(addr)
            )),
            Instruction::MemStoreWLeImm(addr) => Some(format!(
                "mem[{}..{}+3] = A. Stores little-endian word.",
                format_u32_immediate(addr),
                format_u32_immediate(addr)
            )),

            // === Local memory operations ===
            Instruction::LocLoad(idx) => Some(format!(
                "v = local[{}]. Pushes element from local memory.",
                format_u16_immediate(idx)
            )),
            Instruction::LocLoadWBe(idx) => Some(format!(
                "A = local[{}..{}+3]. Loads big-endian word.",
                format_u16_immediate(idx),
                format_u16_immediate(idx)
            )),
            Instruction::LocLoadWLe(idx) => Some(format!(
                "A = local[{}..{}+3]. Loads little-endian word.",
                format_u16_immediate(idx),
                format_u16_immediate(idx)
            )),
            Instruction::LocStore(idx) => Some(format!(
                "local[{}] = v. Stores element to local memory.",
                format_u16_immediate(idx)
            )),
            Instruction::LocStoreWBe(idx) => Some(format!(
                "local[{}..{}+3] = A. Stores big-endian word.",
                format_u16_immediate(idx),
                format_u16_immediate(idx)
            )),
            Instruction::LocStoreWLe(idx) => Some(format!(
                "local[{}..{}+3] = A. Stores little-endian word.",
                format_u16_immediate(idx),
                format_u16_immediate(idx)
            )),

            // === Locaddr ===
            Instruction::Locaddr(idx) => Some(format!(
                "Pushes address of local memory at index {}.",
                format_u16_immediate(idx)
            )),

            // === Advice stack ===
            Instruction::AdvPush(n) => Some(format!(
                "Pops {} value(s) from advice stack to operand stack.",
                format_u8_immediate(n)
            )),

            // === Emit/Trace ===
            Instruction::EmitImm(v) => Some(format!(
                "Emits event {} to the host.",
                format_felt_immediate(v)
            )),
            Instruction::Trace(v) => Some(format!(
                "Emits trace {} to the host.",
                format_u32_immediate(v)
            )),

            // All other instructions use static hints
            _ => None,
        }
    }
}

// ================================================================================================
// Helper functions
// ================================================================================================

/// Format ordinal numbers: 1st, 2nd, 3rd, 4th, ...
pub fn ordinal(n: u8) -> String {
    match n {
        1 => "1st".into(),
        2 => "2nd".into(),
        3 => "3rd".into(),
        n if n % 10 == 1 && n != 11 => format!("{n}st"),
        n if n % 10 == 2 && n != 12 => format!("{n}nd"),
        n if n % 10 == 3 && n != 13 => format!("{n}rd"),
        n => format!("{n}th"),
    }
}

/// Format a list of field elements, preserving their original representation.
pub fn format_felt_list(values: &[Felt]) -> String {
    let formatted: Vec<String> = values.iter().map(|v| format!("{v}")).collect();
    format!("[{}]", formatted.join(", "))
}

/// Format a push immediate value, preserving original representation.
pub fn format_push_immediate(imm: &Immediate<PushValue>) -> String {
    format!("{imm}")
}

/// Format a field element immediate, preserving original representation.
pub fn format_felt_immediate(imm: &Immediate<Felt>) -> String {
    format!("{imm}")
}

/// Format a u32 immediate, preserving original representation.
pub fn format_u32_immediate(imm: &Immediate<u32>) -> String {
    format!("{imm}")
}

/// Format a u16 immediate, preserving original representation.
pub fn format_u16_immediate(imm: &Immediate<u16>) -> String {
    format!("{imm}")
}

/// Format a u8 immediate, preserving original representation.
pub fn format_u8_immediate(imm: &Immediate<u8>) -> String {
    format!("{imm}")
}

// ================================================================================================
// Tests
// ================================================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use miden_assembly_syntax::parser::IntValue;
    use miden_debug_types::Span;

    // === Tests for ordinal function ===

    #[test]
    fn ordinal_first_three() {
        assert_eq!(ordinal(1), "1st");
        assert_eq!(ordinal(2), "2nd");
        assert_eq!(ordinal(3), "3rd");
    }

    #[test]
    fn ordinal_regular_th() {
        assert_eq!(ordinal(4), "4th");
        assert_eq!(ordinal(5), "5th");
        assert_eq!(ordinal(10), "10th");
        assert_eq!(ordinal(15), "15th");
    }

    #[test]
    fn ordinal_teens() {
        assert_eq!(ordinal(11), "11th");
        assert_eq!(ordinal(12), "12th");
        assert_eq!(ordinal(13), "13th");
    }

    // === Tests for ToDescription trait ===

    #[test]
    fn hint_movup() {
        let hint = Instruction::MovUp3.to_description();
        assert_eq!(hint, Some("Moves the 3rd stack item to the top.".into()));

        let hint = Instruction::MovUp15.to_description();
        assert_eq!(hint, Some("Moves the 15th stack item to the top.".into()));
    }

    #[test]
    fn hint_movupw() {
        let hint = Instruction::MovUpW2.to_description();
        assert_eq!(hint, Some("Moves the 2nd stack word to the top.".into()));

        let hint = Instruction::MovUpW3.to_description();
        assert_eq!(hint, Some("Moves the 3rd stack word to the top.".into()));
    }

    #[test]
    fn hint_movdn() {
        let hint = Instruction::MovDn4.to_description();
        assert_eq!(
            hint,
            Some("Moves the top stack item to the 4th position.".into())
        );
    }

    #[test]
    fn hint_dup() {
        let hint = Instruction::Dup0.to_description();
        assert_eq!(hint, Some("Pushes a copy of the top stack item.".into()));

        let hint = Instruction::Dup5.to_description();
        assert_eq!(hint, Some("Pushes a copy of the 5th stack item.".into()));
    }

    #[test]
    fn hint_dupw() {
        let hint = Instruction::DupW0.to_description();
        assert_eq!(hint, Some("Pushes a copy of the top stack word.".into()));

        let hint = Instruction::DupW2.to_description();
        assert_eq!(hint, Some("Pushes a copy of the 2nd stack word.".into()));
    }

    #[test]
    fn hint_swap() {
        let hint = Instruction::Swap1.to_description();
        assert_eq!(hint, Some("Swaps the top two stack items.".into()));

        let hint = Instruction::Swap7.to_description();
        assert_eq!(hint, Some("Swaps the top item with the 7th item.".into()));
    }

    #[test]
    fn hint_swapw() {
        let hint = Instruction::SwapW1.to_description();
        assert_eq!(hint, Some("Swaps the top two stack words.".into()));

        let hint = Instruction::SwapW3.to_description();
        assert_eq!(hint, Some("Swaps the top word with the 3rd word.".into()));
    }

    #[test]
    fn hint_push_felt_list() {
        let values = vec![Felt::new(0), Felt::new(0)];
        let hint = Instruction::PushFeltList(values).to_description();
        assert_eq!(
            hint,
            Some("Pushes the field elements [0, 0] onto the stack.".into())
        );
    }

    #[test]
    fn hint_push_felt_list_multiple() {
        let values = vec![Felt::new(1), Felt::new(2), Felt::new(3), Felt::new(4)];
        let hint = Instruction::PushFeltList(values).to_description();
        assert_eq!(
            hint,
            Some("Pushes the field elements [1, 2, 3, 4] onto the stack.".into())
        );
    }

    #[test]
    fn hint_push_single() {
        let imm = Immediate::from(Span::unknown(PushValue::Int(IntValue::U8(42))));
        let hint = Instruction::Push(imm).to_description();
        assert_eq!(hint, Some("Pushes 42 onto the stack.".into()));
    }

    #[test]
    fn hint_add_imm() {
        let imm = Immediate::from(Felt::new(5));
        let hint = Instruction::AddImm(imm).to_description();
        assert_eq!(hint, Some("c = (a + 5) mod p.".into()));
    }

    #[test]
    fn hint_mem_load_imm() {
        let imm = Immediate::from(42u32);
        let hint = Instruction::MemLoadImm(imm).to_description();
        assert_eq!(
            hint,
            Some("v = mem[42]. Pushes element from memory.".into())
        );
    }

    #[test]
    fn hint_loc_load() {
        let imm = Immediate::from(3u16);
        let hint = Instruction::LocLoad(imm).to_description();
        assert_eq!(
            hint,
            Some("v = local[3]. Pushes element from local memory.".into())
        );
    }

    #[test]
    fn hint_adv_push() {
        let imm = Immediate::from(4u8);
        let hint = Instruction::AdvPush(imm).to_description();
        assert_eq!(
            hint,
            Some("Pops 4 value(s) from advice stack to operand stack.".into())
        );
    }

    #[test]
    fn hint_fallback_to_none() {
        // Instructions without dynamic hints should return None
        let hint = Instruction::Add.to_description();
        assert_eq!(hint, None);

        let hint = Instruction::Nop.to_description();
        assert_eq!(hint, None);

        let hint = Instruction::Hash.to_description();
        assert_eq!(hint, None);
    }

    // === Tests for format functions ===

    #[test]
    fn format_felt_list_empty() {
        let values: Vec<Felt> = vec![];
        assert_eq!(format_felt_list(&values), "[]");
    }

    #[test]
    fn format_felt_list_single() {
        let values = vec![Felt::new(42)];
        assert_eq!(format_felt_list(&values), "[42]");
    }

    #[test]
    fn format_felt_list_multiple() {
        let values = vec![Felt::new(1), Felt::new(2), Felt::new(3)];
        assert_eq!(format_felt_list(&values), "[1, 2, 3]");
    }
}
