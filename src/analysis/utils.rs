//! Shared utility functions for analysis modules.
//!
//! This module provides common helper functions for extracting values from
//! Miden assembly immediates, used across contract inference, stack effects,
//! and loop analysis.

use miden_assembly_syntax::ast::Immediate;

/// Try to extract a u64 value from a push immediate.
///
/// Returns `Some(value)` if the immediate is a single integer value,
/// or `None` if it's a word push or other non-extractable value.
pub fn push_imm_to_u64(imm: &Immediate<miden_assembly_syntax::parser::PushValue>) -> Option<u64> {
    use miden_assembly_syntax::parser::{IntValue, PushValue};

    match imm {
        Immediate::Value(span) => match span.inner() {
            PushValue::Int(int_val) => {
                let val = match int_val {
                    IntValue::U8(v) => *v as u64,
                    IntValue::U16(v) => *v as u64,
                    IntValue::U32(v) => *v as u64,
                    IntValue::Felt(f) => f.as_int(),
                };
                Some(val)
            }
            PushValue::Word(_) => None, // Word pushes multiple values, can't extract single u64
        },
        _ => None,
    }
}

/// Try to extract a u64 value from a Felt immediate.
///
/// This is used for instructions with immediate field element arguments,
/// such as `add.N`, `sub.N`, `eq.N`, etc.
pub fn felt_imm_to_u64(imm: &Immediate<miden_assembly_syntax::Felt>) -> Option<u64> {
    match imm {
        Immediate::Value(span) => Some(span.inner().as_int()),
        _ => None,
    }
}

/// Try to extract a u64 value from a u32 immediate.
///
/// This is used for instructions with u32 immediate arguments.
pub fn u32_imm_to_u64(imm: &Immediate<u32>) -> Option<u64> {
    match imm {
        Immediate::Value(span) => Some(*span.inner() as u64),
        _ => None,
    }
}

/// Try to extract a u64 value from a u8 immediate.
///
/// This is used for instructions with u8 immediate arguments,
/// such as `u32shr.N`, `u32rotl.N`, etc.
pub fn u8_imm_to_u64(imm: &Immediate<u8>) -> Option<u64> {
    match imm {
        Immediate::Value(span) => Some(*span.inner() as u64),
        _ => None,
    }
}

/// Try to extract a u16 value from a u16 immediate.
///
/// This is used for instructions with u16 immediate arguments,
/// such as `loc_load.N`, `loc_store.N`, etc.
pub fn u16_imm_to_u16(imm: &Immediate<u16>) -> Option<u16> {
    match imm {
        Immediate::Value(span) => Some(*span.inner()),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    // Note: Testing these functions requires constructing Immediate values,
    // which depends on internal parser types. Integration tests cover this
    // functionality through the higher-level analysis modules.
}
