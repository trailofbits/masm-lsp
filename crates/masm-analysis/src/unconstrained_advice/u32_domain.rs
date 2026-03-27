//! Sanitization-based abstract domain for intraprocedural `u32` validity.

/// Whether a value is known to satisfy the MASM `u32` precondition.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub(crate) enum U32Validity {
    /// No proof is available that the value is a valid `u32`.
    #[default]
    Unknown,
    /// The value has been validated or produced by a trusted `u32` operation.
    ProvenU32,
}

impl U32Validity {
    /// Join two `u32` validity facts conservatively.
    pub(crate) fn join(self, other: Self) -> Self {
        match (self, other) {
            (Self::ProvenU32, Self::ProvenU32) => Self::ProvenU32,
            _ => Self::Unknown,
        }
    }

    /// Return `true` when the value is proven to be a valid `u32`.
    pub(crate) const fn is_proven(self) -> bool {
        matches!(self, Self::ProvenU32)
    }
}

#[cfg(test)]
mod tests {
    use super::U32Validity;

    #[test]
    fn join_requires_both_sides_to_be_proven() {
        assert_eq!(
            U32Validity::ProvenU32.join(U32Validity::ProvenU32),
            U32Validity::ProvenU32
        );
        assert_eq!(
            U32Validity::ProvenU32.join(U32Validity::Unknown),
            U32Validity::Unknown
        );
        assert_eq!(
            U32Validity::Unknown.join(U32Validity::ProvenU32),
            U32Validity::Unknown
        );
    }
}
