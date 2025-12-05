/// Unique identifier for an SSA value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SsaId(pub usize);

impl SsaId {
    /// Create a new SSA ID.
    pub fn new(id: usize) -> Self {
        Self(id)
    }
}

impl From<&SsaId> for SsaId {
    fn from(id: &SsaId) -> Self {
        *id
    }
}

/// The kind of variable an SSA value represents.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VarKind {
    /// Procedure argument at fixed position (a_0, a_1, ...)
    Argument(usize),
    /// Local/intermediate variable (v_0, v_1, ...)
    Local(usize),
    /// Return value (r_0, r_1, ...)
    Return(usize),
}

impl VarKind {
    /// Get the display name for this variable kind.
    pub fn display_name(&self) -> String {
        match self {
            VarKind::Argument(i) => format!("a_{}", i),
            VarKind::Local(i) => format!("v_{}", i),
            VarKind::Return(i) => format!("r_{}", i),
        }
    }
}

/// An SSA value with its definition.
#[derive(Debug, Clone)]
pub struct SsaValue {
    /// Unique identifier for this value.
    pub id: SsaId,
    /// The kind of variable this value represents.
    pub kind: VarKind,
    /// The canonical name to use when displaying this value.
    /// Initially set based on `kind`, but may be updated through phi resolution.
    pub display_name: String,
}

impl SsaValue {
    /// Create a new argument value.
    pub fn argument(id: SsaId, index: usize) -> Self {
        let kind = VarKind::Argument(index);
        Self {
            id,
            display_name: kind.display_name(),
            kind,
        }
    }

    /// Create a new local value.
    pub fn local(id: SsaId, index: usize) -> Self {
        let kind = VarKind::Local(index);
        Self {
            id,
            display_name: kind.display_name(),
            kind,
        }
    }

    /// Create a new return value.
    pub fn return_val(id: SsaId, index: usize) -> Self {
        let kind = VarKind::Return(index);
        Self {
            id,
            display_name: kind.display_name(),
            kind,
        }
    }
}
