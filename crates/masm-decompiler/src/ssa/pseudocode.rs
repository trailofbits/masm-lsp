use super::{context::SsaContext, ids::SsaId};

/// A segment of pseudocode that may contain SSA ID references.
#[derive(Debug, Clone)]
pub enum PseudocodeSegment {
    /// Literal text (e.g., " = ", " + ", "mem[")
    Literal(String),
    /// Reference to an SSA value that will be resolved to a display name
    SsaRef(SsaId),
}

/// A pseudocode template that can be resolved to a final string.
#[derive(Debug, Clone)]
pub struct PseudocodeTemplate {
    pub(crate) segments: Vec<PseudocodeSegment>,
}

impl PseudocodeTemplate {
    /// Create a new empty template.
    pub fn new() -> Self {
        Self {
            segments: Vec::new(),
        }
    }

    /// Add a literal string segment.
    pub fn literal(mut self, s: &str) -> Self {
        self.segments
            .push(PseudocodeSegment::Literal(s.to_string()));
        self
    }

    /// Add an SSA reference segment.
    pub fn ssa_ref(mut self, id: SsaId) -> Self {
        self.segments.push(PseudocodeSegment::SsaRef(id));
        self
    }

    /// Resolve the template to a final string using the SSA context.
    pub fn resolve(&self, ctx: &SsaContext) -> String {
        self.segments
            .iter()
            .map(|seg| match seg {
                PseudocodeSegment::Literal(s) => s.clone(),
                PseudocodeSegment::SsaRef(id) => ctx.get_display_name(*id).to_string(),
            })
            .collect()
    }

    /// Resolve the template with an override function for specific SSA IDs.
    ///
    /// The override function is called for each SSA reference. If it returns
    /// `Some(name)`, that name is used instead of the default display name.
    /// This is used for parametric variable naming at resolution time.
    pub fn resolve_with_overrides<F>(&self, ctx: &SsaContext, override_fn: F) -> String
    where
        F: Fn(SsaId) -> Option<String>,
    {
        self.segments
            .iter()
            .map(|seg| match seg {
                PseudocodeSegment::Literal(s) => s.clone(),
                PseudocodeSegment::SsaRef(id) => {
                    // First check for override, then fall back to context
                    override_fn(*id).unwrap_or_else(|| ctx.get_display_name(*id).to_string())
                }
            })
            .collect()
    }

    /// Check if the template is empty.
    pub fn is_empty(&self) -> bool {
        self.segments.is_empty()
    }

    /// Get all SSA IDs referenced in this template.
    pub fn ssa_ids(&self) -> impl Iterator<Item = SsaId> + '_ {
        self.segments.iter().filter_map(|seg| match seg {
            PseudocodeSegment::SsaRef(id) => Some(*id),
            PseudocodeSegment::Literal(_) => None,
        })
    }
}

impl Default for PseudocodeTemplate {
    fn default() -> Self {
        Self::new()
    }
}

/// Builder for creating pseudocode templates with a fluent API.
///
/// Example:
/// ```ignore
/// let template = PseudocodeBuilder::new()
///     .var(result_id)
///     .text(" = ")
///     .var(left_id)
///     .text(" + ")
///     .var(right_id)
///     .build();
/// ```
#[derive(Debug, Default)]
pub struct PseudocodeBuilder {
    template: PseudocodeTemplate,
}

impl PseudocodeBuilder {
    /// Create a new builder.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add literal text.
    pub fn text(&mut self, s: &str) -> &mut Self {
        self.template
            .segments
            .push(PseudocodeSegment::Literal(s.to_string()));
        self
    }

    /// Add an SSA variable reference.
    pub fn var<V: Into<SsaId>>(&mut self, id: V) -> &mut Self {
        self.template
            .segments
            .push(PseudocodeSegment::SsaRef(id.into()));
        self
    }

    /// Build the template.
    pub fn build(self) -> PseudocodeTemplate {
        self.template
    }
}
