//! Domain types for the uninitialized-local-read analysis.
//!
//! These types model the local memory frame of a MASM procedure and track
//! which frame cells may be accessed by memory operations. They form the
//! foundation for the transfer functions and intraprocedural analysis.

use std::collections::BTreeMap;
use std::fmt;

// ---------------------------------------------------------------------------
// CellInterval
// ---------------------------------------------------------------------------

/// A closed interval `[start, end]` of frame cell indices, inclusive on both
/// ends.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CellInterval {
    /// First cell index in the interval (inclusive).
    pub start: u16,
    /// Last cell index in the interval (inclusive).
    pub end: u16,
}

impl CellInterval {
    /// Create a new interval. Panics if `start > end`.
    pub fn new(start: u16, end: u16) -> Self {
        assert!(start <= end, "CellInterval: start ({start}) > end ({end})");
        Self { start, end }
    }

    /// Number of cells in this interval.
    pub fn len(&self) -> usize {
        (self.end - self.start) as usize + 1
    }

    /// Whether this interval contains the given cell.
    pub fn contains(&self, cell: u16) -> bool {
        cell >= self.start && cell <= self.end
    }

    /// Whether two intervals overlap or are directly adjacent (and thus can
    /// be merged into one).
    pub fn touches(&self, other: &Self) -> bool {
        // Adjacent means one ends exactly 1 before the other starts.
        // Overlap means ranges intersect.
        self.start <= other.end.saturating_add(1) && other.start <= self.end.saturating_add(1)
    }

    /// Merge two touching intervals into one. Panics if they don't touch.
    pub fn merge(&self, other: &Self) -> Self {
        debug_assert!(self.touches(other));
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

// ---------------------------------------------------------------------------
// CellSet
// ---------------------------------------------------------------------------

/// A set of frame cell indices, stored as a sorted vector of disjoint,
/// non-adjacent [`CellInterval`]s.
///
/// All mutating operations preserve the invariant that intervals are sorted
/// by `start` and are neither overlapping nor adjacent.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CellSet {
    /// Sorted, disjoint, non-adjacent intervals.
    intervals: Vec<CellInterval>,
}

impl CellSet {
    /// The empty cell set.
    pub fn empty() -> Self {
        Self {
            intervals: Vec::new(),
        }
    }

    /// A cell set containing exactly one cell.
    pub fn singleton(cell: u16) -> Self {
        Self {
            intervals: vec![CellInterval::new(cell, cell)],
        }
    }

    /// A cell set containing all cells in the closed range `[start, end]`.
    ///
    /// Returns the empty set if `start > end`.
    pub fn closed_range(start: u16, end: u16) -> Self {
        if start > end {
            return Self::empty();
        }
        Self {
            intervals: vec![CellInterval::new(start, end)],
        }
    }

    /// Whether the set contains no cells.
    pub fn is_empty(&self) -> bool {
        self.intervals.is_empty()
    }

    /// Whether the set contains the given cell.
    pub fn contains(&self, cell: u16) -> bool {
        self.intervals.iter().any(|iv| iv.contains(cell))
    }

    /// The total number of individual cells in the set.
    pub fn len(&self) -> usize {
        self.intervals.iter().map(|iv| iv.len()).sum()
    }

    /// Return the union of two cell sets.
    pub fn union(&self, other: &CellSet) -> CellSet {
        if self.is_empty() {
            return other.clone();
        }
        if other.is_empty() {
            return self.clone();
        }

        // Merge-sort the two interval lists, then coalesce.
        let mut merged = Vec::with_capacity(self.intervals.len() + other.intervals.len());
        let (mut i, mut j) = (0, 0);
        while i < self.intervals.len() && j < other.intervals.len() {
            if self.intervals[i].start <= other.intervals[j].start {
                merged.push(self.intervals[i]);
                i += 1;
            } else {
                merged.push(other.intervals[j]);
                j += 1;
            }
        }
        merged.extend_from_slice(&self.intervals[i..]);
        merged.extend_from_slice(&other.intervals[j..]);

        CellSet {
            intervals: coalesce(merged),
        }
    }

    /// Return the intersection of two cell sets.
    pub fn intersection(&self, other: &CellSet) -> CellSet {
        let mut result = Vec::new();
        let (mut i, mut j) = (0, 0);
        while i < self.intervals.len() && j < other.intervals.len() {
            let a = &self.intervals[i];
            let b = &other.intervals[j];

            let lo = a.start.max(b.start);
            let hi = a.end.min(b.end);
            if lo <= hi {
                result.push(CellInterval::new(lo, hi));
            }

            // Advance the interval that ends first.
            if a.end < b.end {
                i += 1;
            } else {
                j += 1;
            }
        }

        CellSet { intervals: result }
    }

    /// Return `self \ other` (set difference).
    pub fn difference(&self, other: &CellSet) -> CellSet {
        if other.is_empty() {
            return self.clone();
        }

        let mut result = Vec::new();
        let mut j = 0;
        for iv in &self.intervals {
            let mut remaining_start = iv.start;
            let remaining_end = iv.end;

            // Skip other-intervals that end before the current interval starts.
            while j < other.intervals.len() && other.intervals[j].end < remaining_start {
                j += 1;
            }

            // Walk through other-intervals that overlap.
            let mut k = j;
            while k < other.intervals.len() && other.intervals[k].start <= remaining_end {
                let sub = &other.intervals[k];
                if sub.start > remaining_start {
                    result.push(CellInterval::new(remaining_start, sub.start - 1));
                }
                remaining_start = sub.end.saturating_add(1);
                k += 1;
            }

            // Anything left after the last subtracted interval.
            if remaining_start <= remaining_end {
                result.push(CellInterval::new(remaining_start, remaining_end));
            }
        }

        CellSet { intervals: result }
    }

    /// Shift every cell index up by `offset` (saturating at `u16::MAX`).
    pub fn shift_by(&self, offset: u16) -> CellSet {
        if offset == 0 {
            return self.clone();
        }
        let intervals = self
            .intervals
            .iter()
            .map(|iv| {
                CellInterval::new(
                    iv.start.saturating_add(offset),
                    iv.end.saturating_add(offset),
                )
            })
            .collect();
        // Saturation may merge intervals at the u16::MAX boundary.
        CellSet {
            intervals: coalesce(intervals),
        }
    }

    /// Remove all cells with indices `>= frame_size`.
    pub fn clip_to_frame(&self, frame_size: u16) -> CellSet {
        if frame_size == 0 {
            return CellSet::empty();
        }
        let limit = frame_size - 1;
        let mut result = Vec::new();
        for iv in &self.intervals {
            if iv.start > limit {
                break;
            }
            result.push(CellInterval::new(iv.start, iv.end.min(limit)));
        }
        CellSet { intervals: result }
    }

    /// Iterate over the individual cell indices in the set.
    pub fn iter_cells(&self) -> impl Iterator<Item = u16> + '_ {
        self.intervals.iter().flat_map(|iv| iv.start..=iv.end)
    }

    /// Return a reference to the underlying intervals.
    pub fn intervals(&self) -> &[CellInterval] {
        &self.intervals
    }
}

impl fmt::Display for CellSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{")?;
        for (i, iv) in self.intervals.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            if iv.start == iv.end {
                write!(f, "{}", iv.start)?;
            } else {
                write!(f, "{}..={}", iv.start, iv.end)?;
            }
        }
        write!(f, "}}")
    }
}

/// Coalesce a list of intervals that is already sorted by `start`.
///
/// Adjacent and overlapping intervals are merged into single intervals.
fn coalesce(sorted: Vec<CellInterval>) -> Vec<CellInterval> {
    let mut result: Vec<CellInterval> = Vec::with_capacity(sorted.len());
    for iv in sorted {
        if let Some(last) = result.last_mut() {
            if last.touches(&iv) {
                *last = last.merge(&iv);
                continue;
            }
        }
        result.push(iv);
    }
    result
}

// ---------------------------------------------------------------------------
// FrameLayout
// ---------------------------------------------------------------------------

/// Layout of a procedure's local memory frame.
///
/// `declared` is the number of locals the procedure declares, while `aligned`
/// is `declared` rounded up to the next multiple of 4 (the Miden VM allocates
/// locals in 4-cell words). A procedure with 0 declared locals has
/// `aligned == 0`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FrameLayout {
    /// Number of locals declared by the procedure.
    pub declared: u16,
    /// Size of the frame after alignment to a 4-cell boundary.
    pub aligned: u16,
}

impl FrameLayout {
    /// Create a frame layout for a procedure declaring `num_locals` locals.
    ///
    /// The aligned size is `(num_locals + 3) & !3` (next multiple of 4),
    /// except that 0 declared locals produces an aligned size of 0.
    pub fn new(num_locals: u16) -> Self {
        let aligned = if num_locals == 0 {
            0
        } else {
            (num_locals + 3) & !3
        };
        Self {
            declared: num_locals,
            aligned,
        }
    }

    /// The set of all cells in the aligned frame (indices `0..aligned-1`).
    ///
    /// Empty if `aligned == 0`.
    pub fn all_cells(&self) -> CellSet {
        if self.aligned == 0 {
            CellSet::empty()
        } else {
            CellSet::closed_range(0, self.aligned - 1)
        }
    }
}

// ---------------------------------------------------------------------------
// AddrAbs
// ---------------------------------------------------------------------------

/// Abstract address that may refer to local frame cells and/or non-local
/// (global) memory.
///
/// An address where `maybe_nonlocal` is true indicates that the concrete
/// address might fall outside the local frame, meaning we cannot be certain
/// which (if any) local cell is affected.
///
/// The `input_cells` field tracks which callee input positions an address was
/// derived from during summary inference. For each input position `i`, the
/// associated [`CellSet`] describes which cells (relative to input `i`) this
/// address may target. This is used to build exec summaries: when a callee
/// reads or writes through an input-derived address, the summary records the
/// effect relative to the input position, so the caller can later map it back
/// to its own local frame.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AddrAbs {
    /// Set of local frame cells this address may point to.
    pub local_cells: CellSet,
    /// Whether this address might point to non-local memory.
    pub maybe_nonlocal: bool,
    /// For summary inference: which cells this address targets relative to
    /// each callee input position. Empty during normal (non-summary) analysis.
    pub input_cells: BTreeMap<usize, CellSet>,
}

impl AddrAbs {
    /// An address known to point to exactly the given local cells.
    pub fn exact_local(cells: CellSet) -> Self {
        Self {
            local_cells: cells,
            maybe_nonlocal: false,
            input_cells: BTreeMap::new(),
        }
    }

    /// Join (widen) two abstract addresses.
    ///
    /// The result conservatively over-approximates both addresses: local cells
    /// are unioned, the non-local flag is OR-ed, and input cells are merged
    /// per input position.
    pub fn join(&self, other: &AddrAbs) -> AddrAbs {
        let mut input_cells = self.input_cells.clone();
        for (idx, cells) in &other.input_cells {
            input_cells
                .entry(*idx)
                .and_modify(|existing| *existing = existing.union(cells))
                .or_insert_with(|| cells.clone());
        }
        AddrAbs {
            local_cells: self.local_cells.union(&other.local_cells),
            maybe_nonlocal: self.maybe_nonlocal || other.maybe_nonlocal,
            input_cells,
        }
    }
}

// ---------------------------------------------------------------------------
// AccessFootprint
// ---------------------------------------------------------------------------

/// The set of frame cells an operation may and must access.
///
/// `may_cells` is an over-approximation: every cell that *could* be touched.
/// `must_cells` is an under-approximation: every cell that is *definitely*
/// touched regardless of which concrete address is chosen at runtime.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AccessFootprint {
    /// Cells the operation may access (over-approximation).
    pub may_cells: CellSet,
    /// Cells the operation must access (under-approximation).
    pub must_cells: CellSet,
}

impl AccessFootprint {
    /// Compute the footprint for a scalar (single-cell) memory access.
    ///
    /// If the address points to exactly one local cell and cannot be non-local,
    /// both `may` and `must` are that single cell. Otherwise `must` is empty
    /// because we cannot guarantee which cell is actually accessed.
    pub fn for_scalar(addr: &AddrAbs) -> Self {
        let may_cells = addr.local_cells.clone();
        let must_cells = if !addr.maybe_nonlocal && addr.local_cells.len() == 1 {
            addr.local_cells.clone()
        } else {
            CellSet::empty()
        };
        Self {
            may_cells,
            must_cells,
        }
    }

    /// Compute the footprint for a word (4-cell) memory access.
    ///
    /// A word access at base cell `b` touches cells `b, b+1, b+2, b+3`.
    /// The `may_cells` is the union of all such 4-cell windows for every
    /// possible base cell in the address. The `must_cells` is the intersection
    /// of all windows when the address is not maybe-nonlocal; otherwise empty.
    pub fn for_word(addr: &AddrAbs) -> Self {
        if addr.local_cells.is_empty() {
            return Self {
                may_cells: CellSet::empty(),
                must_cells: CellSet::empty(),
            };
        }

        let mut may_union = CellSet::empty();
        let mut must_inter: Option<CellSet> = None;

        for base in addr.local_cells.iter_cells() {
            let window = CellSet::closed_range(base, base.saturating_add(3));
            may_union = may_union.union(&window);
            must_inter = Some(match must_inter {
                None => window,
                Some(acc) => acc.intersection(&window),
            });
        }

        let must_cells = if addr.maybe_nonlocal {
            CellSet::empty()
        } else {
            must_inter.unwrap_or_else(CellSet::empty)
        };

        Self {
            may_cells: may_union,
            must_cells,
        }
    }
}
