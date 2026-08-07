use crate::{judgment::IfThen, set, Fallible, Map, Set};
use std::{
    collections::BTreeSet,
    fmt::Debug,
    hash::{Hash, Hasher},
    panic::Location,
};

/// The genuine outputs found while evaluating a judgment, together with
/// orthogonal completeness and failure-diagnostic information.
///
/// An incomplete result may contain known proofs or no proofs. In either case,
/// absence from the known output set does not establish non-provability.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[must_use]
pub struct ProvenSet<J> {
    data: ProvenSetData<J>,
    incomplete: Set<IncompleteTree>,
}

#[derive(Clone)]
enum ProvenSetData<J> {
    Failure(Box<FailedJudgment>),
    Empty,
    Success(Map<J, ProofTree>),
}

/// Why a consuming operation could not produce a complete logical answer.
///
/// The incomplete variant retains the entire partial result, including every
/// genuine proof, so callers can inspect or propagate it without losing data.
#[derive(Debug)]
pub enum ProvenSetError<J> {
    Failed(Box<FailedJudgment>),
    Incomplete(IncompleteProvenSet<J>),
}

/// An incomplete typed result paired with its type-erased error-chain marker.
///
/// The fields are intentionally private so the marker cannot disagree with the
/// frontiers in the typed result.
#[derive(Debug)]
pub struct IncompleteProvenSet<J> {
    result: ProvenSet<J>,
    source: IncompleteError,
}

impl<J> IncompleteProvenSet<J> {
    /// Borrow the typed partial result, including every genuine proof found.
    pub fn result(&self) -> &ProvenSet<J> {
        &self.result
    }

    /// Recover the typed partial result, including every genuine proof found.
    pub fn into_result(self) -> ProvenSet<J> {
        self.result
    }
}

impl<J> ProvenSetError<J> {
    fn incomplete(result: ProvenSet<J>) -> Self {
        let source = IncompleteError::from_result(&result);
        Self::Incomplete(IncompleteProvenSet { result, source })
    }

    /// Returns the complete-failure diagnostic, if this was a complete failure.
    pub fn as_failed(&self) -> Option<&FailedJudgment> {
        match self {
            Self::Failed(failed) => Some(failed),
            Self::Incomplete(_) => None,
        }
    }

    /// Recover the partial result when evaluation was incomplete.
    pub fn into_incomplete(self) -> Option<ProvenSet<J>> {
        match self {
            Self::Failed(_) => None,
            Self::Incomplete(incomplete) => Some(incomplete.into_result()),
        }
    }

    /// Erase the logical output type while preserving completion metadata in
    /// an [`anyhow::Error`].
    ///
    /// This is intended for legacy adapters that return [`crate::Fallible`].
    /// Generated judgment premises recognize the erased incomplete marker and
    /// propagate its frontiers instead of treating it as an ordinary failure.
    pub fn into_anyhow(self) -> anyhow::Error {
        match self {
            Self::Failed(failed) => anyhow::Error::new(*failed),
            Self::Incomplete(incomplete) => anyhow::Error::new(incomplete.source),
        }
    }

    /// Format the most useful leaf-level diagnostic available.
    pub fn format_leaves(&self) -> String
    where
        J: Debug,
    {
        match self {
            Self::Failed(failed) => failed.format_leaves(),
            Self::Incomplete(incomplete) => incomplete.result.to_string(),
        }
    }
}

impl<J: Debug> std::fmt::Display for ProvenSetError<J> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Failed(_) => f.write_str("judgment evaluation failed"),
            Self::Incomplete(_) => f.write_str("judgment evaluation was incomplete"),
        }
    }
}

impl<J: Debug> std::error::Error for ProvenSetError<J> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Failed(failed) => Some(failed.as_ref()),
            Self::Incomplete(incomplete) => Some(&incomplete.source),
        }
    }
}

/// Type-erased completion metadata transported through an `anyhow::Error`.
///
/// This exists for hand-written adapters whose return type cannot carry a
/// typed [`ProvenSet`]. Judgment-macro plumbing recognizes it and restores the
/// incomplete frontiers at the next premise boundary.
#[derive(Clone, Debug)]
#[doc(hidden)]
pub struct IncompleteError {
    incomplete: Set<IncompleteTree>,
    failure: Option<Box<FailedJudgment>>,
}

impl IncompleteError {
    fn from_result<J>(result: &ProvenSet<J>) -> Self {
        let failure = match &result.data {
            ProvenSetData::Failure(failure) => Some(failure.clone()),
            ProvenSetData::Empty | ProvenSetData::Success(_) => None,
        };
        Self {
            incomplete: result.incomplete.clone(),
            failure,
        }
    }

    /// Recover an erased incomplete marker from an anyhow context chain.
    pub fn from_anyhow(error: &anyhow::Error) -> Option<&Self> {
        error
            .downcast_ref::<Self>()
            .or_else(|| error.chain().find_map(|cause| cause.downcast_ref::<Self>()))
    }

    /// The frontiers that must propagate to the enclosing judgment.
    pub fn incomplete_frontiers(&self) -> &Set<IncompleteTree> {
        &self.incomplete
    }

    /// A complete-failure diagnostic from alternative branches, if present.
    pub fn failure(&self) -> Option<&FailedJudgment> {
        self.failure.as_deref()
    }
}

impl std::fmt::Display for IncompleteError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(failure) = &self.failure {
            writeln!(f, "{failure}")?;
        }
        writeln!(f, "judgment evaluation was incomplete:")?;
        for frontier in &self.incomplete {
            writeln!(f, "  {frontier}")?;
        }
        Ok(())
    }
}

impl std::error::Error for IncompleteError {}

impl<J> From<ProvenSetData<J>> for ProvenSet<J> {
    fn from(data: ProvenSetData<J>) -> Self {
        ProvenSet {
            data,
            incomplete: Set::new(),
        }
    }
}

impl<J> From<FailedJudgment> for ProvenSet<J> {
    fn from(failure: FailedJudgment) -> Self {
        ProvenSetData::<J>::Failure(Box::new(failure)).into()
    }
}

impl<J: Ord + Debug + Clone> ProvenSet<J> {
    /// Creates a judgment set with a single item that was successfully proven.
    pub fn singleton(item: Proven<J>) -> Self {
        Self::proven(std::iter::once(item).collect())
    }

    /// Creates a judgment set with a set of `T` items that were successfully proven.
    /// The set should be non-empty.
    pub fn proven(data: Map<J, ProofTree>) -> Self {
        assert!(!data.is_empty());
        ProvenSetData::Success(data).into()
    }

    /// Creates a result with no proven values whose evaluation stopped at the
    /// given incomplete frontier.
    pub fn incomplete(frontier: IncompleteTree) -> Self {
        Self::from_incomplete_frontiers(std::iter::once(frontier).collect())
    }

    /// Creates a result with no proven values and one or more incomplete frontiers.
    pub fn from_incomplete_frontiers(incomplete: Set<IncompleteTree>) -> Self {
        assert!(!incomplete.is_empty());
        Self {
            data: ProvenSetData::Empty,
            incomplete,
        }
    }

    /// Adds incomplete frontiers without changing any genuinely proven values
    /// or complete-failure diagnostics in this result.
    #[doc(hidden)]
    pub fn with_incomplete(mut self, incomplete: Set<IncompleteTree>) -> Self {
        self.incomplete.extend(incomplete);
        self
    }

    /// Creates a `JudgmentSet` from a Rust function that failed for the given reason.
    pub fn failed(
        description: impl std::fmt::Display,
        location: FailureLocation,
        reason: impl std::fmt::Display,
    ) -> Self {
        FailedJudgment::new(
            description.to_string(),
            location,
            set![FailedRule::new(RuleFailureCause::Inapplicable {
                reason: reason.to_string()
            })],
        )
        .into()
    }

    /// Creates a judgment set that resulted from a failed judgment.
    /// Meant to be used from the judgment macro, probably annoying to call manually.
    pub fn failed_rules(
        judgment: impl std::fmt::Debug,
        location: FailureLocation,
        failed_rules: Set<FailedRule>,
    ) -> Self {
        let judgment = format!("{judgment:?}");
        FailedJudgment::new(judgment, location, failed_rules).into()
    }

    /// True if the judgment whose result this set represents was proven at least once.
    pub fn is_proven(&self) -> bool {
        match &self.data {
            ProvenSetData::Failure(_) | ProvenSetData::Empty => false,
            ProvenSetData::Success(s) => {
                assert!(!s.is_empty());
                true
            }
        }
    }

    /// True if evaluation completed and established that there are no proofs.
    pub fn is_failed(&self) -> bool {
        self.is_complete() && matches!(&self.data, ProvenSetData::Failure(_))
    }

    /// True if every branch of this evaluation completed normally.
    pub fn is_complete(&self) -> bool {
        self.incomplete.is_empty()
    }

    /// True if one or more branches stopped before derivation enumeration completed.
    pub fn is_incomplete(&self) -> bool {
        !self.is_complete()
    }

    /// The identifiable frontiers at which proof search stopped.
    pub fn incomplete_frontiers(&self) -> &Set<IncompleteTree> {
        &self.incomplete
    }

    /// Convert a complete result to its non-empty map of proven values.
    ///
    /// An incomplete evaluation is rejected even when it contains genuine
    /// proofs, because returning only the map would falsely imply that it is
    /// exhaustive. The error retains the complete partial result.
    pub fn into_map(self) -> Result<Map<J, ProofTree>, ProvenSetError<J>> {
        if self.is_incomplete() {
            return Err(ProvenSetError::incomplete(self));
        }

        match self.data {
            ProvenSetData::Failure(e) => Err(ProvenSetError::Failed(e)),
            ProvenSetData::Empty => unreachable!("a complete result cannot be empty"),
            ProvenSetData::Success(s) => {
                assert!(!s.is_empty());
                Ok(s)
            }
        }
    }

    /// Extract the single proven result from this set.
    /// Panics if the set contains more than one result.
    /// Returns an error if the judgment failed or its evaluation was incomplete.
    pub fn into_singleton(self) -> Result<Proven<J>, ProvenSetError<J>> {
        if self.is_incomplete() {
            return Err(ProvenSetError::incomplete(self));
        }

        match self.data {
            ProvenSetData::Failure(e) => Err(ProvenSetError::Failed(e)),
            ProvenSetData::Empty => unreachable!("a complete result cannot be empty"),
            ProvenSetData::Success(mut s) => {
                assert!(s.len() == 1, "expected singleton, got {} results", s.len());
                Ok(s.pop_first().unwrap())
            }
        }
    }

    /// Iterate through every genuine solution found so far.
    ///
    /// When [`Self::is_incomplete`] is true, this is not an exhaustive set.
    pub fn iter(&self) -> Box<dyn Iterator<Item = Proven<J>> + '_> {
        match &self.data {
            ProvenSetData::Failure(_) | ProvenSetData::Empty => Box::new(std::iter::empty()),
            ProvenSetData::Success(s) => Box::new(
                s.iter()
                    .map(|(judgment, tree)| (J::clone(judgment), tree.clone())),
            ),
        }
    }

    /// For each item `t` that was proven,
    /// invoke `op(t)` to yield a new set of proven results
    /// and then flatten those into a new proven set.
    /// This function preserves failure cause information and is the preferred way to chain
    /// sets.
    #[track_caller]
    pub fn flat_map<Iterable, K>(self, mut op: impl FnMut(Proven<J>) -> Iterable) -> ProvenSet<K>
    where
        Iterable: EachProof<Judgment = K>,
        K: Ord + Debug + Clone,
    {
        let ProvenSet { data, incomplete } = self;
        match data {
            ProvenSetData::Failure(e) => ProvenSet {
                data: ProvenSetData::Failure(e),
                incomplete,
            },
            ProvenSetData::Empty => ProvenSet {
                data: ProvenSetData::Empty,
                incomplete,
            },
            ProvenSetData::Success(proven_items) => {
                let mut items = Map::default();
                let mut failures = set![];
                let mut incomplete = incomplete;

                for proven_item in proven_items {
                    let collection = op(proven_item);
                    let report = collection.each_proof(|(item, proof_tree)| {
                        items.insert(item, proof_tree);
                    });
                    incomplete.extend(report.incomplete);
                    if let Some(cause) = report.failure {
                        failures.insert(FailedRule::new(cause));
                    }
                }

                if !items.is_empty() {
                    ProvenSet::proven(items).with_incomplete(incomplete)
                } else if failures.is_empty() && !incomplete.is_empty() {
                    ProvenSet::from_incomplete_frontiers(incomplete)
                } else {
                    ProvenSet::failed_rules("flat_map", FailureLocation::caller(), failures)
                        .with_incomplete(incomplete)
                }
            }
        }
    }

    /// For each item `t` that was proven,
    /// invoke `op(t)` to yield a new item `u`
    /// and create a proven set from that.
    /// This function preserves failure cause information.
    #[track_caller]
    pub fn map<K>(self, mut op: impl FnMut(Proven<J>) -> Proven<K>) -> ProvenSet<K>
    where
        K: Ord + Debug + Clone,
    {
        self.flat_map::<_, K>(|elem| ProvenSet::singleton(op(elem)))
    }

    /// Convenience function for tests: asserts that the proven values match expected.
    #[track_caller]
    pub fn assert_ok(&self, expect: expect_test::Expect) {
        self.assert_ok_with(expect, &[])
    }

    /// Convenience function for tests: asserts that the proven values match expected,
    /// and that the proof tree contains all the required strings.
    #[track_caller]
    pub fn assert_ok_with(&self, expect_values: expect_test::Expect, must_contain: &[&str]) {
        assert!(
            self.is_complete(),
            "expected a complete successful proof, got an incomplete result: {self}"
        );
        match &self.data {
            ProvenSetData::Failure(e) => panic!("expected a successful proof, got {e}"),
            ProvenSetData::Empty => unreachable!("a complete result cannot be empty"),
            ProvenSetData::Success(map) => {
                crate::judgment::coverage::record_coverage(map.values());

                // Check values only (not proof trees)
                let values: Set<_> = map.keys().cloned().collect();
                expect_values.assert_eq(&format!("{values:?}"));

                // Check proof tree contains required strings
                let full_output = format!("{map:?}");
                for s in must_contain {
                    assert!(
                        full_output.contains(s),
                        "proof tree must contain {s:?} but didn't.\nFull output:\n{full_output}"
                    );
                }
            }
        }
    }

    /// Convenience function for tests: asserts that the proven set is ok and that the debug value is as expected.
    /// Shows only the leaf failures for a concise view.
    #[track_caller]
    pub fn assert_err(&self, expect: expect_test::Expect) {
        assert!(
            self.is_complete(),
            "expected a complete failure, got an incomplete result: {self}"
        );
        match &self.data {
            ProvenSetData::Failure(e) => {
                crate::judgment::coverage::record_negative_coverage(std::iter::once(e.as_ref()));
                expect.assert_eq(&crate::test_util::normalize_paths(e.format_leaves()));
            }
            ProvenSetData::Success(_) => {
                panic!("expected an error, got successful proofs: {self}");
            }
            ProvenSetData::Empty => unreachable!("a complete result cannot be empty"),
        }
    }
}

impl ProvenSet<()> {
    /// For cases where the "value" is just `()`, we can just extract the singular proof-tree directly
    pub fn check_proven(self) -> Result<ProofTree, ProvenSetError<()>> {
        if self.is_incomplete() {
            return Err(ProvenSetError::incomplete(self));
        }

        match self.data {
            ProvenSetData::Failure(e) => Err(ProvenSetError::Failed(e)),
            ProvenSetData::Empty => unreachable!("a complete result cannot be empty"),
            ProvenSetData::Success(mut map) => Ok(map.remove(&()).expect("non-empty")),
        }
    }
}

impl<J: Ord + Debug + Clone> FromIterator<Proven<J>> for ProvenSet<J> {
    #[track_caller]
    fn from_iter<T: IntoIterator<Item = Proven<J>>>(iter: T) -> Self {
        let set: Map<J, ProofTree> = iter.into_iter().collect();
        if set.is_empty() {
            ProvenSet::failed("collect", FailureLocation::caller(), "empty collection")
        } else {
            ProvenSet::proven(set)
        }
    }
}

impl<J: PartialEq> PartialEq for ProvenSetData<J> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Failure(l0), Self::Failure(r0)) => format!("{l0:?}") == format!("{r0:?}"),
            (Self::Empty, Self::Empty) => true,
            (Self::Success(l0), Self::Success(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl<J: std::fmt::Debug> std::fmt::Debug for ProvenSet<J> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self { data, incomplete } = self;
        if incomplete.is_empty() {
            std::fmt::Debug::fmt(data, f)
        } else {
            f.debug_struct("ProvenSet")
                .field("data", data)
                .field("incomplete", incomplete)
                .finish()
        }
    }
}

impl<J: std::fmt::Debug> std::fmt::Debug for ProvenSetData<J> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Failure(arg0) => std::fmt::Debug::fmt(arg0, f),
            Self::Empty => f.write_str("Empty"),
            Self::Success(arg0) => std::fmt::Debug::fmt(arg0, f),
        }
    }
}

impl<J: Eq> Eq for ProvenSetData<J> {}

impl<J: PartialOrd> PartialOrd for ProvenSetData<J> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Failure(l0), Self::Failure(r0)) => {
                PartialOrd::partial_cmp(&format!("{l0:?}"), &format!("{r0:?}"))
            }
            (Self::Success(l0), Self::Success(r0)) => PartialOrd::partial_cmp(l0, r0),
            (Self::Failure(_), Self::Success(_)) => Some(std::cmp::Ordering::Less),
            (Self::Success(_), Self::Failure(_)) => Some(std::cmp::Ordering::Greater),
            (Self::Failure(_), Self::Empty) => Some(std::cmp::Ordering::Less),
            (Self::Empty, Self::Failure(_)) => Some(std::cmp::Ordering::Greater),
            (Self::Empty, Self::Success(_)) => Some(std::cmp::Ordering::Less),
            (Self::Success(_), Self::Empty) => Some(std::cmp::Ordering::Greater),
            (Self::Empty, Self::Empty) => Some(std::cmp::Ordering::Equal),
        }
    }
}

impl<J: Ord> Ord for ProvenSetData<J> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (Self::Failure(l0), Self::Failure(r0)) => {
                Ord::cmp(&format!("{l0:?}"), &format!("{r0:?}"))
            }
            (Self::Success(l0), Self::Success(r0)) => Ord::cmp(l0, r0),
            (Self::Failure(_), Self::Success(_)) => std::cmp::Ordering::Less,
            (Self::Success(_), Self::Failure(_)) => std::cmp::Ordering::Greater,
            (Self::Failure(_), Self::Empty) => std::cmp::Ordering::Less,
            (Self::Empty, Self::Failure(_)) => std::cmp::Ordering::Greater,
            (Self::Empty, Self::Success(_)) => std::cmp::Ordering::Less,
            (Self::Success(_), Self::Empty) => std::cmp::Ordering::Greater,
            (Self::Empty, Self::Empty) => std::cmp::Ordering::Equal,
        }
    }
}

impl<J: Hash> Hash for ProvenSetData<J> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            ProvenSetData::Failure(e) => format!("{e:?}").hash(state),
            ProvenSetData::Empty => 1_u8.hash(state),
            ProvenSetData::Success(s) => s.hash(state),
        }
    }
}

pub type Proven<J> = (J, ProofTree);

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
#[must_use]
pub struct ProofTree {
    /// Name of the judgment being proved
    pub judgment_name: String,

    /// Attributes as field: value pairs
    pub attributes: Vec<(String, String)>,

    /// Succeeded with this rule-name and index, if Some;
    /// if None, then there is only a single rule and this is not relevant.
    pub rule_name: Option<&'static str>,

    /// ...located in this file...
    pub file: String,

    /// ...at this line...
    pub line: u32,

    /// ...and this column...
    pub column: u32,

    /// ...with these subproofs.
    pub children: Vec<ProofTree>,
}

/// A branch at which derivation enumeration stopped without producing a
/// logical output.
///
/// Frontiers are leaf records. Propagating a nested incomplete result preserves
/// the original record instead of wrapping it at every caller, which keeps
/// recursive fixed-point evaluation finite and leaves room for future
/// frontier-specific subsumption.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
pub struct IncompleteTree {
    /// Name of the judgment whose branch stopped.
    pub judgment_name: String,

    /// Debug attributes identifying that judgment invocation.
    pub attributes: Vec<(String, String)>,

    /// The rule containing an explicit incomplete premise, if applicable.
    pub rule_name: Option<&'static str>,

    /// Source location of the cutoff or explicit premise.
    pub file: String,
    pub line: u32,
    pub column: u32,

    /// Why enumeration stopped.
    pub reason: IncompleteReason,
}

/// The operational reason that a judgment branch was incomplete.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
pub enum IncompleteReason {
    /// The branch reached an explicit `(incomplete)` premise.
    Explicit,

    /// The complete judgment input exceeded the active structural-size cutoff.
    Size { size: usize, cutoff: usize },
}

impl IncompleteTree {
    /// Construct the frontier recorded by an explicit `(incomplete)` premise.
    #[doc(hidden)]
    pub fn explicit(
        judgment_name: impl ToString,
        attributes: Vec<(String, String)>,
        rule_name: &'static str,
        file: impl ToString,
        line: u32,
        column: u32,
    ) -> Self {
        Self {
            judgment_name: judgment_name.to_string(),
            attributes,
            rule_name: Some(rule_name),
            file: file.to_string(),
            line,
            column,
            reason: IncompleteReason::Explicit,
        }
    }

    /// Construct the frontier recorded when a judgment input exceeds its cutoff.
    #[doc(hidden)]
    pub fn size(
        judgment_name: impl ToString,
        attributes: Vec<(String, String)>,
        file: impl ToString,
        line: u32,
        column: u32,
        size: usize,
        cutoff: usize,
    ) -> Self {
        Self {
            judgment_name: judgment_name.to_string(),
            attributes,
            rule_name: None,
            file: file.to_string(),
            line,
            column,
            reason: IncompleteReason::Size { size, cutoff },
        }
    }
}

impl ProofTree {
    /// Create a leaf "proof tree" from a value.
    ///
    /// The origin will be the caller of this function
    /// and there won't be any child trees.
    #[track_caller]
    pub fn leaf(judgment: impl ToString) -> Self {
        Self::new(judgment, None, Vec::new())
    }

    pub fn with_all(
        judgment_name: impl ToString,
        attributes: Vec<(String, String)>,
        rule_name: Option<&'static str>,
        file: impl ToString,
        line: u32,
        column: u32,
        children: Vec<ProofTree>,
    ) -> Self {
        Self {
            judgment_name: judgment_name.to_string(),
            attributes,
            rule_name,
            file: file.to_string(),
            line,
            column,
            children,
        }
    }

    /// Create a "proof tree" from a value.
    /// The origin will be the caller of this function.
    #[track_caller]
    pub fn new(
        judgment: impl ToString,
        rule_name: Option<&'static str>,
        children: Vec<ProofTree>,
    ) -> Self {
        let caller = Location::caller();
        let proof = ProofTree {
            judgment_name: judgment.to_string(),
            attributes: Vec::new(),
            rule_name,
            file: caller.file().replace('\\', "/"),
            line: caller.line(),
            column: caller.column(),
            children,
        };
        proof
    }

    /// Create a "proof tree" with explicit attributes.
    /// The origin will be the caller of this function.
    #[track_caller]
    pub fn new_with_attributes(
        judgment_name: impl ToString,
        attributes: Vec<(String, String)>,
        rule_name: Option<&'static str>,
        children: Vec<ProofTree>,
    ) -> Self {
        let caller = Location::caller();
        ProofTree {
            judgment_name: judgment_name.to_string(),
            attributes,
            rule_name,
            file: caller.file().replace('\\', "/"),
            line: caller.line(),
            column: caller.column(),
            children,
        }
    }

    /// Total number of nodes in the proof tree.
    pub fn total_nodes(&self) -> usize {
        1 + self.children.iter().map(|c| c.total_nodes()).sum::<usize>()
    }
}

/// Insert a proof tree into the map, keeping the smaller tree if one already exists.
/// Comparison is by total nodes first, then by string representation as tiebreaker.
pub fn insert_smallest_proof<K: Ord + Clone>(
    map: &mut Map<K, ProofTree>,
    key: K,
    proof: ProofTree,
) {
    match map.get(&key) {
        Some(existing) => {
            let dominated = (proof.total_nodes(), format!("{:?}", proof))
                < (existing.total_nodes(), format!("{:?}", existing));
            if dominated {
                map.insert(key, proof);
            }
        }
        None => {
            map.insert(key, proof);
        }
    }
}

/// Tracks the location in the source where the failure occurred.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone)]
pub struct FailureLocation {
    pub file: String,
    pub line: u32,
    pub column: u32,
}

impl FailureLocation {
    #[track_caller]
    pub fn caller() -> Self {
        let caller = std::panic::Location::caller();
        Self {
            file: caller.file().replace('\\', "/"),
            line: caller.line(),
            column: caller.column(),
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone)]
pub struct FailedJudgment {
    /// Trying to prove this judgment...
    pub judgment: String,

    /// ...defined at this location...
    pub location: FailureLocation,

    /// ...failed with these partially applicable rules.
    /// If empty, it means no rules matched.
    pub failed_rules: Set<FailedRule>,
}

impl std::fmt::Display for FailureLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FailureLocation { file, line, column } = self;
        write!(f, "{file}:{line}:{column}")
    }
}

#[derive(Debug)]
struct HasNonCycle(bool);

impl FailedJudgment {
    /// Create a new "failed judgment" representing a failure to
    /// solve `judgment` with the given set of partially solved rules.
    ///
    /// Does a bit of post-processing to detect cycles,
    /// since some of these rule failures may represent a cyclic attempt
    /// to solve judgment. We employ the heuristic that we only present
    /// cycle errors if ALL the rule failes are cycles. Otherwise, we
    /// show only the non-cyclic failures, as usually that's what the user
    /// is interested in.
    #[tracing::instrument(level = "Debug", ret)]
    fn new(judgment: String, location: FailureLocation, failed_rules: Set<FailedRule>) -> Self {
        // Strip cycles out from the set of failed rules. Note that this algorithm
        // has an O(n^2) character, as the set of failed rules will get recursively
        // simplified as we progress up the stack, but .. who cares?
        let (failed_rules, _) = Self::strip_cycles(&set![&judgment], failed_rules);

        Self {
            judgment,
            location,
            failed_rules,
        }
    }

    /// Simplifies a set of failed rules to exclude cycles, but only if there are non-cyclic results to show instead.
    /// Given a set of failed rules, along with the `stack` of judgments that was being solved at the time these failures occurred,
    /// recursively returns `(set, has_non_cycle)`. The `set` represents a new, simplified set of failed rules.
    #[tracing::instrument(level = "Debug", skip(stack), ret)]
    fn strip_cycles(
        stack: &Set<&String>,
        failed_rules: Set<FailedRule>,
    ) -> (Set<FailedRule>, HasNonCycle) {
        // The input doesn't match any of the judgment rules, so this
        // can't be a cycle — report it as a genuine failure.
        if failed_rules.is_empty() {
            return (set![], HasNonCycle(true));
        }

        // Collect all the failures that were due to cycles
        let mut cycles = set![];

        // Collect failures not due to cycles
        let mut non_cycles = set![];

        // Go over each failure and insert it into the appropriate entry above
        for mut failed_rule in failed_rules {
            let span = tracing::debug_span!("failed_rule", ?failed_rule);
            let _guard = span.enter();

            if let RuleFailureCause::FailedJudgment(mut judgment) = failed_rule.cause {
                // Recursive case: we failed because a judgment failed...
                if stack.contains(&judgment.judgment) && judgment.failed_rules.is_empty() {
                    // ...if that judgment was already on the stack, and we didn't have a more interesting reason,
                    // then this is a failure.
                    failed_rule.cause = RuleFailureCause::Cycle {
                        judgment: judgment.judgment.clone(),
                    };
                    cycles.insert(failed_rule);
                } else {
                    // ...otherwise, recursively simplify the failed rules.
                    // This will return a boolean indicating if all the failed rules
                    // ultimately failed because of a cycle.

                    let mut stack1 = stack.clone();
                    stack1.insert(&judgment.judgment);

                    let judgment_has_non_cycle;
                    (judgment.failed_rules, judgment_has_non_cycle) =
                        Self::strip_cycles(&stack1, judgment.failed_rules);
                    failed_rule.cause = RuleFailureCause::FailedJudgment(judgment);

                    if judgment_has_non_cycle.0 {
                        non_cycles.insert(failed_rule);
                    } else {
                        // If all the failed rules failed because of a cycle,
                        // then this judgment is itself a cycle.
                        cycles.insert(failed_rule);
                    }
                }
            } else if let RuleFailureCause::Cycle { .. } = failed_rule.cause {
                cycles.insert(failed_rule);
            } else {
                non_cycles.insert(failed_rule);
            }
        }

        tracing::debug!(?cycles, ?non_cycles);

        if non_cycles.is_empty() {
            assert!(!cycles.is_empty());
            (cycles, HasNonCycle(false))
        } else {
            (non_cycles, HasNonCycle(true))
        }
    }

    /// Extract "leaf" failures - the actual terminal failure causes
    /// rather than the full nested tree of failed judgments.
    pub fn leaf_failures(&self) -> Vec<LeafFailure> {
        let mut leaves = Vec::new();
        if self.failed_rules.is_empty() {
            leaves.push(LeafFailure::JudgmentNoRules(self.clone()));
        } else {
            for rule in &self.failed_rules {
                rule.collect_leaves(&mut leaves);
            }
        }
        leaves
    }

    /// Format just the leaf failures in a concise way
    pub fn format_leaves(&self) -> String {
        let leaves = self.leaf_failures();
        if leaves.is_empty() {
            format!("judgment had no applicable rules: `{}`", self.judgment)
        } else {
            leaves
                .iter()
                .map(|leaf| leaf.to_string())
                .collect::<Vec<_>>()
                .join("\n\n")
        }
    }

    /// Collect the set of reasons this judgment failed, walking every level
    /// of the failure tree. Used by negative coverage tracking. Each reason
    /// is one of:
    ///
    /// * [`FailureReason::Premise`] — a named rule was tried (its conclusion
    ///   patterns matched and its `!`-clauses survived) and a specific
    ///   premise failed. The `file`/`line` identify that premise's source
    ///   location (the macro respans failures onto the failing premise), so
    ///   we can tell *which* premise of the rule broke.
    /// * [`FailureReason::NoApplicableRule`] — the judgment was exercised but
    ///   no rule matched (or every matching rule was stripped before its
    ///   `!`-clause). This is the leaf case where there is no rule to blame.
    pub fn collect_failure_reasons(&self) -> BTreeSet<FailureReason> {
        let mut acc = BTreeSet::new();
        self.collect_failure_reasons_into(&mut acc);
        acc
    }

    fn collect_failure_reasons_into(&self, acc: &mut BTreeSet<FailureReason>) {
        let judgment = judgment_name_prefix(&self.judgment);
        if self.failed_rules.is_empty() {
            acc.insert(FailureReason::NoApplicableRule {
                judgment,
                file: self.location.file.clone(),
                line: self.location.line,
            });
            return;
        }
        for rule in &self.failed_rules {
            rule.collect_failure_reasons_into(&judgment, acc);
        }
    }
}

/// Extract the leading identifier from `FailedJudgment.judgment`. The
/// macro's `__JudgmentStruct` Debug impl is `debug_struct(stringify!(name))`,
/// so the formatted string always starts with the judgment name followed
/// by ` { ... }`. We grab characters up to the first non-identifier char.
pub(crate) fn judgment_name_prefix(formatted: &str) -> String {
    formatted
        .chars()
        .take_while(|c| c.is_ascii_alphanumeric() || *c == '_')
        .collect()
}

impl FailedRule {
    /// Record a [`FailureReason::Premise`] for this rule's failing premise
    /// (if named), then recurse into a nested `FailedJudgment` cause so that
    /// premises deeper in the tree are recorded against their own judgment.
    fn collect_failure_reasons_into(&self, judgment: &str, acc: &mut BTreeSet<FailureReason>) {
        if let Some(rule_name) = &self.rule_name {
            acc.insert(FailureReason::Premise {
                judgment: judgment.to_string(),
                rule: rule_name.clone(),
                file: self.file.replace('\\', "/"),
                line: self.line,
                cause: self.cause.discriminant_tag().to_string(),
            });
        }
        if let RuleFailureCause::FailedJudgment(inner) = &self.cause {
            inner.collect_failure_reasons_into(acc);
        }
    }
}

/// A single reason a judgment failed, as collected by
/// [`FailedJudgment::collect_failure_reasons`]. This is also the wire format
/// for negative coverage records (see the `coverage` module).
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug, serde::Serialize, serde::Deserialize)]
#[serde(tag = "reason", rename_all = "snake_case")]
pub enum FailureReason {
    /// A named rule was tried and a specific premise failed. `file`/`line`
    /// locate that premise; `cause` is the [`RuleFailureCause`] discriminant
    /// tag describing how it failed.
    Premise {
        judgment: String,
        rule: String,
        file: String,
        line: u32,
        cause: String,
    },

    /// The judgment was exercised but no rule applied (the leaf case).
    NoApplicableRule {
        judgment: String,
        file: String,
        line: u32,
    },
}

impl FailedRule {
    /// Recursively collect leaf failures (failures whose cause is not another FailedJudgment)
    fn collect_leaves<'a>(&'a self, leaves: &mut Vec<LeafFailure>) {
        match &self.cause {
            RuleFailureCause::FailedJudgment(inner) => {
                leaves.extend(inner.leaf_failures());
            }
            _ => {
                // This is a leaf - the cause is a terminal condition
                leaves.push(LeafFailure::Rule(self.clone()));
            }
        }
    }
}

pub enum LeafFailure {
    /// Indicates a specific rule within a judgment that failed
    Rule(FailedRule),

    /// Indicates a judgment with no matching rules
    JudgmentNoRules(FailedJudgment),
}

impl std::fmt::Display for LeafFailure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LeafFailure::Rule(failed_rule) => std::fmt::Display::fmt(failed_rule, f),
            LeafFailure::JudgmentNoRules(FailedJudgment {
                judgment,
                location,
                failed_rules: _,
            }) => write!(f, "{location}: no applicable rules for {judgment}"),
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone)]
pub struct FailedRule {
    /// If Some, then the given rule failed
    /// (if None, then there is only a single rule and this is not relevant)...
    pub rule_name: Option<String>,

    /// ...and is located in this file...
    pub file: String,

    /// ...at this line...
    pub line: u32,

    /// ...and this column...
    pub column: u32,

    /// ...for this reason...
    pub cause: RuleFailureCause,
}

impl FailedRule {
    #[track_caller]
    pub fn new(cause: RuleFailureCause) -> Self {
        let location = Location::caller();
        FailedRule {
            rule_name: None,
            file: location.file().to_string(),
            line: location.line(),
            column: location.column(),
            cause,
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum RuleFailureCause {
    /// The rule did not succeed because an `(if X)` condition evaluated to false.
    IfFalse(IfThen),

    /// The rule did not succeed because an `(if let)` pattern failed to match.
    IfLetDidNotMatch { pattern: String, value: String },

    /// The rule did not succeed because the `x` in a `(x => y)` rule was an empty collection.
    EmptyCollection { expr: String },

    /// The rule did not succeed because the `x` in a `(x => y)` rule was a judgment that failed
    /// (for the given reason).
    FailedJudgment(Box<FailedJudgment>),

    /// The rule did not succeed for some custom reason. This occurs when a `?` fails in the judgment function macro.
    Inapplicable { reason: String },

    /// The rule attempted to prove something that was already in the process of being proven
    Cycle { judgment: String },

    /// The rule explicitly failed with a user-provided message via `(fail "...", ...)`.
    /// Used to document intentionally unsupported cases and produce clear error messages.
    ExplicitFailure { message: String },
}

impl RuleFailureCause {
    /// Snake-case tag identifying the variant. Used by negative coverage
    /// to record *why* a blamed rule failed without leaking the inner
    /// payload (which could differ from one test run to another).
    pub fn discriminant_tag(&self) -> &'static str {
        match self {
            RuleFailureCause::IfFalse(_) => "if_false",
            RuleFailureCause::IfLetDidNotMatch { .. } => "if_let",
            RuleFailureCause::EmptyCollection { .. } => "empty_collection",
            RuleFailureCause::FailedJudgment(_) => "failed_judgment",
            RuleFailureCause::Inapplicable { .. } => "inapplicable",
            RuleFailureCause::Cycle { .. } => "cycle",
            RuleFailureCause::ExplicitFailure { .. } => "explicit_failure",
        }
    }

    pub fn from_anyhow(e: anyhow::Error) -> Self {
        if let Some(failed) = e.downcast_ref::<Box<FailedJudgment>>() {
            RuleFailureCause::FailedJudgment(Box::new((**failed).clone()))
        } else if let Some(failed) = e.downcast_ref::<FailedJudgment>() {
            RuleFailureCause::FailedJudgment(Box::new(failed.clone()))
        } else if let Some(failed) = e
            .chain()
            .find_map(|cause| cause.downcast_ref::<FailedJudgment>())
        {
            // Completion-aware extraction errors expose a complete failure as
            // their source. Preserve the pre-incompleteness leaf diagnostic
            // instead of embedding the wrapper's full anyhow chain.
            RuleFailureCause::FailedJudgment(Box::new(failed.clone()))
        } else {
            RuleFailureCause::Inapplicable {
                reason: format!("{e:?}"),
            }
        }
    }
}

impl std::error::Error for FailedJudgment {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }

    fn description(&self) -> &str {
        "description() is deprecated; use Display"
    }

    fn cause(&self) -> Option<&dyn std::error::Error> {
        self.source()
    }
}

impl std::fmt::Display for FailedJudgment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FailedJudgment {
            judgment,
            failed_rules,
            location,
        } = self;
        if failed_rules.is_empty() {
            write!(
                f,
                "{location}: judgment had no applicable rules: `{judgment}`",
            )
        } else {
            let rules: Vec<String> = failed_rules.iter().map(|r| r.to_string()).collect();
            let rules = indent(rules.join("\n"));
            write!(
                f,
                "judgment `{judgment}` failed at the following rule(s):\n{rules}"
            )
        }
    }
}

impl std::fmt::Display for FailedRule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FailedRule {
            rule_name,
            file,
            line,
            column,
            cause,
        } = self;

        if let Some(rule_name) = rule_name {
            write!(
                f,
                "the rule {rule_name:?} at ({file}:{line}:{column}) failed because\n{cause}",
                cause = indent(cause),
            )
        } else {
            write!(
                f,
                "failed at ({file}:{line}:{column}) because\n{cause}",
                cause = indent(cause),
            )
        }
    }
}

impl std::fmt::Display for RuleFailureCause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuleFailureCause::IfFalse(IfThen { cond, args }) => {
                write!(f, "condition evaluated to false: `{cond}`")?;
                for (arg_expr, arg_value) in args {
                    write!(f, "\n  {arg_expr} = {arg_value:?}")?;
                }
                Ok(())
            }
            RuleFailureCause::IfLetDidNotMatch { pattern, value } => {
                write!(f, "pattern `{pattern}` did not match value `{value}`")
            }
            RuleFailureCause::EmptyCollection { expr } => {
                write!(f, "expression evaluated to an empty collection: `{expr}`")
            }
            RuleFailureCause::FailedJudgment(judgment) => std::fmt::Display::fmt(judgment, f),
            RuleFailureCause::Inapplicable { reason } => {
                write!(f, "{reason}")
            }
            RuleFailureCause::Cycle { judgment } => {
                write!(f, "cyclic proof attempt: `{judgment}`")
            }
            RuleFailureCause::ExplicitFailure { message } => {
                write!(f, "{message}")
            }
        }
    }
}

impl<T: Debug> std::fmt::Display for ProvenSet<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.data {
            ProvenSetData::Failure(err) => {
                std::fmt::Display::fmt(err, f)?;
                if !self.incomplete.is_empty() {
                    writeln!(f)?;
                }
                Ok(())
            }
            ProvenSetData::Empty => writeln!(f, "no proven outputs"),
            ProvenSetData::Success(set) => {
                writeln!(f, "{{")?;
                for (judgment, proof_tree) in set {
                    writeln!(f, "  {judgment:?}")?;
                    proof_tree.fmt_indented(f, "    ")?;
                }
                writeln!(f, "}}")?;
                Ok(())
            }
        }?;

        if !self.incomplete.is_empty() {
            writeln!(f, "incomplete frontiers:")?;
            for frontier in &self.incomplete {
                writeln!(f, "  {frontier}")?;
            }
        }

        Ok(())
    }
}

impl std::fmt::Display for IncompleteTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let file_name = self.file.rsplit('/').next().unwrap_or(&self.file);
        let rule = self
            .rule_name
            .map(|name| format!(" in rule {name:?}"))
            .unwrap_or_default();
        write!(
            f,
            "{}{} at {file_name}:{}:{}",
            self.judgment_name, rule, self.line, self.column
        )?;
        match self.reason {
            IncompleteReason::Explicit => write!(f, ": reached `(incomplete)`"),
            IncompleteReason::Size { size, cutoff } => {
                write!(f, ": input size {size} exceeds cutoff {cutoff}")
            }
        }
    }
}

impl std::fmt::Display for ProofTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_indented(f, "")
    }
}

impl std::fmt::Debug for ProofTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}

impl ProofTree {
    fn fmt_indented(&self, f: &mut std::fmt::Formatter<'_>, prefix: &str) -> std::fmt::Result {
        let file_name = self.file.rsplit('/').next().unwrap_or(&self.file);
        let rule_info = match self.rule_name {
            Some(name) => format!(" ({name})"),
            None => String::new(),
        };

        // Print judgment name with rule info and location
        writeln!(
            f,
            "{prefix}└─ {}:{rule_info} at {file_name}:{}",
            self.judgment_name, self.line
        )?;

        // Print attributes indented under the judgment name
        let attr_prefix = format!("{prefix}       ");
        for (field, value) in &self.attributes {
            let value_str = format!("{value}");
            for (i, line) in value_str.lines().enumerate() {
                if i == 0 {
                    writeln!(f, "{attr_prefix}{field}: {line}")?;
                } else {
                    writeln!(f, "{attr_prefix}  {line}")?;
                }
            }
        }

        let child_prefix = format!("{prefix}   ");
        for child in &self.children {
            child.fmt_indented(f, &child_prefix)?;
        }
        Ok(())
    }
}

fn indent(s: impl std::fmt::Display) -> String {
    let s = s.to_string();
    let lines: Vec<String> = s
        .lines()
        .map(|l| format!("  {l}"))
        .map(|l| l.trim_end().to_string())
        .collect();
    lines.join("\n")
}

/// Metadata produced while enumerating the genuine proofs of one premise.
#[doc(hidden)]
pub struct EachProofReport {
    pub incomplete: Set<IncompleteTree>,
    pub failure: Option<RuleFailureCause>,
}

/// This trait is used for the `(foo => bar)` patterns.
pub trait EachProof {
    type Judgment;

    /// Invokes `each_proof` for each genuine proof and returns orthogonal
    /// complete-failure and incomplete-frontier metadata.
    #[track_caller]
    fn each_proof(self, each_proof: impl FnMut(Proven<Self::Judgment>)) -> EachProofReport;
}

impl<T> EachProof for ProvenSet<T> {
    type Judgment = T;

    fn each_proof(self, mut each_proof: impl FnMut(Proven<Self::Judgment>)) -> EachProofReport {
        let incomplete = self.incomplete;
        match self.data {
            ProvenSetData::Failure(e) => EachProofReport {
                incomplete,
                failure: Some(RuleFailureCause::FailedJudgment(e)),
            },
            ProvenSetData::Empty => EachProofReport {
                incomplete,
                failure: None,
            },
            ProvenSetData::Success(s) => {
                for item in s {
                    each_proof(item);
                }
                EachProofReport {
                    incomplete,
                    failure: None,
                }
            }
        }
    }
}

impl<T: Clone> EachProof for &ProvenSet<T> {
    type Judgment = T;

    fn each_proof(self, mut each_proof: impl FnMut(Proven<Self::Judgment>)) -> EachProofReport {
        match &self.data {
            ProvenSetData::Failure(e) => EachProofReport {
                incomplete: self.incomplete.clone(),
                failure: Some(RuleFailureCause::FailedJudgment(e.clone())),
            },
            ProvenSetData::Empty => EachProofReport {
                incomplete: self.incomplete.clone(),
                failure: None,
            },
            ProvenSetData::Success(s) => {
                for (key, proof) in s {
                    each_proof((key.clone(), proof.clone()));
                }
                EachProofReport {
                    incomplete: self.incomplete.clone(),
                    failure: None,
                }
            }
        }
    }
}

/// Proof rule for `(x in collection)`.
/// Invokes `each_proof` for each member of `collection`.
///
/// # Parameters
///
/// * `collection` is the value to be iterated.
/// * `stringify_expr` gives a string for error message purposes, in case the collection is empty
/// * `each_proof` is the action to take on each item
pub fn member_of<T: Debug>(
    collection: impl IntoIterator<Item = T>,
    stringify_expr: impl FnOnce() -> String,
    mut each_proof: impl FnMut(Proven<T>),
) -> Result<(), RuleFailureCause> {
    let mut iter = collection.into_iter().peekable();
    if iter.peek().is_none() {
        Err(RuleFailureCause::EmptyCollection {
            expr: stringify_expr(),
        })
    } else {
        for item in iter {
            let proof_tree = ProofTree::leaf(format!("item = {item:?}"));
            each_proof((item, proof_tree));
        }
        Ok(())
    }
}

/// Result of checking a premise whose logical output type is `()`.
#[doc(hidden)]
pub struct CheckProvenReport {
    pub proof: Option<ProofTree>,
    pub incomplete: Set<IncompleteTree>,
    pub failure: Option<RuleFailureCause>,
}

pub trait CheckProven {
    /// Return the genuine unit proof, failure diagnostic, and incomplete
    /// frontiers without conflating any of those dimensions.
    #[track_caller]
    fn check_proven(self) -> CheckProvenReport;
}

impl CheckProven for Fallible<ProofTree> {
    #[track_caller]
    fn check_proven(self) -> CheckProvenReport {
        match self {
            Ok(proof_tree) => CheckProvenReport {
                proof: Some(proof_tree),
                incomplete: Set::new(),
                failure: None,
            },
            Err(e) => match IncompleteError::from_anyhow(&e) {
                Some(incomplete) => CheckProvenReport {
                    proof: None,
                    incomplete: incomplete.incomplete_frontiers().clone(),
                    failure: incomplete
                        .failure()
                        .cloned()
                        .map(|failure| RuleFailureCause::FailedJudgment(Box::new(failure))),
                },
                None => CheckProvenReport {
                    proof: None,
                    incomplete: Set::new(),
                    failure: Some(RuleFailureCause::from_anyhow(e)),
                },
            },
        }
    }
}

impl CheckProven for ProvenSet<()> {
    #[track_caller]
    fn check_proven(self) -> CheckProvenReport {
        let incomplete = self.incomplete;
        match self.data {
            ProvenSetData::Failure(e) => CheckProvenReport {
                proof: None,
                incomplete,
                failure: Some(RuleFailureCause::FailedJudgment(e)),
            },
            ProvenSetData::Empty => CheckProvenReport {
                proof: None,
                incomplete,
                failure: None,
            },
            ProvenSetData::Success(mut map) => CheckProvenReport {
                proof: Some(map.remove(&()).expect("non-empty")),
                incomplete,
                failure: None,
            },
        }
    }
}

impl CheckProven for &ProvenSet<()> {
    #[track_caller]
    fn check_proven(self) -> CheckProvenReport {
        <ProvenSet<()> as CheckProven>::check_proven(self.clone())
    }
}
