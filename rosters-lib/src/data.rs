use arrayvec::{ArrayVec, ArrayString};
use derive_more::{AsRef, Display, From, FromStr, Into};
use itertools::Itertools;
use rand::seq::SliceRandom;
use serde::{Deserialize, Deserializer};
use serde::de::Error;

use std::fmt::{Display, Formatter};
use std::collections::HashMap;

use crate::error;

/// The maximum number of groups per lab. This is obviously a hard limit, given two lab rooms only
/// have 6 benches in them.
pub(crate) const NGROUPS: usize = 6;
/// The maximum number of students in per group. This is a physical constraint since lab rooms
/// literally cannot hold that many people, not unless the department spends money it does not have
/// to uprgade them.
pub(crate) const MAXGROUPSIZE: usize = 7;
/// The maximum number of chars a first/last name could have. Given common names fall within 10
/// chars, the probability of a name exceeding 50 chars and that Canvas could even handle that is
/// almost non-existent.
pub(crate) const MAXNAMELEN: usize = 50;
/// The maximum length of a lab name. Given the format needs to be "lab#", it is impossible to
/// breach this number unless we have more than 100 labs per quarter or if labs start to be named
/// with crazy numbers.
pub(crate) const LABSTRMAXLEN: usize = 5;
/// The target group size. A full class obviously has more than 5 students in every group and an
/// underenrolled class could have less. This is simply a reference so the algorithm could decide
/// when to split up groups.
const TARGETGROUPSIZE: usize = 5;
/// The maximum number of checkpoints per lab. This is obviously an overkill--most labs should have
/// no more than 4-5 checkpoints.
const MAXCHECKPOINTS: usize = 10;
/// The maximum number of sections a TA can be assigned. The current number should be more than
/// adequate unless the department intends to turn graduate students into full-time instructors.
const MAXSECTION: usize = 20;
/// The maximum number of chars a checkpoint label could have. This is obvious an overkill since
/// anything more than 10 chars would serious disrupt the layout algorithm.
const MAXCHECKPOINTLEN: usize = 20;


/// This defines the configuration file format. The configuration consists of two sections:
/// `ta_assignemnt` and `checkpoints`. Both sections are optional, as is the configuration itself.
/// There is an upper limit on how many sections a TA can be assigned, which is defined by the
/// `MAXSECTION` constant above. There is also an upper limit on how many checkpoints a lab can
/// have. That is defined by `MAXCHECKPOINTS` constant. While the HashMap is allocated on the heap,
/// its contents are locally owned to prevent further indirection.
#[derive(Deserialize, Debug)]
pub(crate) struct Config {
    #[serde(rename="ta-assignment")]
    pub(crate) ta_assignment: Option<HashMap<TA, ArrayVec<Section, MAXSECTION>>>,
    pub(crate) checkpoints: Option<HashMap<Lab, ArrayVec<Checkpoint, MAXCHECKPOINTS>>>,
}

/// Data record entry
#[derive(Clone, Debug, Deserialize, Eq, Ord, PartialEq, PartialOrd)]
pub(crate) struct Record {
    #[serde(rename(deserialize="Section"))]
    section: Section,
    #[serde(rename(deserialize="Student"))]
    student: Name
}

/// In memory representation of a lab section
#[derive(Clone, Copy, Debug, Deserialize, Display, Eq, From, Into, Hash, Ord, PartialEq, PartialOrd)]
pub(crate) struct Section(#[serde(deserialize_with="deserialize_section")] usize);

/// A convenient/ephemeral type for the deserialization of section strings
#[derive(Deserialize)]
#[serde(untagged)]
enum Union { S(ArrayString<MAXNAMELEN>), U(usize), }

/// Helper function for the deserialization of lab section strings
fn deserialize_section<'de, D>(deserializer: D) -> Result<usize, D::Error>
    where D: Deserializer<'de>, {
    let res = match Union::deserialize(deserializer).map_err(D::Error::custom)? {
        Union::S(v) => {
            v.split_whitespace().rev().skip(1).next()
             .ok_or(D::Error::custom("unrecognized string"))?
             .parse::<usize>()
             .map_err(|m| D::Error::custom(m))?
        },
        Union::U(v) => v
    };
    Ok(res)
}

/// A statically/stack allocated representation of a TA's name. Because of the staticity of the
/// definition, there is a size limitation that is defined by `MAXNAMELEN`.
#[derive(AsRef, Clone, Debug, Deserialize, Display, Eq, From, Hash, Ord, PartialEq, PartialOrd)]
#[serde(transparent)]
#[as_ref(forward)]
pub(crate) struct TA(ArrayString<MAXNAMELEN>);

/// The memory representation of a lab
#[derive(Copy, Clone, Debug, Deserialize, Display, Eq, From, Into, Hash, Ord, PartialEq, PartialOrd)]
#[serde(try_from="ArrayString<LABSTRMAXLEN>")]
pub(crate) struct Lab(usize);

impl TryFrom<ArrayString<LABSTRMAXLEN>> for Lab {
    type Error = error::Error;
    fn try_from(str: ArrayString<LABSTRMAXLEN>) -> Result<Self, error::Error> {
        if str.bytes().zip(b"lab".iter()).all(|(a, &b)| a == b) {
            Ok(Lab(str.as_str()[3..].parse()?))
        } else {
            Err(error::Error::UnknownLabPrefix(str))
        }
    }
}

/// A statically/stack allocated representation of a checkpoint. Because of the staticity of the
/// definition, there is a size limitation that is defined by `MAXCHECKPOINTLEN`.
#[derive(AsRef, Clone, Debug, Deserialize, Display, Eq, From, FromStr, Ord, PartialEq, PartialOrd)]
#[from(forward)]
#[as_ref(forward)]
pub(crate) struct Checkpoint(ArrayString<MAXCHECKPOINTLEN>);

/// A statically/stack allocated representation of a student's name.
#[derive(Clone, Debug, Deserialize, Eq, Ord, PartialEq, PartialOrd)]
#[serde(try_from="ArrayString<MAXNAMELEN>")]
pub(crate) struct Name {
    first: ArrayString<MAXNAMELEN>,
    last: ArrayString<MAXNAMELEN>
}

impl TryFrom<ArrayString<MAXNAMELEN>> for Name {
    type Error = error::Error;
    fn try_from(str: ArrayString<MAXNAMELEN>) -> Result<Self, error::Error> {
        let name_parts: Vec<_> = str.split(',').collect();
        match &name_parts[..] {
            &[l, f] => {
                let trim = |s: &str| {
                    let trimmed = s.trim();
                    trimmed.try_into()
                           .map_err(|_| error::Error::NameTooLongError(trimmed.into()))
                };
                Ok(Name { first: trim(f)?, last: trim(l)? })
            },
            _ => Err(error::Error::ParseNameError(str))
        }
    }
}

impl Display for Name {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{} {}", self.first, self.last)
    }
}

/// The representation of a roster
#[derive(Clone, Debug)]
pub(crate) struct Roster<'a> {
    /// Lab section
    pub(crate) section: Section,
    /// List of students enrolled in the section
    pub(crate) names: ArrayVec<&'a Name, {MAXGROUPSIZE * NGROUPS}>,
}

/// An iterator of groups within a section. A group is a set of students assigned to the same lab
/// bench.
#[derive(Clone, Debug)]
pub(crate) struct Groups<'a> {
    names: &'a [&'a Name],
    ngroups: usize,
    start: usize,
    group: usize
}

/// Convenience function for performing ceiling division. This is more optimizable than using
/// branching operations.
fn ceil_div(a: usize, b: usize) -> usize { (a + b - 1) / b }

impl<'a> Iterator for Groups<'a> {
    type Item = &'a [&'a Name];
    fn next(&mut self) -> Option<Self::Item> {
        let size = ceil_div(self.names.len() - self.group, self.ngroups);
        let start = self.start;
        self.start += size;
        self.group += 1;
        self.names.get(start..start + size)
    }
}

impl<'a> Roster<'a> {
    /// The number of groups we need to have in a section
    pub(crate) fn ngroups(&self) -> usize {
        Ord::min(NGROUPS, ceil_div(self.names.len(), TARGETGROUPSIZE))
    }

    /// Returns an iterator over groups of students
    pub(crate) fn groups(&'a self) -> Groups<'a> {
        Groups { ngroups: self.ngroups(), names: self.names.as_slice(), start: 0, group: 0 }
    }

    /// Creates a vec of rosters from a given set of record entries
    pub(crate) fn from_records(records: &'a [Record]) -> Result<Vec<Roster<'a>>, error::Error> {
        let mut rng = rand::thread_rng();
        let mut recs: Vec<_> = records.iter().collect();
        recs.sort_by_key(|&record| record.section);
        recs.iter()
            .group_by(|record| record.section)
            .into_iter()
            .map(|(section, list)| {
                let mut names = ArrayVec::new();
                for item in list {
                    names.try_push(&item.student).map_err(|_| error::Error::SectionSizeError)?;
                }
                names.shuffle(&mut rng);
                Ok(Roster { section, names })
            })
            .collect()
    }
}
