use derive_more::{AsRef, Display, From, FromStr, Into};
use itertools::Itertools;
use rand::seq::SliceRandom;
use serde::{Deserialize, Deserializer};
use serde::de::Error;

use std::fmt::{Display, Formatter};
use std::collections::HashMap;
use std::str::FromStr;

use crate::error;

/// This defines the configuration file format. The configuration consists of two sections:
/// `ta_assignemnt` and `checkpoints`. Both sections are optional, as is the configuration itself.
#[derive(Deserialize, Debug)]
pub(crate) struct CheckpointConfig {
    pub(crate) checkpoints: HashMap<Lab, Vec<Checkpoint>>
}

#[derive(Deserialize, Debug)]
pub(crate) struct Config {
    #[serde(rename="number-of-groups")]
    pub(crate) ngroups: Option<usize>,
    #[serde(rename="ta-assignment")]
    pub(crate) ta_assignment: Option<HashMap<TA, Vec<Section>>>,
    pub(crate) checkpoints: Option<HashMap<Lab, Vec<Checkpoint>>>,
    #[serde(rename="1AL")]
    pub(crate) a: Option<CheckpointConfig>,
    #[serde(rename="1BL")]
    pub(crate) b: Option<CheckpointConfig>,
    #[serde(rename="1CL")]
    pub(crate) c: Option<CheckpointConfig>
}

impl Config {
    pub(crate) fn get_checkpoints(
        &self,
        class: Class,
        lab: &Lab,
        no_sign: bool
    ) -> Option<Vec<Checkpoint>> {
        let checkpoint_config = self.checkpoints.as_ref()
            .or(match class {
                Class::A => &self.a,
                Class::B => &self.b,
                Class::C => &self.c
            }.as_ref().map(|c| &c.checkpoints));
        if let Some(checkpoints) = checkpoint_config.as_ref().and_then(|m| m.get(lab)) {
            let mut chkpts = checkpoints.to_vec();
            if !no_sign { chkpts.push(FromStr::from_str("Signed").unwrap()); }
            Some(chkpts)
        } else {
            None
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub(crate) struct Session {
    pub(crate) class: Class,
    pub(crate) section: Section
}

impl<'de> Deserialize<'de> for Session {
    fn deserialize<D>(deserializer: D) -> Result<Session, D::Error>
        where D: Deserializer<'de>, {
        let str = String::deserialize(deserializer).map_err(D::Error::custom)?;
        let chars: Vec<_> = str.trim().chars().collect();
        let class = match chars.as_slice() {
            // the first pattern is for the toml configs, the second for Canvas output
            &['P', 'H', 'Y', 'S', ' ', '1', 'A', 'L', ..] => Ok(Class::A),
            &['P', 'H', 'Y', 'S', ' ', '1', 'B', 'L', ..] => Ok(Class::B),
            &['P', 'H', 'Y', 'S', ' ', '1', 'C', 'L', ..] => Ok(Class::C),
            _ => Err(D::Error::custom("unrecognized string"))
        }?;
        let section = Section(
            str.split_whitespace().rev().skip(1).next()
               .ok_or(D::Error::custom("unrecognized string"))?
               // Try to parse NNN as an integer
               .parse::<usize>()
               .map_err(|m| D::Error::custom(m))?
            );
        Ok(Session { class, section })
    }
}

/// Data record entry
#[derive(Clone, Debug, Deserialize, Eq, Ord, PartialEq, PartialOrd)]
pub(crate) struct Record {
    #[serde(rename(deserialize="Section"))]
    pub(crate) session: Session,
    #[serde(rename(deserialize="Student"))]
    pub(crate) name: Name,
    #[serde(rename(deserialize="SIS User ID"))]
    pub(crate) sid: SID
}

/// In memory representation of a lab section
#[derive(Clone, Copy, Debug, Deserialize, Display, Eq, From, Into, Hash, Ord, PartialEq, PartialOrd)]
pub struct Section(usize);

#[derive(AsRef, Clone, Debug, Deserialize, Display, Eq, From, Hash, Ord, PartialEq, PartialOrd)]
#[serde(transparent)]
#[as_ref(forward)]
pub(crate) struct TA(String);

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Class { A, B, C }

impl<'de> Deserialize<'de> for Class {
    fn deserialize<D>(deserializer: D) -> Result<Class, D::Error>
        where D: Deserializer<'de>, {
        let str = String::deserialize(deserializer).map_err(D::Error::custom)?;
        FromStr::from_str(&str).map_err(D::Error::custom)
    }
}

// for clap
impl FromStr for Class {
    type Err = &'static str;
    fn from_str(s: &str) -> Result<Class, &'static str> {
        match s.trim() {
            "A" => Ok(Class::A),
            "B" => Ok(Class::B),
            "C" => Ok(Class::C),
            _ => Err("the only acceptable inputs are 'A', 'B' or 'C'")
        }
    }
}

impl Display for Class {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Class::A => write!(f, "1AL"),
            Class::B => write!(f, "1BL"),
            Class::C => write!(f, "1CL")
        }
    }
}

/// The memory representation of a lab
#[derive(Copy, Clone, Debug, Deserialize, Eq, From, Into, Hash, Ord, PartialEq, PartialOrd)]
#[serde(try_from="String")]
pub struct Lab(usize);

impl TryFrom<String> for Lab {
    type Error = error::Error;
    fn try_from(str: String) -> Result<Self, error::Error> {
        FromStr::from_str(&str)
    }
}

impl FromStr for Lab {
    type Err = error::Error;
    fn from_str(str: &str) -> Result<Self, error::Error> {
        let s: Vec<_> = str.trim().chars().collect();
        match s.as_slice() {
            ['m', 'a', 't', 'h', 'b', 'o', 'o', 't', 'c', 'a', 'm', 'p'] => Ok(Lab(0)),
            ['l', 'a', 'b', n] | [n] if n.is_digit(10) => Ok(Lab(n.to_digit(10).unwrap() as usize)),
            _ => Err(error::Error::UnknownLabPrefix(str.to_owned()))
        }
    }
}

impl Display for Lab {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self.0 {
            0 => write!(f, "Math Bootcamp"),
            n => write!(f, "Lab {}", n)
        }
    }
}

/// Representation of a checkpoint.
#[derive(AsRef, Clone, Debug, Deserialize, Display, Eq, From, FromStr, Ord, PartialEq, PartialOrd)]
#[from(forward)]
#[as_ref(forward)]
pub(crate) struct Checkpoint(String);

/// Representation of a student's name.
#[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[serde(try_from="String")]
pub(crate) struct Name {
    first: String,
    last: String
}

impl TryFrom<String> for Name {
    type Error = error::Error;
    fn try_from(str: String) -> Result<Self, error::Error> {
        // Canvas names are formatted as "Last, First"
        let name_parts: Vec<_> = str.split(',').collect();
        match &name_parts[..] {
            &[l, f] => {
                Ok(Name { first: f.trim().to_owned(), last: l.trim().to_owned() })
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

#[derive(Clone, Debug, Deserialize, Display, Eq, From, Into, Hash, Ord, PartialEq, PartialOrd)]
pub (crate)struct SID(pub(crate) String);

#[derive(Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub (crate)struct Student<'a> {
    /// name of student
    pub(crate) name: &'a Name,
    /// student ID
    pub(crate) sid: &'a SID,
    /// flag for name clashes
    pub(crate) name_clash: bool
}

impl<'a> Display for Student<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        if self.name_clash {
            write!(f, "{} ({})", self.name, self.sid)
        } else {
            write!(f, "{}", self.name)
        }
    }
}

/// The representation of a roster
#[derive(Clone, Debug)]
pub(crate) struct Roster<'a> {
    /// Maximum number of groups
    pub(crate) ngroups: usize,
    /// Target number of students per group
    pub(crate) group_size: usize,
    /// Lab section
    pub(crate) session: Session,
    /// List of students enrolled in the section
    pub(crate) students: Vec<Student<'a>>,
}

/// An iterator of groups within a section. A group is a set of students assigned to the same lab
/// bench.
#[derive(Clone, Debug)]
pub(crate) struct Groups<'a> {
    students: &'a [Student<'a>],
    ngroups: usize,
    start: usize,
    group: usize
}

/// Convenience function for performing ceiling division. This is more optimizable than using
/// branching operations.
fn ceil_div(a: usize, b: usize) -> usize { (a + b - 1) / b }

impl<'a> Iterator for Groups<'a> {
    type Item = &'a [Student<'a>];
    fn next(&mut self) -> Option<Self::Item> {
        // size is always (TARGETGROUPSIZE - 1) or more unless the number of students is less than
        // two groups full
        let size = ceil_div(self.students.len() - self.group, self.ngroups);
        let start = self.start;
        self.start += size;
        self.group += 1;
        self.students.get(start..start + size)
    }
}

impl<'a> Roster<'a> {
    /// The number of groups we need to have in a section
    pub(crate) fn ngroups(&self) -> usize {
        Ord::min(self.ngroups, ceil_div(self.students.len(), self.group_size))
    }

    /// Returns an iterator over groups of students
    pub(crate) fn groups(&'a self) -> Groups<'a> {
        Groups { ngroups: self.ngroups(), students: self.students.as_slice(), start: 0, group: 0 }
    }

    /// Creates a vec of rosters from a given set of record entries
    pub(crate) fn from_records(
        ngroups: usize,
        group_size: usize,
        records: &'a [Record]
    ) -> Vec<Roster<'a>> {
        let mut rng = rand::thread_rng();
        let mut recs: Vec<_> = records.iter().collect();
        recs.sort_by_key(|&record| record.session.section);
        recs.iter()
            .group_by(|record| record.session)
            .into_iter()
            .map(|(session, list)| {
                let mut students = vec![];
                let mut ord_list: Vec<_> = list.cloned().collect();
                ord_list.sort();
                let mut clashed_last = false;
                let iter = ord_list.iter();
                let offset_iter = ord_list.iter().cycle().skip(1);
                for (item, next) in iter.zip(offset_iter) {
                    let clashed_with_next = item.name == next.name;
                    let name_clash = clashed_with_next || clashed_last;
                    clashed_last = clashed_with_next;
                    let student = Student { name: &item.name, sid: &item.sid, name_clash };
                    students.push(student);
                }
                students.shuffle(&mut rng);
                Roster { group_size, ngroups, session, students }
            })
            .collect()
    }
}
