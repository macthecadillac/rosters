use arrayvec::{ArrayVec, ArrayString, CapacityError};
use derive_more::{AsRef, Display, From, FromStr, Into};
use itertools::Itertools;
use rand::seq::SliceRandom;
use serde::{Deserialize, Deserializer};
use serde::de::Error;

use std::fmt::{Display, Formatter};
use std::collections::HashMap;

use crate::error;

pub const NGROUPS: usize = 6;
pub const MAXGROUPSIZE: usize = 7;
pub const MAXNAMELEN: usize = 40;
pub const LABSTRMAXLEN: usize = 10;
const TARGETGROUPSIZE: usize = 5;
const MAXCHECKPOINTS: usize = 8;
const MAXCHECKPOINTLEN: usize = 20;


#[derive(Deserialize, Debug)]
pub struct Config {
    #[serde(rename="ta-assignment")]
    pub ta_assignment: Option<HashMap<TA, ArrayVec<Section, MAXCHECKPOINTS>>>,
    pub checkpoints: Option<HashMap<Lab, ArrayVec<Checkpoint, MAXCHECKPOINTS>>>,
}

#[derive(Clone, Debug, Deserialize, Eq, Ord, PartialEq, PartialOrd)]
pub struct Record {
    #[serde(rename(deserialize="Section"))]
    section: Section,
    #[serde(rename(deserialize="Student"))]
    student: Name
}

#[derive(Clone, Copy, Debug, Deserialize, Display, Eq, From, Into, Hash, Ord, PartialEq, PartialOrd)]
pub struct Section(#[serde(deserialize_with="deserialize_section")] usize);

#[derive(Deserialize)]
#[serde(untagged)]
enum Union { S(ArrayString<MAXNAMELEN>), U(usize), }

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

#[derive(AsRef, Clone, Debug, Deserialize, Display, Eq, From, Hash, Ord, PartialEq, PartialOrd)]
#[serde(transparent)]
#[as_ref(forward)]
pub struct TA(ArrayString<MAXNAMELEN>);

#[derive(Copy, Clone, Debug, Deserialize, Display, Eq, From, Into, Hash, Ord, PartialEq, PartialOrd)]
#[serde(try_from="ArrayString<LABSTRMAXLEN>")]
pub struct Lab(usize);

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

#[derive(AsRef, Clone, Debug, Deserialize, Display, Eq, From, FromStr, Ord, PartialEq, PartialOrd)]
#[from(forward)]
#[as_ref(forward)]
pub struct Checkpoint(ArrayString<MAXCHECKPOINTLEN>);

#[derive(Clone, Debug, Deserialize, Eq, Ord, PartialEq, PartialOrd)]
#[serde(try_from="ArrayString<MAXNAMELEN>")]
pub struct Name {
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

#[derive(Clone, Debug)]
pub struct Roster<'a> {
    pub section: Section,
    pub names: ArrayVec<&'a Name, {MAXGROUPSIZE * NGROUPS}>,
}

#[derive(Clone, Debug)]
pub struct Groups<'a> {
    names: &'a [&'a Name],
    ngroups: usize,
    start: usize,
    group: usize
}

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
    pub fn ngroups(&self) -> usize {
        Ord::min(NGROUPS, ceil_div(self.names.len(), TARGETGROUPSIZE))
    }

    pub fn groups(&'a self) -> Groups<'a> {
        Groups { ngroups: self.ngroups(), names: self.names.as_slice(), start: 0, group: 0 }
    }

    pub fn from_records(records: &'a [Record]) -> Result<Vec<Roster<'a>>, error::Error> {
        let mut rng = rand::thread_rng();
        let mut recs: Vec<_> = records.iter().collect();
        recs.sort_by_key(|&record| record.section);
        let rosters = recs.iter()
            .group_by(|record| record.section)
            .into_iter()
            .map(|(section, list)| {
                let mut names = ArrayVec::new();
                for item in list {
                    names.try_push(&item.student).map_err(CapacityError::simplify)?;
                }
                names.shuffle(&mut rng);
                Ok(Roster { section, names })
            })
            .collect::<Result<Vec<_>, error::Error>>()?;
        Ok(rosters)
    }
}
