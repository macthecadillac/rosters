use arrayvec::{ArrayVec, ArrayString};
use derive_more::{AsRef, Display, From, FromStr, Into};
use itertools::Itertools;
use rand::seq::SliceRandom;
use serde::{Deserialize, Deserializer};
use serde::de::Error;

use std::fmt::{Display, Formatter};
use std::collections::HashMap;

use crate::error;

#[derive(Deserialize, Debug)]
pub struct Config {
    #[serde(rename="ta-assignment")]
    pub ta_assignment: Option<HashMap<TA, ArrayVec<Section, 8>>>,
    pub checkpoints: Option<HashMap<Lab, ArrayVec<Checkpoint, 8>>>,
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
enum Union { S(ArrayString<40>), U(usize), }

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
pub struct TA(ArrayString<40>);

#[derive(Copy, Clone, Debug, Deserialize, Display, Eq, From, Into, Hash, Ord, PartialEq, PartialOrd)]
#[serde(try_from="ArrayString<10>")]
pub struct Lab(usize);

impl TryFrom<ArrayString<10>> for Lab {
    type Error = error::Error;
    fn try_from(str: ArrayString<10>) -> Result<Self, error::Error> {
        if str.bytes().zip(b"lab".iter()).all(|(a, &b)| a == b) {
            Ok(Lab(str.as_str()[3..].parse()?))
        } else {
            Err(error::Error::UnknownLabPrefix(str))
        }
    }
}

#[derive(AsRef, Clone, Debug, Deserialize, Display, Eq, From, FromStr, Ord, PartialEq, PartialOrd)]
#[as_ref(forward)]
pub struct Checkpoint(ArrayString<20>);

#[derive(Clone, Debug, Deserialize, Eq, Ord, PartialEq, PartialOrd)]
#[serde(try_from="ArrayString<40>")]
struct Name {
    first: ArrayString<40>,
    last: ArrayString<40>
}

impl TryFrom<ArrayString<40>> for Name {
    type Error = error::Error;
    fn try_from(str: ArrayString<40>) -> Result<Self, error::Error> {
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

impl<'a> Name {
    fn as_ref(&'a self) -> NameRef<'a> {
        NameRef { first: self.first.as_str(), last: self.last.as_str() }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct NameRef<'a> {
    first: &'a str,
    last: &'a str
}

impl<'a> Display for NameRef<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{} {}", self.first, self.last)
    }
}

#[derive(Clone, Debug)]
pub struct Roster<'a> {
    pub section: Section,
    pub groups: ArrayVec<ArrayVec<NameRef<'a>, 7>, 6>
}

impl<'a> Roster<'a> {
    pub fn from_records(records: &'a [Record]) -> Result<Vec<Roster<'a>>, error::Error> {
        let mut rng = rand::thread_rng();
        let mut recs: Vec<_> = records.iter().collect();
        recs.sort_by_key(|&record| record.section);
        let rosters = recs.iter()
            .group_by(|record| record.section)
            .into_iter()
            .map(|(section, list)| {
                let mut names: Vec<_> = list.map(|&r| r.student.as_ref()).collect();
                names.shuffle(&mut rng);
                let n = names.len();
                let ngroups = n / 5 + if n % 5 == 0 { 0 } else { 1 };
                let mut groups = ArrayVec::new();
                for _ in 0..ngroups { groups.push(ArrayVec::new()); }
                for (n, &name) in (0..ngroups).cycle().zip(names.iter()) {
                    groups[n].push(name);
                }
                Roster { section, groups }
            })
            .collect();
        Ok(rosters)
    }
}
