use derive_more::{AsRef, Display, From, Into};
use itertools::Itertools;
use rand::seq::SliceRandom;
use serde::{Deserialize, Deserializer};
use serde::de::Error;

use std::collections::HashMap;
use std::fmt::{Display, Formatter};

#[derive(Deserialize, Debug)]
pub struct Config {
    #[serde(rename = "ta-assignment")]
    pub ta_assignment: Option<HashMap<String, Vec<Section>>>,
    pub checkpoints: Option<HashMap<Lab, Vec<Checkpoint>>>,
}

#[derive(Clone, Debug, Deserialize)]
pub struct Record {
    #[serde(rename(deserialize = "Student"))]
    student: Name,
    #[serde(rename(deserialize = "Section"))]
    section: Section
}

#[derive(Clone, Copy, Debug, Deserialize, Display, Eq, From, Into, Hash, Ord, PartialEq, PartialOrd)]
pub struct Section(#[serde(deserialize_with = "deserialize_section")] usize);

#[derive(Deserialize)]
#[serde(untagged)]
enum Union { S(String), U(usize), }

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

#[derive(Clone, Debug, Deserialize, Display, Eq, From, Ord, PartialEq, PartialOrd)]
struct TA(String);

#[derive(Copy, Clone, Debug, Deserialize, Display, Eq, From, Hash, Ord, PartialEq, PartialOrd)]
#[serde(try_from = "String")]
pub struct Lab(usize);

impl TryFrom<String> for Lab {
    type Error = crate::error::Error;
    fn try_from(str: String) -> Result<Self, crate::error::Error> {
        if str.chars().zip("lab".chars()).all(|(a, b)| a == b) {
            Ok(Lab(str.as_str()[3..].parse()?))
        } else {
            Err(crate::error::Error::UnknownLabPrefix)
        }
    }
}

#[derive(AsRef, Clone, Debug, Deserialize, Display, Eq, From, Ord, PartialEq, PartialOrd)]
#[from(forward)]
#[as_ref(forward)]
pub struct Checkpoint(String);

#[derive(Clone, Debug, Deserialize)]
#[serde(try_from = "String")]
struct Name {
    first: String,
    last: String
}

impl TryFrom<String> for Name {
    type Error = crate::error::Error;
    fn try_from(str: String) -> Result<Self, crate::error::Error> {
        let name_parts: Vec<&str> = str.split(',').collect();
        match &name_parts[..] {
            &[last, first] => Ok(Name { first: first.trim().to_owned(), last: last.trim().to_owned() }),
            _ => Err(crate::error::Error::ParseNameError)
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
    pub groups: Vec<Vec<NameRef<'a>>>
}

impl<'a> From<NameList<'a>> for Roster<'a> {
    fn from(list: NameList<'a>) -> Self {
        let NameList { names, section } = list;
        let n = names.len();
        let ngroups = n / 5 + if n % 5 == 0 { 0 } else { 1 };
        let mut groups = vec![vec![]; ngroups];
        for (n, &name) in (0..ngroups).cycle().zip(names.iter()) {
            groups[n].push(name);
        }
        Roster { section, groups }
    }
}

pub struct NameList<'a> {
    section: Section,
    names: Vec<NameRef<'a>>
}

impl<'a> NameList<'a> {
    pub fn from_records(records: &[Record]) -> Vec<NameList> {
        let mut recs: Vec<_> = records.iter().collect();
        recs.sort_by_key(|&record| record.section);
        recs.iter()
            .group_by(|record| record.section)
            .into_iter()
            .map(|(section, names)| {
                let mut l = NameList {
                    section,
                    names: names.map(|&r| r.student.as_ref()).collect()
                };
                l.shuffle();
                l
            })
            .collect()
    }
}

impl<'a> NameList<'a> {
    fn shuffle(&mut self) {
        let mut rng = rand::thread_rng();
        self.names.shuffle(&mut rng);
    }
}
