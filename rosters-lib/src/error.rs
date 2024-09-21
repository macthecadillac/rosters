use derive_more::{Display, Error, From};
use crate::data::Section;

/// The Error type
#[derive(Debug, Display, From, Error)]
#[error(ignore)]  // don't derive backtrace/source
pub enum Error {
    #[display(fmt="name not formatted as 'last, first': {}", _0)]
    #[from(ignore)]
    ParseNameError(String),
    #[display(fmt="no valid record found in the data file")]
    NoRecordFound,
    #[display(fmt="cannot parse lab number from key: {}", _0)]
    UnknownLabPrefix(String),
    #[display(fmt="cannot parse lab number from key")]
    UnknownLabNumber(std::num::ParseIntError),
    #[display(fmt="section {} does not exist in the input data. Check your configuration/input data.", _0)]
    #[from(ignore)]
    NonexistentSection(Section),
    PdfError(printpdf::Error),
    IOError(std::io::Error),
    XlsxError(rust_xlsxwriter::XlsxError),
    #[display(fmt="the name \"{}\" is too long and overflowed the stack", _0)]
    #[from(ignore)]
    NameTooLongError(String),
    #[display(fmt="font parsing error")]
    FontParsingError(owned_ttf_parser::FaceParsingError),
    #[display(fmt="font subset error")]
    SubsetError(subsetter::Error)
}
