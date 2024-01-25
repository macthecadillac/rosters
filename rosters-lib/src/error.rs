use arrayvec::ArrayString;
use derive_more::{Display, Error, From};
use crate::data::{LABSTRMAXLEN, MAXGROUPSIZE, MAXNAMELEN, NGROUPS, Section};

/// The Error type
#[derive(Debug, Display, From, Error)]
#[error(ignore)]  // don't derive backtrace/source
pub(crate) enum Error {
    #[display(fmt="name not formatted as 'last, first': {}", _0)]
    #[from(ignore)]
    ParseNameError(ArrayString<MAXNAMELEN>),
    #[display(fmt="cannot parse lab number from key: {}", _0)]
    UnknownLabPrefix(ArrayString<LABSTRMAXLEN>),
    #[display(fmt="cannot parse lab number from key")]
    UnknownLabNumber(std::num::ParseIntError),
    #[display(fmt="section {} does not exist in the input data. Check your configuration/input data.", _0)]
    #[from(ignore)]
    NonexistentSection(Section),
    PdfError(printpdf::Error),
    IOError(std::io::Error),
    XlsxError(rust_xlsxwriter::XlsxError),
    #[display(fmt="section size of over {} is not supported", "MAXGROUPSIZE * NGROUPS")]
    SectionSizeError,
    #[display(fmt="the name \"{}\" is too long and overflowed the stack", _0)]
    #[from(ignore)]
    NameTooLongError(String),
    #[display(fmt="font parsing error")]
    FontParsingError(owned_ttf_parser::FaceParsingError),
    #[display(fmt="font subset error")]
    SubsetError(subsetter::Error)
}
