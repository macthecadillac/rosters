use derive_more::{Display, Error, From};

#[derive(Debug, Display, From, Error)]
pub enum Error {
    #[display(fmt = "name not formatted as 'last, first'")]
    ParseNameError,
    #[display(fmt = "cannot parse lab number from key")]
    UnknownLabPrefix,
    #[display(fmt = "cannot parse lab number from key")]
    UnknownLabNumber(std::num::ParseIntError),
    #[display(fmt = "could not open configuration file: {}", _0)]
    OpenFileError(opener::OpenError),
    #[display(fmt = "encountered error during IO operation: {}", _0)]
    IOError(std::io::Error),
    #[display(fmt = "xlsx error: {}", _0)]
    XlsxError(rust_xlsxwriter::XlsxError)
}
