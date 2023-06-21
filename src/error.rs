use derive_more::{Display, Error, From};

#[derive(Debug, Display, From, Error)]
pub enum Error {
    #[display(fmt = "name not formatted as 'last, first'")]
    ParseNameError,
    #[display(fmt = "cannot parse lab number from key")]
    UnknownLabPrefix,
    #[display(fmt = "cannot parse lab number from key")]
    UnknownLabNumber(std::num::ParseIntError),
    PdfError(printpdf::Error),
    IOError(std::io::Error),
    XlsxError(rust_xlsxwriter::XlsxError),
    #[display(fmt = "section size of over 42 is not supported")]
    SectionSizeError(arrayvec::CapacityError),
    #[display(fmt = "font parsing error")]
    FontParsingError(owned_ttf_parser::FaceParsingError),
    #[display(fmt = "font subset error")]
    SubsetError(subsetter::Error)
}
