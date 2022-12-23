// TODO: Cell should contain cell indices
// TODO: conditional formatting (when the relevant bindings are published)
extern crate derive_more;
extern crate xlsxwriter;

use derive_more::{Display, From};
use xlsxwriter::{Format, FormatColor, FormatUnderline, Workbook};

use std::collections::LinkedList;

#[derive(ocaml::ToValue, ocaml::FromValue, Clone, Copy)]
#[ocaml::sig("Bold | Italic | Underline")]
pub enum Typography { Bold, Italic, Underline }

#[derive(ocaml::ToValue, ocaml::FromValue, Clone, Copy)]
#[ocaml::sig("Red | Blue | Green | Black")]
pub enum Color { Red, Blue, Green, Black }

#[derive(ocaml::ToValue, ocaml::FromValue)]
#[ocaml::sig("Text of String.t | Float of float | Formula of String.t | Empty")]
pub enum Content { Text(String), Float(f64), Formula(String), Empty }

#[derive(ocaml::ToValue, ocaml::FromValue)]
#[ocaml::sig("{ typography : typography Option.t;
                color : color;
                font : String.t;
                content : content}")]
pub struct XlsxCell {
    pub typography: Option<Typography>,
    pub color: Color,
    pub font: String,
    pub content: Content
}

#[derive(ocaml::ToValue, ocaml::FromValue)]
#[ocaml::sig("{ name : String.t;
                data : xlsx_cell List.t List.t }")]
pub struct XlsxSheet {
    pub name: String,
    pub data: LinkedList<LinkedList<XlsxCell>>
}

#[derive(derive_more::Error, From, Display, Debug)]
pub enum XlsxError {
    #[display(fmt = "integer out of acceptable range: {}", "_0")]
    IntegerOverflow(std::num::TryFromIntError),
    #[display(fmt = "{}", "format!(\"xlsx writer error: {:?}\", _0.to_string())")]
    XlsxWriteError(xlsxwriter::XlsxError),
}

type Result<T> = std::result::Result<T, XlsxError>;

struct FormatBuilder<'a>{ format: Format<'a> }

impl<'a> FormatBuilder<'a> {
    fn new(workbook: &'a Workbook) -> Self {
        FormatBuilder { format: workbook.add_format() }
    }

    fn build(self) -> Format<'a> { self.format }

    fn set_font(self, font: &'a str) -> Self {
        FormatBuilder { format: self.format.set_font_name(font) }
    }

    fn maybe_set_type(self, typography: Option<Typography>) -> Self {
        let format = match typography {
            Some(Typography::Bold) => self.format.set_bold(),
            Some(Typography::Italic) => self.format.set_italic(),
            Some(Typography::Underline) => self.format.set_underline(FormatUnderline::Single),
            None => self.format
        };
        FormatBuilder { format }
    }

    fn set_color(self, color: Color) -> Self {
        let format = match color {
            Color::Red => self.format.set_font_color(FormatColor::Red),
            Color::Blue => self.format.set_font_color(FormatColor::Blue),
            Color::Green => self.format.set_font_color(FormatColor::Green),
            Color::Black => self.format.set_font_color(FormatColor::Black),
        };
        FormatBuilder { format }
    }
}

#[derive(ocaml::ToValue, ocaml::FromValue)]
#[ocaml::sig("{ sheets : xlsx_sheet List.t }")]
struct XlsxWorkbook { sheets: LinkedList<XlsxSheet> }

impl XlsxWorkbook {
    fn write(&self, filename: &str) -> Result<()> {
        let workbook = Workbook::new(filename)?;
        for sheet in self.sheets.iter() {
            let mut ws = workbook.add_worksheet(Some(&sheet.name))?;
            for (i_, row) in sheet.data.iter().enumerate() {
                let i = i_.try_into()?;
                for (j_, cell) in row.iter().enumerate() {
                    let j = j_.try_into()?;
                    let format = FormatBuilder::new(&workbook)
                        .set_font(&cell.font)
                        .set_color(cell.color)
                        .maybe_set_type(cell.typography)
                        .build();
                    match &cell.content {
                        Content::Text(s) => ws.write_string(i, j, &s, Some(&format))?,
                        Content::Float(f) => ws.write_number(i, j, *f, Some(&format))?,
                        Content::Formula(s) => ws.write_formula(i, j, &s, Some(&format))?,
                        Content::Empty => ()
                    }
                }
            }
        }
        workbook.close()?;
        Ok(())
    }
}

#[ocaml::func]
#[ocaml::sig("String.t -> xlsx_sheet List.t -> (unit, String.t) Result.t")]
pub fn write_xlsx(filename: String, sheets: XlsxWorkbook)
    -> std::result::Result<(), String> {
    sheets.write(&filename).map_err(|e| format!("{:?}", e))
}
