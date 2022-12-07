// TODO: Cell should contain cell indices
// TODO: conditional formatting (when the relevant bindings are published)
extern crate calamine;
extern crate derive_more;
extern crate xlsxwriter;

use calamine::{Reader, open_workbook, Xlsx, DataType};
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

impl From<&DataType> for Content {
    fn from(datatype: &DataType) -> Self {
        match datatype {
            DataType::Int(i) => Content::Text(format!("{}", i)),
            DataType::Float(f) => Content::Float(*f),
            DataType::String(s) => Content::Text(s.to_owned()),
            DataType::Empty => Content::Empty,
            DataType::Bool(b) => Content::Text(format!("{}", b)),
            DataType::DateTime(d) => Content::Text(format!("{}", d)),
            DataType::Error(e) => Content::Text(format!("{}", e))
        }
    }
}

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
                freeze_row : Int.t Option.t;
                freeze_col : Int.t Option.t;
                data : xlsx_cell List.t List.t }")]
pub struct XlsxSheet {
    pub name: String,
    pub freeze_row: Option<usize>,
    pub freeze_col: Option<usize>,
    pub data: LinkedList<LinkedList<XlsxCell>>
}

#[derive(derive_more::Error, From, Display, Debug)]
pub enum XlsxError {
    #[display(fmt = "integer out of acceptable range: {}", "_0")]
    IntegerOverflow(std::num::TryFromIntError),
    #[display(fmt = "{}", "format!(\"xlsx writer error: {:?}\", _0.to_string())")]
    XlsxWriteError(xlsxwriter::XlsxError),
    #[display(fmt = "{}", "format!(\"xlsx read error: {:?}\", _0)")]
    XlsxReadError(calamine::XlsxError),
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
            match (sheet.freeze_row, sheet.freeze_col) {
                (Some(i), Some(j)) => ws.freeze_panes(i.try_into()?, j.try_into()?),
                (Some(i), None) => ws.freeze_panes(i.try_into()?, 0),
                (None, Some(j)) => ws.freeze_panes(0, j.try_into()?),
                (None, None) => ()
            };
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

    fn read(filename: &str) -> Result<Self> {
        let mut sheets = Vec::new();
        let mut workbook: Xlsx<_> = open_workbook(filename)?;
        for (s, table) in workbook.worksheets().iter() {
            let mut list = Vec::new();
            for row in table.rows() {
                let mut cells = Vec::new();
                for cell in row.iter() {
                    let typography = None;
                    let color = Color::Black;
                    let font = "Calibri".to_string();
                    let content = cell.into();
                    cells.push(XlsxCell { typography, color, font, content });
                }
                list.push(cells.into_iter().collect::<LinkedList<_>>())
            }
            let data = list.into_iter().collect::<LinkedList<_>>();
            let name = s.to_string();
            let freeze_row = None;
            let freeze_col = None;
            sheets.push(XlsxSheet { data, freeze_col, freeze_row, name });
        }
        Ok(XlsxWorkbook { sheets: sheets.into_iter().collect() })
    }
}

#[ocaml::func]
#[ocaml::sig("String.t -> xlsx_sheet List.t -> (unit, String.t) Result.t")]
pub fn write_xlsx(filename: String, sheets: XlsxWorkbook)
    -> std::result::Result<(), String> {
    sheets.write(&filename).map_err(|e| format!("{:?}", e))
}

#[ocaml::func]
#[ocaml::sig("String.t -> (xlsx_sheet List.t, String.t) Result.t")]
pub fn read_xlsx(filename: String) -> std::result::Result<XlsxWorkbook, String> {
    XlsxWorkbook::read(&filename).map_err(|e| format!("{:?}", e))
}
