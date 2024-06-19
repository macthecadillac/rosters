use derive_more::{From, Add, AddAssign, Div, Mul, Sub, SubAssign, Sum};
use owned_ttf_parser::{Face, GlyphId};
use printpdf::{BlendMode, Color, Cmyk, IndirectFontRef, LineCapStyle, Mm,
               PdfDocumentReference, PdfLayerReference, SeperableBlendMode};
use subsetter::{subset, Profile};

use std::collections::HashSet;
use std::iter;

use crate::data::{Checkpoint, Lab, Roster, Section, Student};
use crate::error::Error;

/// Embedded regular font. This font will be embedded into the PDF we generate
const REGULAR_FONT: &'static [u8] = std::include_bytes!("../fonts/Carlito-Regular.ttf");
/// Embedded bold font. This font will be embedded into the PDF we generate
const BOLD_FONT: &'static [u8] = std::include_bytes!("../fonts/Carlito-Bold.ttf");

/// Width of a standard letter-sized page
const PAGEWIDTH: Length = Length(215.9);   // 8.5 in
/// Height of a standard letter-sized page
const PAGEHEIGHT: Length = Length(279.4);  // 11 in
/// Page margins
const MARGINS: Length = Length(20.32);     // 0.85 in
/// Font size
const FONTSIZE: f64 = 11.;

/// Alias of two font types
#[derive(Copy, Clone, Eq, PartialEq)]
#[allow(dead_code)]
pub(crate) enum Font { Regular, Bold }

impl Into<&'static [u8]> for Font {
    fn into(self) -> &'static [u8] {
        match self {
            Font::Regular => REGULAR_FONT,
            Font::Bold => BOLD_FONT
        }
    }
}

/// Auto width calculation vs. a user provided width
#[derive(Copy, Clone)]
enum Width { Auto, Manual(Length) }

impl Width {
    /// Calculates the actual width of a textbox
    fn width(self, text: &str, font: Font, size: f64) -> Result<Length, Error> {
        match self {
            Width::Auto => {
                let face = Face::parse(font.into(), 0)?;
                let length = text.chars()
                    .filter_map(|c| face.glyph_index(c))
                    .filter_map(|glyph| Some(
                        face.glyph_hor_advance(glyph)? as i16 +
                        face.glyph_hor_side_bearing(glyph)?
                    ))
                    .sum::<i16>();
                Ok(Length::from_pt(size) * length as f64 / 2048.)
            },
            Width::Manual(l) => Ok(l)
        }
    }
}

/// A Cartesian vector struct
#[derive(Add, Copy, Clone, Sub)]
struct Vector { x: Length, y: Length }

impl Into<printpdf::Point> for Vector {
    fn into(self) -> printpdf::Point {
        printpdf::Point::new(self.x(), self.y())
    }
}

impl Vector {
    /// Calculate the vector that points to the a given point from the upper-left corner
    fn from_ul(x: Length, y: Length) -> Self {
        Vector { x: x + MARGINS, y: MARGINS + y }
    }

    /// The x-coordinates
    fn x(self) -> Mm { self.x.into() }

    /// The y-coordinates
    fn y(self) -> Mm { (Length::from_in(11.) - self.y).into() }
}

/// Direction of a line
#[derive(Copy, Clone)]
enum Direction { Horizontal, Vertical }

/// A set of instructions for drawing straight lines
#[derive(Copy, Clone)]
struct Line {
    anchor: Vector,
    length: Length,
    direction: Direction,
    thickness: Length,
    cap: LineCapStyle
}

impl Line {
    /// Render the line onto the PDF
    fn render(&self, layer_ref: &PdfLayerReference) {
        let start = Vector {
            x: self.anchor.x,
            y: self.anchor.y + self.thickness
        };
        let end = match self.direction {
            Direction::Horizontal => Vector {
                x: self.anchor.x + self.length,
                y: self.anchor.y + self.thickness
            },
            Direction::Vertical => Vector {
                x: self.anchor.x,
                y: self.anchor.y + self.length
            }
        };
        let line = printpdf::Line {
            points: vec![(start.into(), true), (end.into(), true)],
            is_closed: false,
            has_fill: false,
            has_stroke: true,
            is_clipping_path: false,
        };
        let fill_color = Color::Cmyk(Cmyk::new(0.0, 0.0, 0.0, 0.0, None));
        layer_ref.set_blend_mode(BlendMode::Seperable(SeperableBlendMode::Normal));
        layer_ref.set_line_cap_style(self.cap);
        layer_ref.set_fill_color(fill_color);
        layer_ref.set_outline_thickness(self.thickness.to_pt());
        layer_ref.add_shape(line);
    }
}

/// Calculate the line height of a given font size for the embedded font
fn line_height(size: f64) -> Result<Length, Error> {
    let face = Face::parse(REGULAR_FONT, 0)?;
    Ok(Length::from_pt(size) * face.height() as f64 / 2048.)
}

/// The memory representation of a unit-agnostic length
#[derive(Add, AddAssign, Clone, Copy, Debug, From, Mul, Default, Div,
         PartialOrd, PartialEq, Sub, SubAssign, Sum)]
pub(crate) struct Length(f64);

impl Into<Mm> for Length {
    fn into(self) -> Mm { Mm(self.0) }
}

impl Length {
    /// Convert inches to `Length`
    fn from_in(l: f64) -> Length { Length(l * 25.4) }
    /// Convert millimeters to `Length`
    fn from_mm(l: f64) -> Length { Length(l) }
    /// Convert points to `Length`
    fn from_pt(l: f64) -> Length { Length(l * 0.34) }
    /// Convert `Length` to points
    fn to_pt(self) -> f64 { self.0 / 0.34 }
}

/// A struct that holds pointers to the fonts within the PDF file
#[derive(Copy, Clone)]
pub(crate) struct FontRef<'a> {
    pub(crate) regular: &'a IndirectFontRef,
    pub(crate) bold: &'a IndirectFontRef
}

impl<'a> FontRef<'a> {
    /// Given a `Font`, return a PDF font reference
    fn pick(self, font: Font) -> &'a IndirectFontRef {
        match font {
            Font::Regular => self.regular,
            Font::Bold => self.bold
        }
    }
}

/// A set of instructions for rendering text
struct Text {
    str: String,
    anchor: Vector,  // upper left corner
    size: f64,
    font: Font
}

impl Text {
    /// Render the text onto the PDF file
    fn render(&self, layer_ref: &PdfLayerReference, font_ref: FontRef)
        -> Result<(), Error> {
        let text_height = line_height(self.size)?.into();
        layer_ref.use_text(&self.str, self.size, self.anchor.x(),
                           self.anchor.y() - text_height, font_ref.pick(self.font));
        Ok(())
    }
}

/// A representation of a column in the PDF file
#[derive(Copy, Clone, Default, Debug)]
struct Column {
    /// The x-coordinates of the left margin of a column
    left: Length,
    /// The x-coordinates of the right margin of a column
    right: Length
}

impl Column {
    /// Calculates the relative coordinates of the left edge of a given piece of text if the text
    /// is to be centered in the column
    fn anchor_center(self, text: &str, font: Font, size: f64) -> Result<Length, Error> {
        let width = Width::Auto.width(text, font, size)?;
        Ok(self.left + (self.right - self.left - width) * 0.5)
    }

    /// Calculates the relative coordinates of the left edge of a given piece of text if the text
    /// is to be aligned to the left edge of the column
    fn anchor_left(self, size: f64) -> Length {
        self.left + Length::from_pt(size / FONTSIZE * 6.)
    }
}

/// A set of instructions for rendering a full page
#[derive(Default)]
struct Page {
    columns: Vec<Column>,
    text: Vec<Text>,
    lines: Vec<Line>,
    table_start: Length,
    table_height: Length,
    font_size: f64,
    ngroups: usize,
    title: String
}

impl Page {
    /// Render the page onto the PDF file
    fn render(&self, pdf_document: &mut PdfDocumentReference, font_ref: FontRef)
        -> Result<(), Error> {
        let (page, layer) = pdf_document.add_page(PAGEWIDTH.into(),
                                                  PAGEHEIGHT.into(),
                                                  &self.title);
        let layer_ref = pdf_document.get_page(page).get_layer(layer);
        for item in self.text.iter() {
            item.render(&layer_ref, font_ref)?;
        }
        for line in self.lines.iter() {
            line.render(&layer_ref);
        }
        Ok(())
    }

    /// Return the set of glyphs present on the page. This is needed for font subsetting.
    fn glyphs(&self, font: Font) -> Result<HashSet<GlyphId>, Error> {
        let face = Face::parse(font.into(), 0)?;
        Ok(self.text.iter()
            .filter(|text| text.font == font)
            .flat_map(|text| text.str.chars())
            .filter_map(|c| face.glyph_index(c))
            .collect())
    }

    /// Add a header to the page
    fn add_header(&mut self, lab: Lab, section: Section,
                  checkpoints: &[Checkpoint],
                  roster: &Roster) -> Result<(), Error> {
        let text_width = PAGEWIDTH - MARGINS * 2.;
        let size = self.font_size;
        let line_height = line_height(size)?;
        let side_padding = Length::from_pt(6.);
        let top_padding = Length::from_pt(size / 22.);
        let bottom_padding = Length::from_pt(size / FONTSIZE * 4.5);
        let x0 = Length::default();
        let mut y0 = Length::default();
        let thickness = Length::from_pt(2.);
        self.lines.push(Line {
            anchor: Vector::from_ul(x0, y0),
            length: text_width,
            direction: Direction::Horizontal,
            cap: LineCapStyle::Butt,
            thickness
        });
        y0 += thickness + top_padding;
        let anchor1 = Vector::from_ul(x0 + side_padding, y0);
        self.text.push(Text { str: format!("Lab {}", lab), size, font: Font::Bold,
                              anchor: anchor1 });
        let section_str = format!("Section {}", section);
        let section_str_width = Width::Auto.width(&section_str, Font::Bold, size)?;
        let x = (text_width - section_str_width) * 0.5;
        self.text.push(Text { str: section_str, size, font: Font::Bold,
                              anchor: Vector::from_ul(x, y0) });
        self.text.push(Text { str: "Date:".into(), size, font: Font::Bold,
                              anchor: Vector::from_ul(Length::from_in(4.8), y0) });
        y0 += line_height + bottom_padding;

        let thickness = Length::from_pt(2.);
        self.lines.push(Line {
            anchor: Vector::from_ul(x0, y0),
            length: text_width,
            direction: Direction::Horizontal,
            cap: LineCapStyle::Butt,
            thickness
        });
        y0 += thickness;
        self.table_start = y0;
        y0 += top_padding;

        let min_col_width = Length::from_mm(9.);
        let left_header_text = [(Width::Manual(Length::from_mm(20.)), "Signature"),
                                (Width::Auto, "Late"),
                                (Width::Auto, "Group")];
        let right_header_ = checkpoints.iter()
            .map(|x| (Width::Auto, x.as_ref()))
            .chain([(Width::Auto, "Signed")].into_iter());
        let right_header_text: Vec<_> = if roster.name_clash {
            let lens: Vec<_> = roster.students.iter()
                .map(|s| Width::Auto.width(&s.sid.0, Font::Regular, size))
                .collect::<Result<_, _>>()?;
            let len = lens.iter().fold(0., |a, &b| f64::max(a, b.0));
            iter::once((Width::Manual(Length(len)), "SID"))
                .chain(right_header_).collect()
        } else {
            right_header_.collect()
        };
        let lwidths: Vec<Length> = left_header_text.iter()
            .map(|(w, x)| w.width(x, Font::Bold, size))
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .map(|w| w + side_padding * 2.)
            .collect();
        let rwidths: Vec<_> = right_header_text.iter()
            .map(|(w, x)| w.width(x, Font::Bold, size))
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .map(|w| w + side_padding * 2.)
            .map(|w| if w < min_col_width { min_col_width } else { w })
            .collect();
        let lwidth = lwidths.iter().copied().sum();
        let rwidth = rwidths.iter().copied().sum();
        let center_width = text_width - lwidth - rwidth;
        self.columns = lwidths.iter()
            .chain([&center_width].into_iter())
            .chain(rwidths.iter())
            .scan(Column::default(), |acc, &w| {
                let c = Column { left: acc.right, right: w + acc.right };
                *acc = c;
                Some(c)
            })
            .collect();
        for (text, &column) in left_header_text.into_iter()
                                .map(|(_, s)| s)
                                .chain(["Student"].into_iter())
                                .chain(right_header_text.into_iter().map(|(_, s)| s))
                                .zip(self.columns.iter()) {
            let x = column.anchor_center(text, Font::Bold, size)?;
            self.text.push(Text { str: text.into(), size, font: Font::Bold,
                anchor: Vector::from_ul(x, y0) });
        };
        y0 += line_height + bottom_padding;
        let thickness = Length::from_pt(2.);
        self.lines.push(Line {
            anchor: Vector::from_ul(x0, y0),
            length: text_width,
            direction: Direction::Horizontal,
            cap: LineCapStyle::Butt,
            thickness
        });
        self.table_height = y0 + thickness;
        Ok(())
    }

    /// Add a group to the roster on the page
    fn add_group(&mut self, group: usize, students: &[Student],
                 show_sid: bool) -> Result<(), Error> {
        let y0 = self.table_height;
        let size = self.font_size;
        let line_height = line_height(size)?;
        let top_padding = Length::from_pt(0.);
        let bottom_padding = Length::from_pt(size / FONTSIZE * 5.);
        let group_col = self.columns[2];
        let student_col = self.columns[3];
        let sid_col = if show_sid { Some(self.columns[4]) } else { None };
        let group_size = students.len();
        let stdt_anchor = student_col.anchor_left(size);
        for (n, student) in students.iter().enumerate() {
            let name = format!("{}", student.name);
            self.text.push(Text { str: name, size, font: Font::Regular,
                anchor: Vector::from_ul(stdt_anchor, self.table_height) });
            if show_sid {
                let sid = format!("{}", student.sid);
                let anchor = sid_col.unwrap().anchor_center(&sid, Font::Regular, size)?;
                self.text.push(Text { str: sid, size, font: Font::Regular,
                    anchor: Vector::from_ul(anchor, self.table_height) });
            }
            self.table_height += line_height + bottom_padding;
            let bottom_line = Line {
                anchor: Vector::from_ul(Length::default(), self.table_height),
                length: PAGEWIDTH - MARGINS * 2.,
                direction: Direction::Horizontal,
                cap: LineCapStyle::Butt,
                thickness: Length::from_pt(1.3)
            };
            if n < group_size - 1 {
                let thickness = Length::from_pt(1.);
                self.lines.push(Line {
                    anchor: Vector::from_ul(Length::default(), self.table_height),
                    length: group_col.left,
                    direction: Direction::Horizontal,
                    cap: LineCapStyle::Butt,
                    thickness
                });
                self.lines.push(Line {
                    anchor: Vector::from_ul(student_col.left, self.table_height),
                    length: PAGEWIDTH - MARGINS * 2. - student_col.left,
                    direction: Direction::Horizontal,
                    cap: LineCapStyle::Butt,
                    thickness
                });
                self.table_height += thickness + top_padding;
            } else if group < self.ngroups {
                self.lines.push(bottom_line);
            } else {
                self.lines.push(Line { thickness: Length::from_pt(2.), ..bottom_line });
            }
        }
        let text = format!("{}", group);
        let x = group_col.anchor_center(&text, Font::Bold, size)?;
        let y = y0 - Length::from_pt(2.) + (self.table_height - y0 - line_height) * 0.5;
        self.text.push(Text {
            str: text, size, font: Font::Bold,
            anchor: Vector::from_ul(x, y)
        });
        if group < self.ngroups {
            self.table_height += Length::from_pt(1.3)
        } else if group == self.ngroups {
            self.table_height += Length::from_pt(2.)
        };
        Ok(())
    }

    /// Add vertical lines to the page
    fn add_vertical_lines(&mut self) {
        for column in self.columns.iter().skip(1) {
            let x = column.left;
            self.lines.push(Line {
                anchor: Vector::from_ul(x - Length::from_pt(0.5), self.table_start),
                length: self.table_height - self.table_start,
                direction: Direction::Vertical,
                cap: LineCapStyle::ProjectingSquare,
                thickness: Length::from_pt(1.)
            });
        }
        self.lines.push(Line {
            anchor: Vector::from_ul(Length::default() - Length::from_pt(1.), Length::default()),
            length: self.table_height,
            direction: Direction::Vertical,
            cap: LineCapStyle::ProjectingSquare,
            thickness: Length::from_pt(2.)
        });
        self.lines.push(Line {
            anchor: Vector::from_ul(PAGEWIDTH - MARGINS * 2. - Length::from_pt(1.), Length::default()),
            length: self.table_height,
            direction: Direction::Vertical,
            cap: LineCapStyle::ProjectingSquare,
            thickness: Length::from_pt(2.)
        });
    }
}

/// A set of instructions for a full PDF document
#[derive(Default)]
pub(crate) struct Document {
    /// Pages within the document
    pages: Vec<Page>,
    /// Glyphs that need to be rendered with the regular font
    regular_glyphs: HashSet<u16>,
    /// Glyphs that need to be rendered with the bold font
    bold_glyphs: HashSet<u16>,
}

impl Document {
    /// Compute the font size based on the number of lines that needs to be rendered onto the page
    fn compute_font_size(&self, roster: &Roster) -> Result<f64, Error> {
        let face = Face::parse(REGULAR_FONT, 0)?;
        let ngroups = roster.ngroups();
        let nrows = roster.students.len();
        let th = PAGEHEIGHT - MARGINS * 2.;
        let group_sep = Length::from_pt(1.3) * (ngroups - 1) as f64;
        let name_sep = Length::from_pt(1.) * (nrows - ngroups - 1) as f64;
        let thick_sep = Length::from_pt(2.) * 3.;
        let max_row_height = (th - group_sep - name_sep - thick_sep) / (nrows + 2) as f64;
        let c = 2048. / face.height() as f64;
        let font_size = max_row_height.to_pt() * c * FONTSIZE / (FONTSIZE + 5. * c);
        Ok(if font_size <= FONTSIZE { font_size } else { FONTSIZE })
    }

    /// Add a page to the document
    pub(crate) fn add_page(&mut self, roster: &Roster, lab: Lab, checkpoints: &[Checkpoint])
        -> Result<(), Error> {
        let mut page = Page::default();
        page.font_size = self.compute_font_size(roster)?;
        page.ngroups = roster.ngroups();
        page.title = format!("Section {}", roster.section);
        page.add_header(lab, roster.section, checkpoints, roster)?;
        for (group, students) in roster.groups().enumerate() {
            page.add_group(group + 1, students, roster.name_clash)?;
        }
        page.add_vertical_lines();
        self.regular_glyphs.extend(page.glyphs(Font::Regular)?.into_iter().map(|g| g.0));
        self.bold_glyphs.extend(page.glyphs(Font::Bold)?.into_iter().map(|g| g.0));
        self.pages.push(page);
        Ok(())
    }

    /// Perform font subsetting to minimize PDF size
    pub(crate) fn font_subset(&self, font: Font) -> Result<Vec<u8>, Error> {
        let glyphs = match font {
            Font::Regular => &self.regular_glyphs,
            Font::Bold => &self.bold_glyphs
        };
        let glyph_ids: Vec<_> = glyphs.iter().copied().collect();
        let font = subset(font.into(), 0, Profile::pdf(&glyph_ids))?;
        Ok(font)
    }

    /// Render the document into a PDF file
    pub(crate) fn render(&self, pdf_document: &mut PdfDocumentReference, font_ref: FontRef)
        -> Result<(), Error> {
        for page in self.pages.iter() {
            page.render(pdf_document, font_ref)?;
        }
        Ok(())
    }
}
