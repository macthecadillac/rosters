use derive_more::{From, Add, AddAssign, Div, Mul, Sub, SubAssign, Sum};
use owned_ttf_parser::GlyphId;
use printpdf::{BlendMode, Color, Cmyk, IndirectFontRef, LineCapStyle, Mm,
               PdfDocumentReference, PdfLayerReference, SeperableBlendMode};
use subsetter::{subset, Profile};

use std::collections::HashSet;

use crate::data::{Checkpoint, Lab, NameRef, Roster, Section};

const REGULAR_FONT: &'static [u8] = std::include_bytes!("../fonts/Carlito-Regular.ttf");
const BOLD_FONT: &'static [u8] = std::include_bytes!("../fonts/Carlito-Bold.ttf");

const PAGEWIDTH: Length = Length(215.9);   // 8.5 in
const PAGEHEIGHT: Length = Length(279.4);  // 11 in
const MARGINS: Length = Length(20.32);     // 0.85 in

#[derive(Copy, Clone, Eq, PartialEq)]
#[allow(dead_code)]
pub enum Font { Regular, Bold }

impl Into<&'static [u8]> for Font {
    fn into(self) -> &'static [u8] {
        match self {
            Font::Regular => REGULAR_FONT,
            Font::Bold => BOLD_FONT
        }
    }
}

#[derive(Copy, Clone)]
enum Width { Auto, Manual(Length) }

impl Width {
    fn width(self, text: &str, font: Font, size: f64) -> Length {
        match self {
            Width::Auto => {
                let face = owned_ttf_parser::Face::parse(font.into(), 0).unwrap();
                let length = text.chars()
                    .filter_map(|c| face.glyph_index(c))
                    .filter_map(|glyph| Some(
                        face.glyph_hor_advance(glyph)? as i16 +
                        face.glyph_hor_side_bearing(glyph)?
                    ))
                    .sum::<i16>();
                Length::from_pt(size) * length as f64 / 2048.
            },
            Width::Manual(l) => l
        }
    }
}

#[derive(Add, Copy, Clone, Sub)]
struct Vector { x: Length, y: Length }

impl Into<printpdf::Point> for Vector {
    fn into(self) -> printpdf::Point {
        printpdf::Point::new(self.x(), self.y())
    }
}

impl Vector {
    fn from_ul(x: Length, y: Length) -> Self {
        Vector { x: x + MARGINS, y: MARGINS + y }
    }

    fn x(self) -> Mm { self.x.into() }

    fn y(self) -> Mm { (Length::from_in(11.) - self.y).into() }
}

#[derive(Copy, Clone)]
enum Direction { Horizontal, Vertical }

#[derive(Copy, Clone)]
struct Line {
    anchor: Vector,
    length: Length,
    direction: Direction,
    thickness: Length
}

impl Line {
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
        layer_ref.set_line_cap_style(LineCapStyle::ProjectingSquare);
        layer_ref.set_fill_color(fill_color);
        layer_ref.set_outline_thickness(self.thickness.to_pt());
        layer_ref.add_shape(line);
    }
}

fn line_height(size: f64) -> Length {
    let face = owned_ttf_parser::Face::parse(REGULAR_FONT, 0).unwrap();
    Length::from_pt(size) * face.height() as f64 / 2048.
}

#[derive(Add, AddAssign, Clone, Copy, Debug, From, Mul, Default, Div, Sub, SubAssign, Sum)]
pub struct Length(f64);

impl Into<Mm> for Length {
    fn into(self) -> Mm { Mm(self.0) }
}

impl Length {
    fn from_in(l: f64) -> Length { Length(l * 25.4) }
    fn from_mm(l: f64) -> Length { Length(l) }
    fn from_pt(l: f64) -> Length { Length(l * 0.34) }
    fn to_pt(self) -> f64 { self.0 / 0.34 }
}

#[derive(Copy, Clone)]
pub struct FontRef<'a> {
    pub regular: &'a IndirectFontRef,
    pub bold: &'a IndirectFontRef
}

impl<'a> FontRef<'a> {
    fn pick(self, font: Font) -> &'a IndirectFontRef {
        match font {
            Font::Regular => self.regular,
            Font::Bold => self.bold
        }
    }
}

struct Text {
    str: String,
    anchor: Vector,  // upper left corner
    size: f64,
    font: Font
}

impl Text {
    fn render(&self, layer_ref: &PdfLayerReference, font_ref: FontRef) {
        let text_height = line_height(self.size).into();
        layer_ref.use_text(&self.str, self.size, self.anchor.x(),
                           self.anchor.y() - text_height, font_ref.pick(self.font));
    }
}

#[derive(Copy, Clone, Default, Debug)]
struct Column { left: Length, right: Length }

impl Column {
    fn anchor_center(self, text: &str, font: Font, size: f64) -> Length {
        let width = Width::Auto.width(text, font, size);
        self.left + (self.right - self.left - width) * 0.5
    }

    fn anchor_left(self) -> Length {
        self.left + Length::from_pt(6.)
    }
}

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
    fn render(&self, pdf_document: &mut PdfDocumentReference, font_ref: FontRef) {
        let (page, layer) = pdf_document.add_page(PAGEWIDTH.into(),
                                                  PAGEHEIGHT.into(),
                                                  &self.title);
        let layer_ref = pdf_document.get_page(page).get_layer(layer);
        for item in self.text.iter() {
            item.render(&layer_ref, font_ref);
        }
        for line in self.lines.iter() {
            line.render(&layer_ref);
        }
    }

    fn glyphs(&self, font: Font) -> HashSet<GlyphId> {
        let face = owned_ttf_parser::Face::parse(font.into(), 0).unwrap();
        self.text.iter()
            .filter(|text| text.font == font)
            .flat_map(|text| text.str.chars())
            .filter_map(|c| face.glyph_index(c))
            .collect()
    }

    fn add_header(&mut self, lab: Lab, section: Section,
                      checkpoints: &[Checkpoint]) {
        let text_width = PAGEWIDTH - MARGINS * 2.;
        let size = self.font_size;
        let line_height = line_height(size);
        let x0 = Length::default();
        let mut y0 = Length::default();
        self.lines.push(Line {
            anchor: Vector::from_ul(x0, y0),
            length: text_width,
            direction: Direction::Horizontal,
            thickness: Length::from_pt(2.)
        });
        // line width of 2pt
        y0 += Length::from_pt(3.);
        let anchor1 = Vector::from_ul(x0 + Length::from_pt(6.), y0);
        self.text.push(Text { str: format!("Lab {}", lab), size, font: Font::Bold,
                              anchor: anchor1 });
        let section_str = format!("Section {}", section);
        let section_str_width = Width::Auto.width(&section_str, Font::Bold, size);
        let x = (text_width - section_str_width - Length::from_mm(2.)) * 0.5;
        self.text.push(Text { str: section_str, size, font: Font::Bold,
                              anchor: Vector::from_ul(x, y0) });
        self.text.push(Text { str: "Date:".into(), size, font: Font::Bold,
                              anchor: Vector::from_ul(Length::from_in(4.8), y0) });
        y0 += line_height + Length::from_pt(4.);

        self.lines.push(Line {
            anchor: Vector::from_ul(x0, y0),
            length: text_width,
            direction: Direction::Horizontal,
            thickness: Length::from_pt(2.)
        });
        y0 += Length::from_pt(2.);
        self.table_start = y0;
        y0 += Length::from_pt(1.);

        let left_header_text = [(Width::Manual(Length::from_mm(20.)), "Signature"),
                                (Width::Auto, "Late"),
                                (Width::Auto, "Group")];
        let right_header_text: Vec<_> = checkpoints.iter()
            .map(|x| (Width::Auto, x.as_ref()))
            .chain([(Width::Auto, "Signed")].into_iter())
            .collect();
        let lwidths: Vec<_> = left_header_text.iter()
            .map(|(w, x)| w.width(x, Font::Bold, size) + Length::from_pt(12.))
            .collect();
        let rwidths: Vec<_> = right_header_text.iter()
            .map(|(w, x)| w.width(x, Font::Bold, size) + Length::from_pt(12.))
            .collect();
        let center_width = text_width
            - lwidths.iter().chain(rwidths.iter()).cloned().sum::<Length>();
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
            let x = column.anchor_center(text, Font::Bold, size);
            self.text.push(Text { str: text.into(), size, font: Font::Bold,
                anchor: Vector::from_ul(x, y0) });
        };
        y0 += line_height + Length::from_pt(4.);
        self.lines.push(Line {
            anchor: Vector::from_ul(x0, y0),
            length: text_width,
            direction: Direction::Horizontal,
            thickness: Length::from_pt(2.)
        });
        self.table_height = y0 + Length::from_pt(3.);
    }

    fn add_group(&mut self, group: usize, students: &[NameRef]) {
        let y0 = self.table_height;
        let size = self.font_size;
        let line_height = line_height(size);
        let group_col = self.columns[2];
        let student_col = self.columns[3];
        let group_size = students.len();
        for (n, student) in students.iter().enumerate() {
            let text = format!("{}", student);
            let x = student_col.anchor_left();
            self.text.push(Text { str: text, size, font: Font::Regular,
                anchor: Vector::from_ul(x, self.table_height) });
            self.table_height += line_height + Length::from_pt(4.);
            let bottom_line = Line {
                anchor: Vector::from_ul(Length::default(), self.table_height),
                length: PAGEWIDTH - MARGINS * 2.,
                direction: Direction::Horizontal,
                thickness: Length::from_pt(1.3)
            };
            if n < group_size - 1 {
                self.lines.push(Line {
                    anchor: Vector::from_ul(Length::default(), self.table_height),
                    length: group_col.left,
                    direction: Direction::Horizontal,
                    thickness: Length::from_pt(1.)
                });
                self.lines.push(Line {
                    anchor: Vector::from_ul(student_col.left, self.table_height),
                    length: PAGEWIDTH - MARGINS * 2. - student_col.left,
                    direction: Direction::Horizontal,
                    thickness: Length::from_pt(1.)
                });
                self.table_height += Length::from_pt(2.);
            } else if group < self.ngroups {
                self.lines.push(bottom_line);
            } else {
                self.lines.push(Line { thickness: Length::from_pt(2.), ..bottom_line });
            }
        }
        let text = format!("{}", group);
        let x = group_col.anchor_center(&text, Font::Bold, size);
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
    }

    fn add_vertical_lines(&mut self) {
        for column in self.columns.iter().skip(1) {
            let x = column.left;
            self.lines.push(Line {
                anchor: Vector::from_ul(x - Length::from_pt(0.5), self.table_start),
                length: self.table_height - self.table_start,
                direction: Direction::Vertical,
                thickness: Length::from_pt(1.)
            });
        }
        self.lines.push(Line {
            anchor: Vector::from_ul(Length::default() - Length::from_pt(1.), Length::default()),
            length: self.table_height,
            direction: Direction::Vertical,
            thickness: Length::from_pt(2.)
        });
        self.lines.push(Line {
            anchor: Vector::from_ul(PAGEWIDTH - MARGINS * 2., Length::default()),
            length: self.table_height,
            direction: Direction::Vertical,
            thickness: Length::from_pt(2.)
        });
    }
}

#[derive(Default)]
pub struct Document {
    pages: Vec<Page>,
    regular_glyphs: HashSet<u16>,
    bold_glyphs: HashSet<u16>,
}

impl Document {
    pub fn add_page(&mut self, roster: &Roster, lab: Lab, checkpoints: &[Checkpoint]) {
        let mut page = Page::default();
        page.font_size = 11.;
        page.ngroups = roster.groups.len();
        page.title = format!("Section {}", roster.section);
        page.add_header(lab, roster.section, checkpoints);
        for (group, students) in roster.groups.iter().enumerate() {
            page.add_group(group + 1, students);
        }
        page.add_vertical_lines();
        self.regular_glyphs.extend(page.glyphs(Font::Regular).into_iter().map(|g| g.0));
        self.bold_glyphs.extend(page.glyphs(Font::Bold).into_iter().map(|g| g.0));
        self.pages.push(page)
    }

    pub fn font_subset(&self, font: Font) -> Vec<u8> {
        let glyphs = match font {
            Font::Regular => &self.regular_glyphs,
            Font::Bold => &self.bold_glyphs
        };
        let glyph_ids: Vec<_> = glyphs.iter().cloned().collect();
        subset(font.into(), 0, Profile::pdf(&glyph_ids)).unwrap()
    }

    pub fn render(&self, pdf_document: &mut PdfDocumentReference, font_ref: FontRef) {
        for page in self.pages.iter() {
            page.render(pdf_document, font_ref);
        }
    }
}