use crate::data::{Lab, Roster, Section};
use crate::error::Error;

use std::path::PathBuf;

use rust_xlsxwriter::{Color, Format};

/// Workbook struct
#[derive(Default)]
pub(crate) struct Workbook {
    data: rust_xlsxwriter::Workbook
}

impl Workbook {
    /// Initialize Excel workbook
    pub(crate) fn initialize(&mut self, lab: Lab, sections: &[Section])
        -> Result<(), crate::error::Error> {
        let sheet = self.data.add_worksheet();
        sheet.write_string(0, 0, format!("{}", lab))?;
        sheet.write_string(0, 1, "Check if complete")?;
        for (row, section) in sections.iter().enumerate() {
            sheet.write_string(row as u32 + 1, 0, format!("section {}", section))?;
        }
        Ok(())
    }

    /// Add sheet to Excel workbook
    pub(crate) fn add_sheet(&mut self, roster: &Roster) -> Result<(), Error> {
        let sheet = self.data.add_worksheet();
        sheet.set_name(format!("section {}", roster.session.section))?;
        let red_text = Format::new().set_font_color(Color::Red).set_bold();
        let num_fmt = Format::new().set_num_format("0");
        sheet.write_string_with_format(0, 0, "\
            Under the \"Signature\" column, leave blank if present, enter \
            \"Absent\" if absent, describe circumstances if student left \
            soon after quiz",
            &red_text)?;
        sheet.write_string_with_format(1, 0, "\
            Under the \"Late\" column, enter the amount of time if they are late",
            &red_text)?;
        let mut row = 2;
        for (i, s) in ["Signature", "Late", "Group", "Student"].into_iter().enumerate() {
            sheet.write_string(row, i as u16, s)?;
        }
        row += 1;
        for (group, students) in roster.groups().enumerate() {
            for student in students.iter() {
                sheet.write_number_with_format(row, 2, group as f64 + 1., &num_fmt)?;
                sheet.write_string(row, 3, format!("{}", student))?;
                row += 1;
            }
        }
        Ok(())
    }

    /// Save Excel workbook
    pub(crate) fn save(&mut self, path: PathBuf) -> Result<(), Error> {
        self.data.save(path)?;
        Ok(())
    }
}
