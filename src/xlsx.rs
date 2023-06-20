use crate::Roster;

use rust_xlsxwriter::{Color, Format};

#[derive(Default)]
pub struct Workbook {
    pub data: rust_xlsxwriter::Workbook
}

impl Workbook {
    pub fn initialize(&mut self, lab: crate::Lab, sections: &[crate::Section]) -> Result<(), crate::error::Error> {
        let sheet = self.data.add_worksheet();
        sheet.write_string(0, 0, format!("Lab {}", lab))?;
        sheet.write_string(0, 1, "Check if complete")?;
        for (row, section) in sections.iter().enumerate() {
            sheet.write_string(row as u32 + 1, 0, format!("section {}", section))?;
        }
        Ok(())
    }
}

impl<'a> Roster<'a> {
    pub fn write(&self, workbook: &mut Workbook) -> Result<(), crate::error::Error> {
        let sheet = workbook.data.add_worksheet();
        sheet.set_name(format!("section {}", self.section))?;
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
        for (i, s) in ["Signature", "Late", "Group", "Student"].into_iter()
                                                               .enumerate() {
            sheet.write_string(row, i as u16, s)?;
        }
        row += 1;
        for (group, students) in self.groups.iter().enumerate() {
            for student in students.iter() {
                sheet.write_number_with_format(row, 2, group as f64 + 1., &num_fmt)?;
                sheet.write_string(row, 3, format!("{}", student))?;
                row += 1;
            }
        }
        Ok(())
    }
}
