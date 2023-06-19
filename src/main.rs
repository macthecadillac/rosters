use clap::{Parser, Subcommand};
use main_error::MainError;
use printpdf::PdfDocument;
use std::collections::{HashMap, HashSet};
use std::env;
use std::fs;
use std::io::BufWriter;
use std::path::PathBuf;

mod data;
mod error;
mod pdf;
mod xlsx;

use data::{Checkpoint, Config, Lab, NameList, Roster, Section};
use directories::BaseDirs;
use pdf::{BOLD_FONT, REGULAR_FONT, Font, FontRef};
use subsetter::{subset, Profile};

const EXAMPLE_CONFIG: &'static str = std::include_str!("../example_config.toml");

#[derive(Debug, Subcommand)]
enum Subcmd {
    /// open configuration file in text editor
    Configure,
    /// generate rosters
    Generate {
        /// path to canvas exported csv file
        #[arg(short, long)]
        input: std::path::PathBuf,
        /// output directory
        #[arg(short, long)]
        output: Option<std::path::PathBuf>,
        /// lab number
        #[arg(long)]
        lab: usize,
        /// skip Excel file generation
        #[arg(long)]
        nox: bool
    },
    /// remove configuration
    Reset
}

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Args {
    #[command(subcommand)]
    command: Subcmd,
}

fn edit_config(config_dir: &std::path::Path) -> Result<(), error::Error> {
    let config_path = config_dir.join("rosters.txt");
    if config_path.exists() {
        opener::open(&config_path)?;
    } else {
        fs::create_dir_all(config_dir)?;
        fs::write(&config_path, EXAMPLE_CONFIG)?;
        opener::open(&config_path)?;
    };
    Ok(())
}

fn write_pdf<'a>(lab: Lab, checkpoints: &[Checkpoint], ta: &str,
                 rosters: impl Iterator<Item=&'a Roster<'a>>, pdf_dir: &PathBuf)
    -> Result<(), String> {
    let mut pages = vec![];
    let mut regular_glyphs = HashSet::new();
    let mut bold_glyphs = HashSet::new();
    for roster in rosters {
        let page = roster.build_page(lab.into(), &checkpoints);
        regular_glyphs.extend(page.glyphs(Font::Regular).into_iter().map(|g| g.0));
        bold_glyphs.extend(page.glyphs(Font::Bold).into_iter().map(|g| g.0));
        pages.push(page);
    }
    let regular_glyphs: Vec<_> = regular_glyphs.into_iter().collect();
    let regular_font = subset(REGULAR_FONT, 0, Profile::pdf(&regular_glyphs)).unwrap();
    let bold_glyphs: Vec<_> = bold_glyphs.into_iter().collect();
    let bold_font = subset(BOLD_FONT, 0, Profile::pdf(&bold_glyphs)).unwrap();
    let mut doc = PdfDocument::empty("Rosters");
    let regular = doc.add_external_font(&regular_font[..]).unwrap();
    let bold = doc.add_external_font(&bold_font[..]).unwrap();
    let font_ref = FontRef { regular: &regular, bold: &bold };
    for page in pages.iter() {
        page.render(&mut doc, font_ref);
    }
    let pdf_output = pdf_dir
        .join(format!("Lab {} Blank Rosters ({} Sections).pdf", lab, ta));
    let file = fs::File::create(pdf_output)
        .map_err(|_| "error encountered when creating PDF document")?;
    doc.save(&mut BufWriter::new(file))
       .map_err(|_| "error encountered when writing PDF document to file")?;
    Ok(())
}

fn write_xlsx<'a>(base_dir: &PathBuf, lab: Lab,
                  rosters: &[Roster]) -> Result<(), error::Error> {
    let mut workbook = xlsx::Workbook::default();
    let sections: Vec<_> = rosters.iter().map(|x| x.section).collect();
    workbook.initialize(lab.into(), &sections)?;
    for roster in rosters.iter() {
        roster.write(&mut workbook)?;
    }
    let fname = format!("Lab {} Summary Attendance Sheet.xlsx", lab);
    let xlsx_output = base_dir.join(fname);
    workbook.data.save(xlsx_output)?;
    Ok(())
}

fn main() -> Result<(), MainError> {
    let dirs = BaseDirs::new().ok_or("unable to find home directory. Abort")?;
    let args = Args::parse();

    match args.command {
        Subcmd::Configure => {
            edit_config(dirs.config_dir())?;
        },
        Subcmd::Reset => {
            fs::remove_file(dirs.config_dir().join("rosters.txt"))?;
        },
        Subcmd::Generate { input, output, lab, nox } => {
            let file = fs::read_to_string(dirs.config_dir()
                .join("rosters.txt"))
                .unwrap_or("".into());
            let config: Config = toml::from_str(&file)?;
            if !input.exists() { Err("input file does not exist")? }
            let mut csv = csv::Reader::from_path(input)?;
            let records: Vec<_> = csv.deserialize()
                .filter_map(|x| x.ok())
                .collect();
            let name_lists = NameList::from_records(&records);
            let rosters: Vec<Roster> = name_lists.into_iter().map(Into::into).collect();

            let base_dir = output.unwrap_or(env::current_dir()?);
            if !base_dir.exists() { Err("output directory does not exist")? }
            if !base_dir.is_dir() { Err("output path needs to be a directory")? }

            let pdf_dir = if config.checkpoints.is_some() {
                let dir = base_dir.join("Blank Rosters");
                if !dir.exists() { std::fs::create_dir_all(&dir)? };
                dir
            } else {
                env::current_dir()?
            };

            let default_chkpt = vec!["1".into(), "2".into(), "3".into(), "4".into()];
            let lab_checkpoints = config.checkpoints
                .unwrap_or(HashMap::from([(lab.into(), default_chkpt.clone())]));
            let checkpoints = lab_checkpoints.get(&lab.into())
                .unwrap_or(&default_chkpt);
            write_pdf(lab.into(), &checkpoints, "All", rosters.iter(), &pdf_dir)?;
            let ta_assignment = config.ta_assignment.unwrap_or(HashMap::new());
            for (ta, sections) in ta_assignment.iter() {
                let mut rs = vec![];
                for &section in sections.iter() {
                    let roster = rosters.iter().find(|&r| r.section == section)
                        .ok_or(format!("'{}' is a section not found in the input data \
                                       file. check your configuration.", section))?;
                    rs.push(roster);
                }
                write_pdf(lab.into(), &checkpoints, ta, rs.into_iter(), &pdf_dir)?;
            }

            if !nox { write_xlsx(&base_dir, lab.into(), &rosters[..])? }
        }
    }
    Ok(())
}
