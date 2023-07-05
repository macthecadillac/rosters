use arrayvec::ArrayVec;
use clap::{Parser, Subcommand};
use data::{Checkpoint, Config, Lab, Roster};
use directories::BaseDirs;
use main_error::MainError;
use pdf::{Font, FontRef};
use printpdf::PdfDocument;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

use std::env;
use std::fs;
use std::io::BufWriter;
use std::path::PathBuf;
use std::str::FromStr;

mod data;
mod error;
mod pdf;
mod xlsx;

const EXAMPLE_CONFIG: &'static str = std::include_str!("../example_config.toml");

#[derive(Debug, Subcommand)]
enum Subcmd {
    /// Create starter configuration
    Config {
        /// Output path
        #[arg(short, long)]
        output: std::path::PathBuf
    },
    /// Generate rosters
    Generate {
        /// Path to canvas exported CSV file
        #[arg(short, long)]
        input: std::path::PathBuf,
        /// Output directory
        #[arg(short, long)]
        output: Option<std::path::PathBuf>,
        /// Lab number
        #[arg(long)]
        lab: usize,
        /// Skip Excel file generation
        #[arg(long)]
        nox: bool,
        /// Run with supplied configuration
        #[arg(short='c', long="with-config")]
        config: Option<std::path::PathBuf>,
        /// Do not create per-section or per-TA PDFs
        #[arg(long="no-split")]
        no_split: bool
    },
}

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Args {
    #[command(subcommand)]
    command: Subcmd,
}

enum DataStream<'a, T> where T: Iterator<Item=&'a Roster<'a>> {
    Many { rosters: T, tag: &'a str },
    One { roster: &'a Roster<'a> }
}

fn write_pdf<'a, T>(lab: Lab, checkpoints: &[Checkpoint], data: DataStream<'a, T>,
                    pdf_dir: &PathBuf)
    -> Result<(), error::Error>
    where T: Iterator<Item=&'a Roster<'a>> {
    let mut pdf = pdf::Document::default();
    let fname = match data {
        DataStream::Many { rosters, tag } => {
            for roster in rosters {
                pdf.add_page(roster, lab.into(), &checkpoints)?;
            }
            format!("Lab {} Blank Rosters ({} Sections).pdf", lab, tag)
        },
        DataStream::One { roster } => {
            pdf.add_page(roster, lab.into(), &checkpoints)?;
            format!("Lab {} Blank Rosters (Section {}).pdf", lab, roster.section)
        }
    };
    let regular_font = pdf.font_subset(Font::Regular)?;
    let bold_font = pdf.font_subset(Font::Bold)?;
    let mut doc = PdfDocument::empty(&fname);
    let regular = doc.add_external_font(&regular_font[..])?;
    let bold = doc.add_external_font(&bold_font[..])?;
    let font_ref = FontRef { regular: &regular, bold: &bold };
    pdf.render(&mut doc, font_ref)?;
    let pdf_output = pdf_dir.join(fname);
    let file = fs::File::create(pdf_output)?;
    doc.save(&mut BufWriter::new(file))?;
    Ok(())
}

fn write_xlsx<'a>(base_dir: &PathBuf, lab: Lab,
                  rosters: &[Roster]) -> Result<(), error::Error> {
    let mut workbook = xlsx::Workbook::default();
    let sections: Vec<_> = rosters.iter().map(|x| x.section).collect();
    workbook.initialize(lab.into(), &sections)?;
    for roster in rosters.iter() {
        workbook.add_sheet(roster)?;
    }
    let fname = format!("Lab {} Summary Attendance Sheet.xlsx", lab);
    let mut path = base_dir.join(fname);
    if path.exists() {
        for i in 1.. {
            let fname = format!("Lab {} Summary Attendance Sheet({}).xlsx", lab, i);
            path = base_dir.join(fname);
            if !path.exists() { break }
        }
    }
    workbook.data.save(path)?;
    Ok(())
}

fn main() -> Result<(), MainError> {
    let dirs = BaseDirs::new().ok_or("unable to find home directory. Abort")?;
    let args = Args::parse();

    match args.command {
        Subcmd::Config { output } => fs::write(&output, EXAMPLE_CONFIG)?,
        Subcmd::Generate { input, output, lab, nox, config, no_split } => {
            let config_path = config.unwrap_or_else(|| dirs.config_dir().join("rosters.toml"));
            let file = fs::read_to_string(config_path).unwrap_or_else(|_| "".into());
            let config: Config = toml::from_str(&file)?;
            if !input.exists() { Err("input file does not exist")? }
            let mut csv = csv::Reader::from_path(input)?;
            let records: Vec<_> = csv.deserialize()
                .filter_map(|x| x.ok())
                .collect();
            let rosters = Roster::from_records(&records)?;

            let base_dir = output.ok_or("").or_else(|_| env::current_dir())?;
            if !base_dir.exists() { Err("output directory does not exist")? }
            if !base_dir.is_dir() { Err("output path needs to be a directory")? }

            let dir = base_dir.join("Blank Rosters");
            let pdf_dir = if !no_split {
                if !dir.exists() { std::fs::create_dir_all(&dir)? };
                &dir
            } else {
                &base_dir
            };

            let mut default_chkpt = ArrayVec::new();
            for s in ["1", "2", "3", "4"].into_iter() {
                default_chkpt.try_push(FromStr::from_str(s)?)?;
            }
            let checkpoints = config.checkpoints.as_ref()
                .and_then(|m| m.get(&lab.into()))
                .unwrap_or(&default_chkpt);
            if !no_split {
                if let Some(ta_assignment) = config.ta_assignment {
                    ta_assignment.par_iter().try_for_each(|(ta, sections)| {
                        let rosters = sections.iter()
                            .map(|&section| rosters.iter().find(|&r| r.section == section)
                                .ok_or(error::Error::NonexistantSection(section)))
                            .collect::<Result<Vec<_>, _>>()?
                            .into_iter();
                        let data_stream = DataStream::Many { rosters, tag: ta.as_ref() };
                        write_pdf(lab.into(), &checkpoints, data_stream, &pdf_dir)
                    })?;
                } else {
                    rosters.par_iter().try_for_each(|roster| {
                        let data_stream: DataStream<std::iter::Empty<_>> = DataStream::One { roster };
                        write_pdf(lab.into(), &checkpoints, data_stream, &pdf_dir)
                    })?;
                }
            }
            let data_stream = DataStream::Many { rosters: rosters.iter(), tag: "All" };
            write_pdf(lab.into(), &checkpoints, data_stream, &pdf_dir)?;

            if !nox { write_xlsx(&base_dir, lab.into(), &rosters[..])? }
        }
    }
    Ok(())
}
