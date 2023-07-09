use arrayvec::ArrayVec;
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

#[cfg(unix)]
mod unix_cli {
    use clap::{Parser, Subcommand};
    use main_error::MainError;
    use std::fs;
    use std::path::PathBuf;

    #[derive(Debug, Subcommand)]
    pub enum Subcmd {
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
            input: PathBuf,
            /// Output directory
            #[arg(short, long)]
            output: Option<PathBuf>,
            /// Lab number
            #[arg(long)]
            lab: usize,
            /// Skip Excel file generation
            #[arg(long)]
            nox: bool,
            /// Run with supplied configuration
            #[arg(short='c', long="with-config")]
            config: Option<PathBuf>,
            /// Do not create per-section or per-TA PDFs
            #[arg(long="no-split")]
            no_split: bool
        },
    }

    #[derive(Parser, Debug)]
    #[command(author, version, about)]
    pub struct Args {
        #[command(subcommand)]
        pub command: Subcmd,
    }

    pub fn main() -> Result<(), MainError> {
        let args = Args::parse();
        match args.command {
            Subcmd::Config { output } => fs::write(&output, crate::EXAMPLE_CONFIG)?,
            Subcmd::Generate { input, output, lab, nox, config, no_split } => 
                crate::generate(input, output, lab, nox, config, no_split)?
        }
        Ok(())
    }
}

#[cfg(windows)]
mod win_gui {
    use native_windows_derive::NwgUi;
    use native_windows_gui::{Button, CheckBox, CheckBoxState, ComboBox, FileDialog,
                             GridLayout, Label, NativeUi, TextInput, Window};
    use std::env;
    use std::fs;

    #[derive(Default, NwgUi)]
    pub struct App {
        #[nwg_control(title: "Roster Generator", size: (500, 150))]
        #[nwg_events(OnWindowClose: [App::exit])]
        window: Window,
        #[nwg_layout(parent: window, spacing: 1)]
        grid: GridLayout,
        #[nwg_control(text: "Canvas data:", h_align: HTextAlign::Right)]
        #[nwg_layout_item(layout: grid, row: 0, col: 0, col_span: 2)]
        input_label: Label,
        #[nwg_control(readonly: true)]
        #[nwg_layout_item(layout: grid, row: 0, col: 2, col_span: 5)]
        input: TextInput,
        #[nwg_resource(
            title: "Open File",
            action: FileDialogAction::Open,
            filters: "CSV(*.csv)|Any (*.*)"
        )]
        input_file_dialog: FileDialog,
        #[nwg_control(text: "Select")]
        #[nwg_layout_item(layout: grid, row: 0,  col: 7)]
        #[nwg_events(OnButtonClick: [App::open_csv])]
        input_file_picker_button: Button,
        #[nwg_control(text: "Output folder:", h_align: HTextAlign::Right)]
        #[nwg_layout_item(layout: grid, row: 1, col: 0, col_span: 2)]
        output_label: Label,
        #[nwg_control(text: "<optional>", readonly: true)]
        #[nwg_layout_item(layout: grid, row: 1, col: 2, col_span: 5)]
        output: TextInput,
        #[nwg_resource(title: "Open Folder", action: FileDialogAction::OpenDirectory)]
        output_directory_dialog: FileDialog,
        #[nwg_control(text: "Select")]
        #[nwg_layout_item(layout: grid, row: 1,  col: 7)]
        #[nwg_events(OnButtonClick: [App::open_dir])]
        output_file_picker_button: Button,
        #[nwg_control(text: "Config file:", h_align: HTextAlign::Right)]
        #[nwg_layout_item(layout: grid, row: 2, col: 0, col_span: 2)]
        config_label: Label,
        #[nwg_control(text: "<optional>", readonly: true)]
        #[nwg_layout_item(layout: grid, row: 2, col: 2, col_span: 5)]
        config: TextInput,
        #[nwg_resource(
            title: "Open File",
            action: FileDialogAction::Open,
            filters: "TOML(*.toml)|TXT(*.txt)|Any(*.*)"
        )]
        config_file_dialog: FileDialog,
        #[nwg_control(text: "Select")]
        #[nwg_layout_item(layout: grid, row: 2,  col: 7)]
        #[nwg_events(OnButtonClick: [App::open_toml])]
        config_file_picker_button: Button,
        #[nwg_control(text: "Lab:", h_align: HTextAlign::Right)]
        #[nwg_layout_item(layout: grid, row: 3, col: 0, col_span: 2)]
        label: Label,
        #[nwg_control(collection: vec!["1", "2", "3", "4", "5", "6", "7", "8", "9"],
                      selected_index: Some(0))]
        #[nwg_layout_item(layout: grid, row: 3, col: 2)]
        lab: ComboBox<&'static str>,
        #[nwg_control(text: "Do not generate spreadsheet", focus: true)]
        #[nwg_layout_item(layout: grid, row: 3, col: 3, col_span: 3)]
        nox: CheckBox,
        #[nwg_control(text: "Do not split PDF", focus: true)]
        #[nwg_layout_item(layout: grid, row: 3, col: 6, col_span: 2)]
        no_split: CheckBox,
        #[nwg_control(text: "Create Sample Configuration")]
        #[nwg_layout_item(layout: grid, row: 4, col: 2, col_span: 3)]
        #[nwg_events(OnButtonClick: [App::save_config])]
        sample_config_button: Button,
        #[nwg_resource(title: "Save As", action: FileDialogAction::Save)]
        sample_config_dialog: FileDialog,
        #[nwg_control(text: "Run")]
        #[nwg_layout_item(layout: grid, row: 4, col: 5, col_span: 2)]
        #[nwg_events(OnButtonClick: [App::generate])]
        run_button: Button,
        #[nwg_control(text: "Exit")]
        #[nwg_layout_item(layout: grid, row: 4,  col: 7)]
        #[nwg_events(OnButtonClick: [App::exit])]
        exit: Button
    }

    impl App {
        fn generate(&self) {
            let to_bool = |s| match s {
                CheckBoxState::Checked => true,
                CheckBoxState::Unchecked | CheckBoxState::Indeterminate => false
            };
            let maybe_text = |t: &TextInput| {
                let text = t.text();
                (!text.is_empty() && &text != "<optional>").then(|| text.into())
            };
            let maybe_input = maybe_text(&self.input);
            let output = maybe_text(&self.output);
            let config = maybe_text(&self.config);
            let lab = self.lab.selection().unwrap() + 1;
            let nox = to_bool(self.nox.check_state());
            let no_split = to_bool(self.no_split.check_state());
            if let Some(input) = maybe_input {
                if let Err(e) = crate::generate(input, output, lab, nox, config, no_split) {
                    let msg = format!("{:?}", e);
                    native_windows_gui::modal_info_message(&self.window, "Error", &format!("Error: {}", msg));
                } else {
                    native_windows_gui::modal_info_message(&self.window, "Done", "Done.");
                };
            } else {
                native_windows_gui::modal_info_message(&self.window, "Error", "Please select input file.");
            }
        }

        fn save_config(&self) {
            if let Ok(dir) = env::current_dir() {
                if let Some(dir) = dir.to_str() {
                    self.sample_config_dialog.set_default_folder(dir)
                                             .expect("Failed to set default folder.");
                }
            }
            if self.sample_config_dialog.run(Some(&self.window)) {
                if let Ok(directory) = self.sample_config_dialog.get_selected_item() {
                    let output = directory.into_string().unwrap();
                    if let Err(e) = fs::write(&output, crate::EXAMPLE_CONFIG) {
                        let msg = format!("{}", e);
                        native_windows_gui::modal_info_message(&self.window, "Error", &msg);
                    };
                }
            }
        }

        fn open_csv(&self) { self.open_file(&self.input_file_dialog, &self.input) }

        fn open_dir(&self) { self.open_file(&self.output_directory_dialog, &self.output) }

        fn open_toml(&self) { self.open_file(&self.config_file_dialog, &self.config) }

        fn open_file(&self, dialog: &FileDialog, file_name: &TextInput) {
            if let Ok(dir) = env::current_dir() {
                if let Some(dir) = dir.to_str() {
                    dialog.set_default_folder(dir)
                          .expect("Failed to set default folder.");
                }
            }
            if dialog.run(Some(&self.window)) {
                file_name.set_text("");
                if let Ok(p) = dialog.get_selected_item() {
                    let path = p.into_string().unwrap();
                    file_name.set_text(&path);
                }
            }
        }

        fn exit(&self) { native_windows_gui::stop_thread_dispatch() }
    }

    #[allow(unused_imports)]
    pub fn main() {
        native_windows_gui::init().expect("Failed to init Windows GUI");
        native_windows_gui::Font::set_global_family("Segoe UI")
                                 .expect("Failed to set default font");
        use app_ui::AppUi;
        let _app = App::build_ui(Default::default()).unwrap();
        native_windows_gui::dispatch_thread_events();
    }
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

fn generate(input: PathBuf,
            output: Option<PathBuf>,
            lab: usize,
            nox: bool,
            config: Option<PathBuf>,
            no_split: bool)
    -> Result<(), MainError> {
    let dirs = BaseDirs::new().ok_or("unable to find home directory. Abort")?;
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
    Ok(())
}

#[cfg(unix)]
fn main() -> Result<(), MainError> {
    unix_cli::main()
}

#[cfg(windows)]
fn main() {
    win_gui::main()
}
