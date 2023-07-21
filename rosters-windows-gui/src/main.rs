#![cfg(windows)]
#![windows_subsystem = "windows"]
use rosters_lib::*;

use native_windows_derive::NwgUi;
use native_windows_gui::{Button, CheckBox, CheckBoxState, ComboBox, FileDialog,
                         Label, NativeUi, TextInput, Window};

use std::env;
use std::fs;

#[derive(Default, NwgUi)]
pub struct App {
    #[nwg_control(title: "Roster Generator", size: (520, 222), flags: "WINDOW|VISIBLE")]
    #[nwg_events(OnWindowClose: [App::exit])]
    window: Window,

    #[nwg_control(text: "Canvas data:", position: (0, 14), size: (91, 25),
                  h_align: HTextAlign::Right)]
    input_label: Label,

    #[nwg_control(position: (99, 11), size: (333, 20))]
    input: TextInput,

    #[nwg_resource(
        title: "Open File",
        action: FileDialogAction::Open,
        filters: "CSV(*.csv)|Any (*.*)"
    )]
    input_file_dialog: FileDialog,

    #[nwg_control(text: "Select", position: (440, 9), size: (72, 23))]
    #[nwg_events(OnButtonClick: [App::open_csv])]
    input_file_picker_button: Button,

    #[nwg_control(text: "Output folder:", position: (0, 41), size: (91, 25),
                  h_align: HTextAlign::Right)]
    output_label: Label,

    #[nwg_control(placeholder_text: Some("<optional>"),
                  position: (99, 39), size: (333, 20))]
    output: TextInput,

    #[nwg_resource(title: "Open Folder", action: FileDialogAction::OpenDirectory)]
    output_directory_dialog: FileDialog,

    #[nwg_control(text: "Select", position: (440, 36), size: (72, 23))]
    #[nwg_events(OnButtonClick: [App::open_dir])]
    output_file_picker_button: Button,

    #[nwg_control(text: "Config file:", position: (0, 68), size: (91, 25),
                  h_align: HTextAlign::Right)]
    config_label: Label,

    #[nwg_control(placeholder_text: Some("<optional>"),
                  position: (99, 66), size: (333, 20))]
    config: TextInput,

    #[nwg_resource(
        title: "Open File",
        action: FileDialogAction::Open,
        filters: "TOML(*.toml)|TXT(*.txt)|Any(*.*)"
    )]
    config_file_dialog: FileDialog,

    #[nwg_control(text: "Select", position: (440, 64), size: (72, 23))]
    #[nwg_events(OnButtonClick: [App::open_toml])]
    config_file_picker_button: Button,

    #[nwg_control(text: "Lab:", position: (0, 96), size: (91, 25),
                  h_align: HTextAlign::Right)]
    label: Label,

    #[nwg_control(collection: vec!["1", "2", "3", "4", "5", "6", "7", "8", "9"],
                  selected_index: Some(0),
                  position: (99, 92),
                  size: (57, 23))]
    lab: ComboBox<&'static str>,

    #[nwg_control(text: "Do not generate spreadsheet", position: (99, 120),
                  size: (175, 25), focus: true)]
    nox: CheckBox,

    #[nwg_control(text: "Do not split PDF", position: (99, 147),
                  size: (105, 25), focus: true)]
    no_split: CheckBox,

    #[nwg_control(text: "Create Sample Configuration", position: (164, 190),
                  size: (192, 23))]
    #[nwg_events(OnButtonClick: [App::save_config])]
    sample_config_button: Button,

    #[nwg_resource(
        title: "Save As",
        action: FileDialogAction::Save,
        filters: "TOML(*.toml)"
    )]
    sample_config_dialog: FileDialog,

    #[nwg_control(text: "Run", position: (362, 190), size: (72, 23))]
    #[nwg_events(OnButtonClick: [App::generate])]
    run_button: Button,

    #[nwg_control(text: "Exit", position: (440, 190), size: (72, 23))]
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
    let mut font = native_windows_gui::Font::default();
    let _ = native_windows_gui::Font::builder().size(16).family("Segoe UI").build(&mut font);
    let _ = native_windows_gui::Font::set_global_default(Some(font));
    use app_ui::AppUi;
    let _app = App::build_ui(Default::default()).unwrap();
    native_windows_gui::dispatch_thread_events();
}
