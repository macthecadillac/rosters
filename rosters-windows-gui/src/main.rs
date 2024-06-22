#![cfg(windows)]
#![windows_subsystem = "windows"]
use rosters_lib::*;

use directories::BaseDirs;
use native_windows_derive::NwgUi;
use native_windows_gui::{Button, CheckBox, CheckBoxState, ComboBox, FileDialog,
                         Label, NativeUi, TextInput, Window};
use serde::{Deserialize, Serialize};

use std::env;
use std::fs;
use std::path::PathBuf;

#[derive(Deserialize, Debug, Serialize)]
enum SerdeCheckBoxState { Checked, Unchecked, Indeterminate }

impl From<CheckBoxState> for SerdeCheckBoxState {
    fn from(s: CheckBoxState) -> Self {
        match s {
            CheckBoxState::Checked => SerdeCheckBoxState::Checked,
            CheckBoxState::Unchecked => SerdeCheckBoxState::Unchecked,
            CheckBoxState::Indeterminate => SerdeCheckBoxState::Indeterminate
        }
    }
}

impl Into<CheckBoxState> for SerdeCheckBoxState {
    fn into(self) -> CheckBoxState {
        match self {
            SerdeCheckBoxState::Checked => CheckBoxState::Checked,
            SerdeCheckBoxState::Unchecked => CheckBoxState::Unchecked,
            SerdeCheckBoxState::Indeterminate => CheckBoxState::Indeterminate
        }
    }
}

#[derive(Deserialize, Debug, Serialize)]
struct PreviousState {
    lab: Option<usize>,
    nox: SerdeCheckBoxState,
    no_split: SerdeCheckBoxState,
    config_path: String,
    input_path: String,
    output_path: String,
}

#[derive(Default, NwgUi)]
pub struct App {
    #[nwg_control(title: &format!("Roster Generator v{}", env!("CARGO_PKG_VERSION")),
                  size: (520, 222),
                  flags: "WINDOW|VISIBLE")]
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
    fn previous_state_path() -> Result<PathBuf, &'static str> {
        let dirs = BaseDirs::new().ok_or("");
        dirs.map(|d| d.data_dir().join("rosters_last_state.json"))
    }

    fn restore_state(&self) {
        if let Ok(data_path) = App::previous_state_path() {
            if let Ok(file) = fs::read_to_string(data_path) {
                let data: Result<PreviousState, _> = serde_json::from_str(&file);
                if let Ok(state) = data {
                    self.lab.set_selection(state.lab);
                    self.nox.set_check_state(state.nox.into());
                    self.no_split.set_check_state(state.no_split.into());
                    if PathBuf::from(&state.input_path).exists() {
                        self.input.set_text(&state.input_path);
                    }
                    if PathBuf::from(&state.output_path).exists() {
                        self.output.set_text(&state.output_path);
                    }
                    if PathBuf::from(&state.config_path).exists() {
                        self.config.set_text(&state.config_path);
                    }
                }
            }
        }
    }

    fn save_state(&self) {
        let lab = self.lab.selection();
        let nox = self.nox.check_state().into();
        let no_split = self.no_split.check_state().into();
        let input_path = self.input.text().to_owned();
        let output_path = self.output.text().to_owned();
        let config_path = self.config.text().to_owned();
        let state = PreviousState { lab, nox, no_split, input_path, output_path, config_path };
        if let Ok(data_path) = App::previous_state_path() {
            if let Ok(str) = serde_json::to_string(&state) {
                let _ = fs::write(data_path, str);
            }
        }
    }

    fn maybe_text(t: &TextInput) -> Option<PathBuf> {
        let text = t.text();
        (!text.is_empty() && &text != "<optional>").then(|| text.into())
    }

    fn to_bool(s: CheckBoxState) -> bool {
        match s {
            CheckBoxState::Checked => true,
            CheckBoxState::Unchecked | CheckBoxState::Indeterminate => false
        }
    }

    fn generate(&self) {
        let maybe_input = App::maybe_text(&self.input);
        let output = App::maybe_text(&self.output);
        let config = App::maybe_text(&self.config);
        let lab = self.lab.selection().unwrap() + 1;
        let nox = App::to_bool(self.nox.check_state());
        let no_split = App::to_bool(self.no_split.check_state());
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

    fn exit(&self) {
        self.save_state();
        native_windows_gui::stop_thread_dispatch()
    }
}

#[allow(unused_imports)]
pub fn main() {
    native_windows_gui::init().expect("Failed to init Windows GUI");
    let mut font = native_windows_gui::Font::default();
    let _ = native_windows_gui::Font::builder().size(16).family("Segoe UI").build(&mut font);
    let _ = native_windows_gui::Font::set_global_default(Some(font));
    use app_ui::AppUi;
    let _app = App::build_ui(Default::default()).unwrap();
    _app.restore_state();
    native_windows_gui::dispatch_thread_events();
}
