#![cfg(windows)]
#![windows_subsystem = "windows"]
use rosters_lib::*;

use directories::BaseDirs;
use native_windows_derive::NwgUi;
use native_windows_gui::{Button, CheckBox, CheckBoxState, ComboBox, FileDialog,
                         Label, NativeUi, TextInput, Window, NumberSelect,
                         NumberSelectData};
use serde::{Deserialize, Serialize};

use std::env;
use std::fs;
use std::path::PathBuf;

const STANDARD_DPI: i32 = 96;

struct DpiAwareSize(i32);

impl Into<i32> for DpiAwareSize {
    fn into(self) -> i32 {
        let dpi = unsafe { native_windows_gui::dpi() };
        self.0 * dpi / STANDARD_DPI
    }
}

struct DpiAwareSizeTuple(i32, i32);

impl Into<(i32, i32)> for DpiAwareSizeTuple {
    fn into(self) -> (i32, i32) {
        (DpiAwareSize(self.0).into(), DpiAwareSize(self.1).into())
    }
}

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
struct SerdeNumberSelectData { value: i64, step: i64, max: i64, min: i64 }

impl TryFrom<NumberSelectData> for SerdeNumberSelectData {
    type Error = ();
    fn try_from(d: NumberSelectData) -> Result<Self, Self::Error> {
        match d {
            NumberSelectData::Int { value, step, max, min } =>
                Ok(SerdeNumberSelectData { value, step, max, min }),
            _ => Err(())
        }
    }
}

impl Into<NumberSelectData> for SerdeNumberSelectData {
    fn into(self) -> NumberSelectData {
        NumberSelectData::Int {
            value: self.value,
            step: self.step,
            max: self.max,
            min: self.min
        }
    }
}

#[derive(Deserialize, Debug, Serialize)]
struct PreviousState {
    lab: Option<usize>,
    // wrap in option for backward compatibility
    no_sign: Option<SerdeCheckBoxState>,
    nox: SerdeCheckBoxState,
    no_split: SerdeCheckBoxState,
    group_size: Option<SerdeNumberSelectData>,
    ngroups: Option<SerdeNumberSelectData>,
    defaults: Option<SerdeCheckBoxState>,
    config_path: String,
    input_path: String,
    output_path: String,
}

#[derive(Default, NwgUi)]
pub struct App {
    #[nwg_control(title: &format!("Roster Generator v{}", env!("CARGO_PKG_VERSION")),
                  size: DpiAwareSizeTuple(540, 330).into(),
                  flags: "WINDOW|VISIBLE")]
    #[nwg_events(OnWindowClose: [App::exit])]
    window: Window,

    #[nwg_control(text: "Canvas data:",
                  position: DpiAwareSizeTuple(20, 14).into(),
                  size: DpiAwareSizeTuple(91, 25).into(),
                  h_align: HTextAlign::Right)]
    input_label: Label,

    #[nwg_control(position: DpiAwareSizeTuple(119, 11).into(), size: DpiAwareSizeTuple(333, 20).into())]
    input: TextInput,

    #[nwg_resource(
        title: "Open File",
        action: FileDialogAction::Open,
        filters: "CSV(*.csv)|Any (*.*)"
    )]
    input_file_dialog: FileDialog,

    #[nwg_control(text: "Select",
                  position: DpiAwareSizeTuple(460, 9).into(),
                  size: DpiAwareSizeTuple(72, 23).into())]
    #[nwg_events(OnButtonClick: [App::open_csv])]
    input_file_picker_button: Button,

    #[nwg_control(text: "Output folder:",
                  position: DpiAwareSizeTuple(20, 41).into(),
                  size: DpiAwareSizeTuple(91, 25).into(),
                  h_align: HTextAlign::Right)]
    output_label: Label,

    #[nwg_control(placeholder_text: Some("<optional>"),
                  position: DpiAwareSizeTuple(119, 39).into(),
                  size: DpiAwareSizeTuple(333, 20).into())]
    output: TextInput,

    #[nwg_resource(title: "Open Folder", action: FileDialogAction::OpenDirectory)]
    output_directory_dialog: FileDialog,

    #[nwg_control(text: "Select",
                  position: DpiAwareSizeTuple(460, 36).into(),
                  size: DpiAwareSizeTuple(72, 23).into())]
    #[nwg_events(OnButtonClick: [App::open_dir])]
    output_file_picker_button: Button,

    #[nwg_control(text: "Config file:",
                  position: DpiAwareSizeTuple(20, 68).into(),
                  size: DpiAwareSizeTuple(91, 25).into(),
                  h_align: HTextAlign::Right)]
    config_label: Label,

    #[nwg_control(placeholder_text: Some("<optional>"),
                  position: DpiAwareSizeTuple(119, 66).into(),
                  size: DpiAwareSizeTuple(333, 20).into())]
    config: TextInput,

    #[nwg_resource(
        title: "Open File",
        action: FileDialogAction::Open,
        filters: "TOML(*.toml)|TXT(*.txt)|Any(*.*)"
    )]
    config_file_dialog: FileDialog,

    #[nwg_control(text: "Select",
                  position: DpiAwareSizeTuple(460, 64).into(),
                  size: DpiAwareSizeTuple(72, 23).into())]
    #[nwg_events(OnButtonClick: [App::open_toml])]
    config_file_picker_button: Button,

    #[nwg_control(text: "Lab:",
                  position: DpiAwareSizeTuple(20, 96).into(),
                  size: DpiAwareSizeTuple(91, 25).into(),
                  h_align: HTextAlign::Right)]
    lab_label: Label,

    #[nwg_control(collection: vec!["Math Bootcamp", "1", "2", "3", "4", "5"],
                  selected_index: Some(0),
                  position: DpiAwareSizeTuple(119, 92).into(),
                  size: DpiAwareSizeTuple(115, 23).into())]
    lab: ComboBox<&'static str>,

    #[nwg_control(text: "Group size:",
                  position: DpiAwareSizeTuple(20, 120).into(),
                  size: DpiAwareSizeTuple(91, 25).into(),
                  h_align: HTextAlign::Right)]
    group_size_label: Label,

    #[nwg_control(position: DpiAwareSizeTuple(119, 120).into(),
                  size: DpiAwareSizeTuple(40, 20).into())]
    group_size: NumberSelect,

    #[nwg_control(text: "Number of groups:",
                  position: DpiAwareSizeTuple(0, 148).into(),
                  size: DpiAwareSizeTuple(111, 25).into(),
                  h_align: HTextAlign::Right)]
    ngroups_label: Label,

    #[nwg_control(position: DpiAwareSizeTuple(119, 148).into(),
                  size: DpiAwareSizeTuple(40, 20).into())]
    ngroups: NumberSelect,

    #[nwg_control(text: "Use default configuration shipped with this program",
                  position: DpiAwareSizeTuple(119, 176).into(),
                  size: DpiAwareSizeTuple(305, 25).into(),
                  focus: true)]
    #[nwg_events(OnButtonClick: [App::clear_config_path])]
    defaults: CheckBox,

    #[nwg_control(text: "Do not include \"Signed\" column",
                  position: DpiAwareSizeTuple(119, 203).into(),
                  size: DpiAwareSizeTuple(190, 25).into(),
                  focus: true)]
    no_sign: CheckBox,

    #[nwg_control(text: "Do not generate spreadsheet",
                  position: DpiAwareSizeTuple(119, 230).into(),
                  size: DpiAwareSizeTuple(175, 25).into(),
                  focus: true)]
    nox: CheckBox,

    #[nwg_control(text: "Do not split PDF",
                  position: DpiAwareSizeTuple(119, 257).into(),
                  size: DpiAwareSizeTuple(105, 25).into(),
                  focus: true)]
    no_split: CheckBox,

    #[nwg_control(text: "Create Sample Configuration",
                  position: DpiAwareSizeTuple(184, 300).into(),
                  size: DpiAwareSizeTuple(192, 23).into())]
    #[nwg_events(OnButtonClick: [App::save_config])]
    sample_config_button: Button,

    #[nwg_resource(
        title: "Save As",
        action: FileDialogAction::Save,
        filters: "TOML(*.toml)"
    )]
    sample_config_dialog: FileDialog,

    #[nwg_control(text: "Run",
                  position: DpiAwareSizeTuple(382, 300).into(),
                  size: DpiAwareSizeTuple(72, 23).into())]
    #[nwg_events(OnButtonClick: [App::generate])]
    run_button: Button,

    #[nwg_control(text: "Exit",
                  position: DpiAwareSizeTuple(460, 300).into(),
                  size: DpiAwareSizeTuple(72, 23).into())]
    #[nwg_events(OnButtonClick: [App::exit])]
    exit: Button
}

impl App {
    fn previous_state_path() -> Result<PathBuf, &'static str> {
        let dirs = BaseDirs::new().ok_or("");
        dirs.map(|d| d.data_dir().join("rosters_last_state.json"))
    }

    fn restore_state(&self) {
        let default_group_size = NumberSelectData::Int {
            value: 5, step: 1, min: 1, max: 10
        };
        let default_ngroups = NumberSelectData::Int {
            value: 6, step: 1, min: 1, max: 10
        };
        if let Ok(data_path) = App::previous_state_path() {
            if let Ok(file) = fs::read_to_string(data_path) {
                let data: Result<PreviousState, _> = serde_json::from_str(&file);
                if let Ok(state) = data {
                    self.lab.set_selection(state.lab);
                    self.nox.set_check_state(state.nox.into());
                    if let Some(no_sign) = state.no_sign {
                        self.no_sign.set_check_state(no_sign.into());
                    }
                    if let Some(defaults) = state.defaults {
                        self.defaults.set_check_state(defaults.into());
                    }
                    if let Some(group_size) = state.group_size {
                        self.group_size.set_data(group_size.into());
                    } else {
                        self.group_size.set_data(default_group_size);
                    }
                    if let Some(ngroups) = state.ngroups {
                        self.ngroups.set_data(ngroups.into());
                    } else {
                        self.ngroups.set_data(default_ngroups);
                    }
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
                } else {
                    self.group_size.set_data(default_group_size);
                    self.ngroups.set_data(default_ngroups);
                }
            } else {
                self.group_size.set_data(default_group_size);
                self.ngroups.set_data(default_ngroups);
            }
        }
    }

    fn save_state(&self) {
        let lab = self.lab.selection();
        let nox = self.nox.check_state().into();
        let no_sign = Some(self.no_sign.check_state().into());
        let no_split = self.no_split.check_state().into();
        let defaults = Some(self.defaults.check_state().into());
        let ngroups = SerdeNumberSelectData::try_from(self.ngroups.data()).ok();
        let group_size = SerdeNumberSelectData::try_from(self.group_size.data()).ok();
        let input_path = self.input.text().to_owned();
        let output_path = self.output.text().to_owned();
        let config_path = self.config.text().to_owned();
        let state = PreviousState { lab, nox, no_sign, no_split, defaults,
                                    input_path, output_path, config_path,
                                    ngroups, group_size };
        if let Ok(data_path) = App::previous_state_path() {
            if let Ok(str) = serde_json::to_string(&state) {
                let _ = fs::write(data_path, str);
            }
        }
    }

    fn clear_config_path(&self) { self.config.set_text("") }

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

    fn to_int(d: NumberSelectData) -> Option<usize> {
        match d {
            NumberSelectData::Int { value, .. } => Some(value as usize),
            _ => None
        }
    }

    fn generate(&self) {
        let maybe_input = App::maybe_text(&self.input);
        let output = App::maybe_text(&self.output);
        let config = App::maybe_text(&self.config);
        let lab = Lab::from(self.lab.selection().unwrap());
        let no_sign = App::to_bool(self.no_sign.check_state());
        let defaults = App::to_bool(self.defaults.check_state());
        let nox = App::to_bool(self.nox.check_state());
        let no_split = App::to_bool(self.no_split.check_state());
        let ngroups = App::to_int(self.ngroups.data());
        let group_size = App::to_int(self.group_size.data());
        if let Some(input) = maybe_input {
            let generator = crate::Generator { input, output, lab, no_sign, nox,
                                               config, defaults, no_split, group_size,
                                               ngroups };
            if let Err(e) = generator.run() {
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

    fn open_toml(&self) {
        self.open_file(&self.config_file_dialog, &self.config);
        self.defaults.set_check_state(CheckBoxState::Unchecked);
    }

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
    let font_size: i32 = DpiAwareSize(16).into();
    let _ = native_windows_gui::Font::builder().size(font_size as u32).family("Segoe UI").build(&mut font);
    let _ = native_windows_gui::Font::set_global_default(Some(font));
    let _app = App::build_ui(Default::default()).unwrap();
    _app.restore_state();
    native_windows_gui::dispatch_thread_events();
}
