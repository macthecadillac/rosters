open Containers
open Fun

open Common

let read_config config =
  let open Result in
  let from_array f = function
    | Otoml.TomlArray l -> Option.sequence_l (List.map f l)
    | _ -> None in
  let maybe_assoc f s = Option.map (Pair.make s) % from_array f in
  let* ta_assignment =
    let f = function Otoml.TomlInteger i -> Some (Section.of_int i) | _ -> None in
    let* table = Otoml.find_result config Otoml.get_table ["ta-assignment"] in
    List.map (uncurry (maybe_assoc f)) table
      |> Option.sequence_l
      |> Option.to_result "cannot read the \"ta-assignment\" section"
      >|= StringMap.of_list in
  let+ checkpoints =
    let f = function Otoml.TomlString s -> Some s | _ -> None in
    let to_int s = Str.replace_first (Str.regexp {|lab\([0-9]+\)|}) {|\1|} s
      |> Int.of_string in
    match Otoml.find_result config (Otoml.get_table) ["checkpoints"] with
    | Error _ -> pure None
    | Ok table ->
        List.map (uncurry (maybe_assoc f)) table
        |> Option.sequence_l
        |> Option.to_result "cannot read the \"checkpoints\" section"
        >>= List.map (fun (s, l) -> Option.map (flip Pair.make l) @@ to_int s)
            %> Option.sequence_l
            %> Option.to_result "cannot read the lab numbers"
        >|= IntMap.of_list %> Option.some in
  ta_assignment, checkpoints

let config_path = Option.get_or ~default:(Fpath.v "lab-tools.toml") begin
  let open Option in
  let module F = Bos.OS.File in
  let var str = 
    let* s = Bos.OS.Env.var str in
    let+ dir = Fpath.of_string s |> of_result in
    Fpath.(dir / "lab-tools.toml") in
  match Sys.os_type with
    | "Win32" -> 
        let path1 = var "LOCALAPPDADA" >>= (F.must_exist %> of_result) in
        let path2 = var "APPDATA" >>= (F.must_exist %> of_result) in
        path1 <+> path2
    | "Unix" ->
        let path1 = var "XDG_CONFIG_HOME" >>= (F.must_exist %> of_result) in
        let path2 =
          let* user_dir = Result.to_opt @@ Bos.OS.Dir.user () in
          of_result (F.must_exist Fpath.(user_dir / ".config" / "lab-tools.toml")) in
        path1 <+> path2
    | _ -> None
  end

let load_config () =
  let open Result in
  let* toml = Bos.OS.File.read config_path
    |> map_err (fun (`Msg s) -> `Msg "no configuration found") in
  map_err (fun s -> `Msg s) (Otoml.Parser.from_string_result toml >>= read_config)

let open_config_in_editor () =
  let open Result in
  get_lazy (fun (`Msg s) -> print_endline s) begin
    let* cmd = match Sys.os_type with
      | "Unix" -> Ok (Bos.Cmd.(v "open" % Fpath.to_string config_path))
      | "Win32" -> Ok (Bos.Cmd.v @@ Fpath.to_string config_path)
      | _ -> Error (`Msg "unsupported platform for this option") in
    Bos.OS.Cmd.run cmd
  end

let generate_rosters lab data_path =
  let open Result in
  let write_xlsx checkpoints rosters =
    let xlsx = Roster.to_xlsx lab checkpoints rosters in
    Fpath.of_string "test.xlsx" >>= Fun.flip Xlsx.write xlsx in
  let write_pdf checkpoints rosters =
    let pdf = Pdf.to_bytes @@ List.map (Pdf.of_roster lab checkpoints) rosters in
    Fpath.of_string "test.pdf" >>= Fun.flip Bos.OS.File.write pdf in
  get_lazy (fun (`Msg s) -> print_endline s) begin
    let* _, checkpoints_opt = load_config () in
    let checkpoints =
      let open Option in
      let default = ["1"; "2"; "3"; "4"] in
      get_or ~default (checkpoints_opt >>= IntMap.get lab) in
    let* fpath = Fpath.of_string data_path in
    let* s = Bos.OS.File.read fpath in
    let rosters = Record.of_csv_string s |> Roster.of_data |> Seq.to_list in
    let* () = write_xlsx checkpoints rosters in
    write_pdf checkpoints rosters
  end

let merge_data published_path unpublished_path =
  let open Result in
  get_lazy (fun (`Msg s) -> print_endline s) begin
    let* section_map, _ = load_config () in
    let* latest =
      let* fpath = Fpath.of_string unpublished_path in
      let* xlsx = Xlsx.read fpath in
      Record.of_xlsx_sheets xlsx |> map_err (fun s -> `Msg s) in
    let* published =
      let* fpath = Fpath.of_string published_path in
      let+ s = Bos.OS.File.read fpath in
      Record.of_csv_string s in
    let merged = Record.update_grades published latest in
    let updated_ta_grade_sheet = Record.to_xlsx_sheets section_map merged in
    let* to_be_published = Record.to_csv_string merged |> map_err (fun s -> `Msg s) in
    let* xlsx_output_path = Fpath.of_string "updated-grades.xlsx" in
    let* () = Xlsx.write xlsx_output_path updated_ta_grade_sheet in
    let* csv_output_path = Fpath.of_string "updated-grades.csv" in
    Bos.OS.File.write csv_output_path to_be_published
  end

let new_spreadsheet exported_path =
  let open Result in
  get_lazy (fun (`Msg s) -> print_endline s) begin
    let* section_map, _ = load_config () in
    let* grade_spreadsheet = Fpath.of_string exported_path
      >>= Bos.OS.File.read
      >|= Record.of_csv_string
      >|= Record.to_xlsx_sheets section_map in
    let* path = Fpath.of_string "grades.xlsx" in
    Xlsx.write path grade_spreadsheet
  end

let () =
  let open Cmdliner in
  let rosters =
    let docs = "generate rosters" in
    let lab = Arg.required
      @@ Arg.opt ((fun s -> `Ok (Int.of_string s)), Option.pp Int.pp) None
      @@ Arg.info ~docs:"lab number" ["lab"] in
    let data_path = Arg.required
      @@ Arg.opt ((fun s -> `Ok (Some s)), Option.pp String.pp) None
      @@ Arg.info ~docs:"path to canvas exported csv file" ["input"; "i"] in
    Cmd.v (Cmd.info ~docs "rosters") Term.(const generate_rosters $ lab $ data_path) in
  let merge =
    let docs = "merge data files" in
    let published_path = Arg.required
      @@ Arg.opt ((fun s -> `Ok (Some s)), Option.pp String.pp) None
      @@ Arg.info ~docs:"path to canvas exported csv file" ["published"] in
    let latest_data_path = Arg.required
      @@ Arg.opt ((fun s -> `Ok (Some s)), Option.pp String.pp) None
      @@ Arg.info ~docs:"path to TA spreadsheet" ["latest"; "i"] in
    Cmd.v (Cmd.info ~docs "merge") Term.(const merge_data $ published_path $ latest_data_path) in
  let new_spreadsheet =
    let docs = "create new TA grading sheet" in
    let exported_path = Arg.required
      @@ Arg.opt ((fun s -> `Ok (Some s)), Option.pp String.pp) None
      @@ Arg.info ~docs:"path to canvas exported csv file" ["exported"] in
    Cmd.v (Cmd.info ~docs "new-spreadsheet") Term.(const new_spreadsheet $ exported_path) in
  let open_config =
    let docs = "open configuration file in text editor" in
    Cmd.v (Cmd.info ~docs "open-config") Term.(const open_config_in_editor $ const ()) in
  let main =
    let docs = "lab-tools" in
    Cmd.(group (info ~docs "lab-tools") [rosters; merge; new_spreadsheet; open_config]) in
  exit @@ Cmd.eval main
