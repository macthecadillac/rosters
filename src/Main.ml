open Containers
open Fun

open Common

let to_string_err x = Result.map_err (fun (`Msg s) -> s) x

let read_config config =
  let open Result.Infix in
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
    | Error _ -> Result.pure None
    | Ok table ->
        List.map (uncurry (maybe_assoc f)) table
        |> Option.sequence_l
        |> Option.to_result "cannot read the \"checkpoints\" section"
        >>= List.map (fun (s, l) -> Option.map (flip Pair.make l) @@ to_int s)
            %> Option.sequence_l
            %> Option.to_result "cannot read the lab numbers"
        >|= IntMap.of_list %> Option.some in
  ta_assignment, checkpoints

let config_path = Option.to_result "no configuration found" begin
  let open Option.Infix in
  let module F = Bos.OS.File in
  let var str = 
    let* s = Bos.OS.Env.var str in
    let+ dir = Fpath.of_string s |> Option.of_result in
    Fpath.(dir / "lab-tools.toml") in
  match Sys.os_type with
    | "Win32" -> 
        let path1 = var "LOCALAPPDADA" >>= (F.must_exist %> Option.of_result) in
        let path2 = var "APPDATA" >>= (F.must_exist %> Option.of_result) in
        let path3 = Option.of_result @@ F.must_exist @@ Fpath.v "lab-tools.toml" in
        path1 <+> path2 <+> path3
    | "Unix" ->
        let path1 = var "XDG_CONFIG_HOME" >>= (F.must_exist %> Option.of_result) in
        let path2 =
          let* user_dir = Result.to_opt @@ Bos.OS.Dir.user () in
          Option.of_result (F.must_exist Fpath.(user_dir / ".config" / "lab-tools.toml")) in
        let path3 = Option.of_result @@ F.must_exist @@ Fpath.v "lab-tools.toml" in
        path1 <+> path2 <+> path3
    | _ -> None
  end

let load_config () =
  let open Result.Infix in
  let* path = config_path in
  let* toml = Bos.OS.File.read path
    |> Result.map_err @@ const @@ "no configuration found" in
  Otoml.Parser.from_string_result toml
    >>= read_config
    |> Result.add_ctx " malformed configuration file"

let open_config_in_editor () =
  let open Result.Infix in
  let* path = config_path in
  let* cmd = match Sys.os_type with
    | "Unix" -> Ok (Bos.Cmd.(v "open" % Fpath.to_string path))
    | "Win32" -> Ok (Bos.Cmd.v @@ Fpath.to_string path)
    | _ -> Error "unsupported platform for this option" in
  Bos.OS.Cmd.run cmd |> Result.map_err @@ const @@ "no configuration found"

let generate_rosters lab data_path =
  let open Result.Infix in
  let* ta_assignment, checkpoints_opt = load_config () in
  let write_xlsx checkpoints rosters =
    let xlsx = Roster.to_xlsx lab checkpoints rosters in
    Fpath.of_string "test.xlsx" |> to_string_err
    >>= Fun.flip Xlsx.write xlsx in
  let write_pdf checkpoints rosters =
    let rosters_m = SectionMap.of_list @@ List.map (fun x -> Roster.section x, x) rosters in
    let pdfs =
      let pdf_page = Pdf.of_roster lab checkpoints in
      let pdf_of_rosters = Pdf.to_bytes % List.map pdf_page in
      let ta_section_l = StringMap.to_list ta_assignment in
      let all = Pdf.to_bytes @@ List.map (Pdf.of_roster lab checkpoints) rosters in
      let f = List.filter_map (flip SectionMap.get rosters_m) in
      ("All", all) :: List.map (Pair.map_snd @@ pdf_of_rosters % f) ta_section_l in
    let fname s = Format.sprintf "Lab %i Rosters (%s Sections).pdf" lab s in
    (* something of a foldM with EitherT String IO () *)
    let iter_f acc (n, s) = acc >>= fun () ->
      Fpath.of_string (fname n) >>= flip Bos.OS.File.write s |> to_string_err in
    List.fold_left iter_f (Ok ()) pdfs in
  let checkpoints =
    let default = ["1"; "2"; "3"; "4"] in
    Option.(checkpoints_opt >>= IntMap.get lab |> get_or ~default) in
  let* fpath = Fpath.of_string data_path |> Result.map_err (fun (`Msg s) -> s) in
  let* s = Bos.OS.File.read fpath |> Result.map_err (fun (`Msg s) -> s) in
  let rosters = Record.of_csv_string s |> Roster.of_data |> Seq.to_list in
  let* () = write_xlsx checkpoints rosters in
  write_pdf checkpoints rosters

let merge_data published_path unpublished_path =
  let open Result.Infix in
  let* section_map, _ = load_config () in
  let* latest =
    let* fpath = Fpath.of_string unpublished_path |> to_string_err in
    let* xlsx = Xlsx.read fpath in
    Record.of_xlsx_sheets xlsx in
  let* published =
    let* fpath = Fpath.of_string published_path |> to_string_err in
    let+ s = Bos.OS.File.read fpath |> to_string_err in
    Record.of_csv_string s in
  let merged = Record.update_grades published latest in
  let* updated_ta_grade_sheet = Record.to_xlsx_sheets section_map merged in
  let* to_be_published = Record.to_csv_string merged in
  let* xlsx_output_path = Fpath.of_string "updated-grades.xlsx" |> to_string_err in
  let* () = Xlsx.write xlsx_output_path updated_ta_grade_sheet in
  let* csv_output_path = Fpath.of_string "updated-grades.csv" |> to_string_err in
  Bos.OS.File.write csv_output_path to_be_published |> to_string_err

let new_spreadsheet exported_path =
  let open Result.Infix in
  let* section_map, _ = load_config () in
  let* grade_spreadsheet = Fpath.of_string exported_path
    >>= Bos.OS.File.read |> to_string_err
    >|= Record.of_csv_string
    >>= Record.to_xlsx_sheets section_map in
  let* path = Fpath.of_string "grades.xlsx" |> to_string_err in
  Xlsx.write path grade_spreadsheet

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
  exit @@ Cmd.eval_result main
