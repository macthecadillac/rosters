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

let rename_if_exists =
  let rec aux i s =
    let open Result.Infix in
    let s' = if i = 0 then Ok s else
      let fname, ext = Fpath.split_ext s in
      let+ fname' = Fpath.(of_string (to_string fname ^ "-" ^ Int.to_string i)) in
      Fpath.(fname' + ext) in
    let* b = s' >>= Bos.OS.File.exists in
    if b then aux (i + 1) s else s' in
  aux 0

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

let generate_rosters lab data_path output_dir =
  let open Result.Infix in
  let* ta_assignment, checkpoints_opt = load_config () in
  let* prefix = match output_dir with
    | Some s -> Fpath.of_string s |> to_string_err
    | None -> Bos.OS.Dir.current () |> to_string_err in
  let write_xlsx checkpoints rosters =
    let xlsx = Roster.to_xlsx lab checkpoints rosters in
    Fpath.(prefix / (Format.sprintf "Lab %i Summary Attendance Sheet.xlsx" lab))
    |> rename_if_exists |> to_string_err
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
    let fname s = Format.sprintf "Lab %i Blank Rosters (%s Sections).pdf" lab s in
    (* something of a foldM with EitherT String IO () *)
    let iter_f acc (n, s) = acc >>= fun () ->
      Fpath.(prefix / fname n) |> flip Bos.OS.File.write s |> to_string_err in
    List.fold_left iter_f (Ok ()) pdfs in
  let checkpoints =
    let default = ["1"; "2"; "3"; "4"] in
    Option.(checkpoints_opt >>= IntMap.get lab |> get_or ~default) in
  let* fpath = Fpath.of_string data_path |> Result.map_err (fun (`Msg s) -> s) in
  let* s = Bos.OS.File.read fpath |> Result.map_err (fun (`Msg s) -> s) in
  let rosters = Record.of_csv_string s |> Roster.of_data |> Seq.to_list in
  let* () = write_xlsx checkpoints rosters in
  write_pdf checkpoints rosters

let choose_path default user_path =
  let open Result.Infix in begin
    match user_path with
    | Some s -> Fpath.of_string s
    | None -> Fpath.of_string default
  end >>= rename_if_exists |> to_string_err

let merge_data published_path unpublished_path csv_output_path xlsx_output_path =
  let open Result.Infix in
  let* section_map, _ = load_config () in
  let* csv_out_fpath = choose_path "Updated Grades.csv" csv_output_path in
  let* xlsx_out_fpath = choose_path "Updated Grades.xlsx" xlsx_output_path in
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
  let* () = Xlsx.write xlsx_out_fpath updated_ta_grade_sheet in
  Bos.OS.File.write csv_out_fpath to_be_published |> to_string_err

let new_spreadsheet exported_path output_path =
  let open Result.Infix in
  let* path = choose_path "Grades.xlsx" output_path in
  let* section_map, _ = load_config () in
  let* grade_spreadsheet = Fpath.of_string exported_path
    >>= Bos.OS.File.read |> to_string_err
    >|= Record.of_csv_string
    >>= Record.to_xlsx_sheets section_map in
  Xlsx.write path grade_spreadsheet

let () =
  let open Cmdliner in
  let rosters =
    let doc = "generate rosters" in
    let lab = Arg.required
      @@ Arg.opt ((fun s -> `Ok (Int.of_string s)), Option.pp Int.pp) None
      @@ Arg.info ~doc:"lab number" ["lab"] in
    let input = Arg.required
      @@ Arg.opt ((fun s -> `Ok (Some s)), Option.pp String.pp) None
      @@ Arg.info ~doc:"path to canvas exported csv file" ["input"; "i"] in
    let output = Arg.value
      @@ Arg.opt ((fun s -> `Ok (Some s)), Option.pp String.pp) None
      @@ Arg.info ~doc:"output directory" ["output"; "o"] in
    Cmd.v (Cmd.info ~doc "rosters") Term.(const generate_rosters $ lab $ input $ output) in
  let merge =
    let doc = "merge data files" in
    let left = Arg.required
      @@ Arg.opt ((fun s -> `Ok (Some s)), Option.pp String.pp) None
      @@ Arg.info ~doc:"path to canvas exported csv file" ["published"] in
    let right = Arg.required
      @@ Arg.opt ((fun s -> `Ok (Some s)), Option.pp String.pp) None
      @@ Arg.info ~doc:"path to TA spreadsheet" ["latest"; "i"] in
    let csv_out = Arg.value
      @@ Arg.opt ((fun s -> `Ok (Some s)), Option.pp String.pp) None
      @@ Arg.info ~doc:"output csv" ["csv-out"] in
    let xlsx_out = Arg.value
      @@ Arg.opt ((fun s -> `Ok (Some s)), Option.pp String.pp) None
      @@ Arg.info ~doc:"output xlsx" ["xlsx-out"] in
    Cmd.v (Cmd.info ~doc "merge") Term.(const merge_data $ left $ right $ csv_out $ xlsx_out) in
  let new_spreadsheet =
    let doc = "create new TA grading sheet" in
    let input = Arg.required
      @@ Arg.opt ((fun s -> `Ok (Some s)), Option.pp String.pp) None
      @@ Arg.info ~doc:"path to canvas exported csv file" ["input"; "i"] in
    let output = Arg.value
      @@ Arg.opt ((fun s -> `Ok (Some s)), Option.pp String.pp) None
      @@ Arg.info ~doc:"output file name" ["output"; "o"] in
    Cmd.v (Cmd.info ~doc "new-spreadsheet") Term.(const new_spreadsheet $ input $ output) in
  let open_config =
    let doc = "open configuration file in text editor" in
    Cmd.v (Cmd.info ~doc "open-config") Term.(const open_config_in_editor $ const ()) in
  let main =
    let doc = "lab-tools" in
    Cmd.(group (info ~doc "lab-tools") [rosters; merge; new_spreadsheet; open_config]) in
  exit @@ Cmd.eval_result main
