open Containers
open Fun

open Common

let to_string_err x = Result.map_err (fun (`Msg s) -> s) x

let default_config =
"# This is an example configuration to help you get started. This file is
# already written to the correct location, so once you are done editing it,
# simply save and close your text editor. You can always access this file by
# running `lab-tools open-config`.
#
# This file is written in the TOML format. Lines prefixed with the \"#\" sign
# are comments and will be ignored.
#
# This section is mandatory--`lab-tools` won't run without it.
[ta-assignment]
# LHS is the name of the TA. There cannot be spaces within a name.
# RHS is the list of sections that the TA is assigned to. It must be a list of
# integers separated by commas.
Casey = [38, 40]
Harry = [17, 19, 29, 35]
Luke = [3, 21, 23, 33]
Kshitij = [7, 9, 28, 30]
Sophia = [10, 18, 20, 36]
Wanda = [5, 13, 15, 25]
Guru = [1, 11, 27, 31]
Hannah = [2, 12, 22, 32]
Billy = [6, 8, 16, 26]
Ting-Chun = [4, 14, 24, 34]
Aniket = [37, 39]

# This section is optional. `lab-tools` will run with default values if this is
# missing. This section is only used for roster generation. The values below are
# for 1AL.
[checkpoints]
# LHS must be in the form of \"lab\" followed by an integer.
# RHS is a list of strings. Entries must be enclosed in single or double quotes
# separted by commas.
lab1 = ['A', 'B5', 'C1', 'C2']
lab2 = ['B5', 'B6', 'Part C7']
lab3 = ['A7b', 'B7', 'B8/B9', 'B12']
lab4 = ['A4', 'B6', 'D1', 'D2']
lab5 = ['A2', 'B2', 'After C5', 'C6']
lab6 = ['A', 'B5', 'C']
lab7 = ['A3', 'B6', 'B13', 'C10']
lab8 = ['A6', 'A12', 'B2', 'B7']
lab9 = ['A3', 'B2', 'B9', 'C4']
"

let config_not_found_msg =
  "Configuration not found. `lab-tools` cannot run without first being " ^
  "configured. Edit the configuraton file by running `lab-tools open-config` " ^
  "then try again."

let read_config config =
  let open Result.Infix in
  let from_array f = function
    | Otoml.TomlArray l -> Option.sequence_l (List.map f l)
    | _ -> None in
  let maybe_assoc f s = Option.map (Pair.make s) % from_array f in
  let* ta_assignment =
    let f = function Otoml.TomlInteger i -> Some (Section.of_int i) | _ -> None in
    Otoml.find_result config Otoml.get_table ["ta-assignment"]
      >>= List.map (uncurry (maybe_assoc f))
      %> Option.sequence_l
      %> Option.to_result "cannot read the \"ta-assignment\" section"
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

let default_config_dir () =
  let open Option.Infix in
  let var str = Option.(Bos.OS.Env.var str >>= Fpath.of_string %> of_result) in
  let cwd = Bos.OS.Dir.current () |> Option.of_result in
  match Sys.os_type with
    | "Win32" -> var "LOCALAPPDADA" <+> cwd
    | "Unix" ->
        let p =
          let* user_dir = Result.to_opt @@ Bos.OS.Dir.user () in
          let config_dir = Fpath.(user_dir / ".config") in
          let+ _ = Option.of_result @@ Bos.OS.Dir.create config_dir in
          config_dir in
        var "XDG_CONFIG_HOME" <+> p <+> cwd
    | _ -> None

let config_path () =
  let open Result.Infix in
  let msg = "No configuration directory found. Abort." in
  let* config_dir = default_config_dir () |> Option.to_result msg in
  (* use .txt because systems might not know how to open .toml *)
  Bos.OS.File.must_exist Fpath.(config_dir / "lab-tools.txt")
    |> Result.map_err (const config_not_found_msg)

let write_default_config () =
  let open Result.Infix in
  let msg = "No configuration directory found. Abort." in
  let* config_dir = default_config_dir () |> Option.to_result msg in
  Bos.OS.File.write Fpath.(config_dir / "lab-tools.txt") default_config
    |> to_string_err

let load_config () =
  let open Result.Infix in
  config_path ()
    >>= Bos.OS.File.read
    %> Result.map_err (const config_not_found_msg)
    >>= Otoml.Parser.from_string_result
    >>= read_config
    |> Result.add_ctx " malformed configuration file"

let open_config_in_editor () =
  let open Result.Infix in
  let* () = if Result.is_error (config_path ())
            then write_default_config () else Ok () in
  let* path = config_path () in
  let* cmd = match Sys.os_type with
    | "Unix" -> Ok (Bos.Cmd.(v "open" % Fpath.to_string path))
    | "Win32" -> Ok (Bos.Cmd.v @@ Fpath.to_string path)
    | _ -> Error "unsupported platform for this option" in
  Bos.OS.Cmd.run cmd |> Result.map_err @@ const @@
  "Something went wrong. You can open the configuration file manually in a " ^
  "text editor such as Sublime, Notepad++ or TextEdit. On your system, the " ^
  "configuration " ^ Format.sprintf "file is located at %a" Fpath.pp path

let generate_rosters lab data_path output_dir =
  let open Result.Infix in
  let* ta_assignment, checkpoints_opt = load_config () in
  let* prefix = match output_dir with
    | Some s -> Fpath.of_string s >>= Bos.OS.Dir.must_exist |> to_string_err
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
      let ta_section_l = StringMap.to_seq ta_assignment in
      let all = Pdf.to_bytes @@ List.map (Pdf.of_roster lab checkpoints) rosters in
      let f = List.filter_map (flip SectionMap.get rosters_m) in
      Seq.cons ("All", all) @@ Seq.map (Pair.map_snd @@ pdf_of_rosters % f) ta_section_l in
    let fname s = Format.sprintf "Lab %i Blank Rosters (%s Sections).pdf" lab s in
    (* something of a foldM with EitherT String IO () *)
    let iter_f acc (n, s) = acc >>= fun () ->
      Fpath.(prefix / fname n) |> flip Bos.OS.File.write s |> to_string_err in
    Seq.fold_left iter_f (Ok ()) pdfs in
  let checkpoints =
    let default = ["1"; "2"; "3"; "4"] in
    Option.(checkpoints_opt >>= IntMap.get lab |> get_or ~default) in
  let* rosters =
    Fpath.of_string data_path |> to_string_err
    >>= Bos.OS.File.read %> to_string_err
    >>= Record.of_csv_string %> Roster.of_records in
  let* () = write_xlsx checkpoints rosters in
  write_pdf checkpoints rosters

let choose_path default user_path =
  let open Result.Infix in
  Option.get_or ~default user_path |> Fpath.of_string
    >>= rename_if_exists |> to_string_err

let merge_data published_path unpublished_path csv_output_path xlsx_output_path =
  let open Result.Infix in
  let* section_map, _ = load_config () in
  let* csv_out_fpath = choose_path "Updated Grades.csv" csv_output_path in
  let* xlsx_out_fpath = choose_path "Updated Grades.xlsx" xlsx_output_path in
  let* latest =
    Fpath.of_string unpublished_path |> to_string_err
    >>= Xlsx.read
    >>= Record.of_xlsx_sheets in
  let* published =
    Fpath.of_string published_path |> to_string_err
    >>= Bos.OS.File.read %> to_string_err
    >|= Record.of_csv_string in
  let merged = Record.update_grades published latest in
  let* updated_ta_grade_sheet = Record.to_xlsx_sheets section_map merged in
  let* to_be_published = Record.to_csv_string merged in
  let* () = Xlsx.write xlsx_out_fpath updated_ta_grade_sheet in
  Bos.OS.File.write csv_out_fpath to_be_published |> to_string_err

let new_spreadsheet exported_path output_path =
  let open Result.Infix in
  let* path = choose_path "Grades.xlsx" output_path in
  let* section_map, _ = load_config () in
  Fpath.of_string exported_path
    >>= Bos.OS.File.read |> to_string_err
    >|= Record.of_csv_string
    >>= Record.to_xlsx_sheets section_map
    >>= Xlsx.write path

let () =
  let open Cmdliner in
  let parse f pp = (fun s -> `Ok (f s)), Option.pp pp in
  let parse_string = parse Option.pure String.pp in
  let rosters =
    let doc = "generate rosters" in
    let lab = Arg.required @@ Arg.opt (parse Int.of_string Int.pp) None
      @@ Arg.info ~doc:"lab number" ["lab"] in
    let input = Arg.required @@ Arg.opt parse_string None
      @@ Arg.info ~doc:"path to canvas exported csv file" ["input"; "i"] in
    let output = Arg.value @@ Arg.opt parse_string None
      @@ Arg.info ~doc:"output directory" ["output"; "o"] in
    Cmd.v (Cmd.info ~doc "rosters") Term.(const generate_rosters $ lab $ input $ output) in
  let merge =
    let doc = "merge data files" in
    let left = Arg.required @@ Arg.opt parse_string None
      @@ Arg.info ~doc:"path to canvas exported csv file" ["published"] in
    let right = Arg.required @@ Arg.opt parse_string None
      @@ Arg.info ~doc:"path to TA spreadsheet" ["latest"; "i"] in
    let csv_out = Arg.value @@ Arg.opt parse_string None
      @@ Arg.info ~doc:"output csv" ["csv-out"] in
    let xlsx_out = Arg.value @@ Arg.opt parse_string None
      @@ Arg.info ~doc:"output xlsx" ["xlsx-out"] in
    Cmd.v (Cmd.info ~doc "merge") Term.(const merge_data $ left $ right $ csv_out $ xlsx_out) in
  let new_spreadsheet =
    let doc = "create new TA grading sheet" in
    let input = Arg.required @@ Arg.opt parse_string None
      @@ Arg.info ~doc:"path to canvas exported csv file" ["input"; "i"] in
    let output = Arg.value @@ Arg.opt parse_string None
      @@ Arg.info ~doc:"output file name" ["output"; "o"] in
    Cmd.v (Cmd.info ~doc "new-spreadsheet") Term.(const new_spreadsheet $ input $ output) in
  let open_config =
    let doc = "open configuration file in text editor" in
    Cmd.v (Cmd.info ~doc "open-config") Term.(const open_config_in_editor $ const ()) in
  let main =
    let doc = "lab-tools" in
    Cmd.(group (info ~doc "lab-tools") [rosters; merge; new_spreadsheet; open_config]) in
  exit @@ Cmd.eval_result main
