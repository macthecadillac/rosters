open Containers
open Fun.Infix

open Common

let to_string_err x = Result.map_err (fun (`Msg s) -> s) x

let default_config =
"# This is an example configuration to help you get started. This file is
# already written to the correct location, so once you are done editing it,
# simply save and close your text editor. You can always access this file by
# running `rosters configure`. To remove the configuration, simply run
# `rosters reset`.

# Configuration is entirely optional. `rosters` will run
# with default values if a given configuration key is not found.

# This file is written in the TOML format. Lines prefixed with the \"#\" sign
# are comments and will be ignored.

[ta-assignment]
# LHS is the name of the TA. There cannot be spaces within a name.
# RHS is the list of sections that the TA is assigned to. It must be a list of
# integers separated by commas.
Joe = [38, 40]
Donny = [17, 19, 29, 35]
Barry = [3, 21, 23, 33]
Walker = [7, 9, 28, 30]
Billy = [10, 18, 20, 36]
Herbert = [5, 13, 15, 25]
Ronny = [1, 11, 27, 31]
Jimmy = [2, 12, 22, 32]
Jerry = [6, 8, 16, 26]
Ricky = [4, 14, 24, 34]
Lyndon = [37, 39]

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

let read_config config =
  let open Result.Infix in
  let from_array f = function
    | Otoml.TomlArray l -> Option.sequence_l (List.map f l)
    | _ -> None in
  let maybe_assoc f s = Option.map (Pair.make s) % from_array f in
  let* ta_assignment =
    let f = function Otoml.TomlInteger i -> Some (Section.of_int i) | _ -> None in
    match Otoml.find_result config Otoml.get_table ["ta-assignment"] with
    | Error _ -> Result.pure None
    | Ok table ->
        List.map (Fun.uncurry (maybe_assoc f)) table
        |> Option.sequence_l
        |> Option.to_result "cannot read the \"ta-assignment\" section"
        >|= StringMap.of_list %> Option.some in
  let+ checkpoints =
    let f = function Otoml.TomlString s -> Some s | _ -> None in
    let to_int s = Str.replace_first (Str.regexp {|lab\([0-9]+\)|}) {|\1|} s
      |> Int.of_string in
    match Otoml.find_result config (Otoml.get_table) ["checkpoints"] with
    | Error _ -> Result.pure None
    | Ok table ->
        List.map (Fun.uncurry (maybe_assoc f)) table
        |> Option.sequence_l
        |> Option.to_result "cannot read the \"checkpoints\" section"
        >>= List.map (fun (s, l) -> Option.map (Fun.flip Pair.make l) @@ to_int s)
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
  let open Option.Infix in
  let* config_dir = default_config_dir () in
  (* use .txt because systems might not know how to open .toml *)
  Bos.OS.File.must_exist Fpath.(config_dir / "rosters.txt") |> Option.of_result

let write_default_config () =
  let open Result.Infix in
  let msg = "No configuration directory found. Abort." in
  let* config_dir = default_config_dir () |> Option.to_result msg in
  Bos.OS.File.write Fpath.(config_dir / "rosters.txt") default_config
    |> to_string_err

let load_config () =
  let open Option.Infix in
  let+ cpath = config_path () in
    let open Result.Infix in
    Otoml.Parser.from_file_result (Fpath.to_string cpath)
    >>= read_config |> Result.add_ctx " error while loading configuration"

let open_config_in_editor () =
  let open Result.Infix in
  let* () = if Option.is_none (config_path ())
            then write_default_config () else Ok () in
  (* shouldn't fail *)
  let* path = config_path () |> Result.of_opt in
  let* cmd = match Sys.os_type with
    | "Unix" -> Ok (Bos.Cmd.(v "open" % Fpath.to_string path))
    | "Win32" -> Ok (Bos.Cmd.v @@ Fpath.to_string path)
    | _ -> Error "unsupported platform for this option" in
  Bos.OS.Cmd.run cmd |> Result.map_err @@ Fun.const @@
  "Something went wrong. You can open the configuration file manually in a " ^
  "text editor such as Sublime, Notepad++ or TextEdit. On your system, the " ^
  "configuration " ^ Format.sprintf "file is located at %a" Fpath.pp path

let remove_config () =
  match config_path () with
  | None -> Ok ()
  | Some cpath -> Bos.OS.File.delete cpath |> Fun.const (Ok ())

let generate_rosters lab data_path output_dir =
  let open Result.Infix in
  let* ta_assignment_opt, checkpoints_opt =
    Option.get_or ~default:(Ok (None, None)) @@ load_config () in
  let env =
    let checkpoints =
      let default = ["1"; "2"; "3"; "4"] in
      Option.(checkpoints_opt >>= IntMap.get lab |> get_or ~default) in
    let l = Length.of_in 8.5 in  (* letter size width *)
    let h = Length.of_in 11.0 in  (* letter size height *)
    let margin = Length.of_in 0.85 in  (* page margins *)
    let origin = margin, margin in  (* starting anchor from the top-left *)
    let lwidth1 = Length.of_mm 0.3 in  (* table border width 1 *)
    let lwidth2 = Length.of_mm 0.7 in  (* table border width 2 *)
    let weight = Regular in  (* PDF font weight *)
    let font_size = FS (Length.of_pt 11.) in  (* PDF font size *)
    { l; h; margin; origin; lwidth1; lwidth2; weight; font_size; lab; checkpoints } in
  let* prefix = match output_dir with
    | Some s -> Fpath.of_string s >>= Bos.OS.Dir.must_exist |> to_string_err
    | None -> Bos.OS.Dir.current () |> to_string_err in
  let write_xlsx rosters =
    let xlsx = Monad.Reader.run (Roster.to_xlsx rosters) env in
    Fpath.(prefix / (Format.sprintf "Lab %i Summary Attendance Sheet.xlsx" lab))
    |> rename_if_exists |> to_string_err
    >>= Fun.flip Xlsx.write xlsx in
  let write_pdf section_groups rosters =
    let pdfs =
      let open Monad.Reader in
      let pdf_of_rosters l = traverse_l Pdf.of_roster l >>= Pdf.to_bytes in
      let all = traverse_l Pdf.of_roster rosters >>= Pdf.to_bytes in
      let m = SectionMap.of_list @@ List.map (fun x -> Roster.section x, x) rosters in
      let f = List.filter_map (Fun.flip SectionMap.get m) in
      Seq.cons (`N "All", all)
      @@ Seq.map (Pair.map_snd @@ pdf_of_rosters % f)
      @@ Seq.of_list section_groups in
    let fname = function
      | `N s -> Format.sprintf "Lab %i Blank Rosters (%s Sections).pdf" lab s
      | `S s -> Format.sprintf "Lab %i Blank Rosters (Section %a).pdf" lab Section.pp s in
    (* something of a foldM with EitherT String IO () *)
    let iter_f acc (n, m) = acc >>= fun () ->
      Fpath.(prefix / fname n)
      |> Fun.flip Bos.OS.File.write (Monad.Reader.run m env)
      |> to_string_err in
    Seq.fold_left iter_f (Ok ()) pdfs in
  let* rosters =
    Fpath.of_string data_path |> to_string_err
    >>= Bos.OS.File.read %> to_string_err
    >|= Roster.of_csv_string in
  let section_groups =
    Option.get_or ~default:(List.map Roster.(fun x -> `S (section x), [section x]) rosters)
    @@ Option.map (StringMap.to_list %> List.map (fun (x, y) -> `N x, y)) ta_assignment_opt in
  let* () = write_xlsx rosters in
  write_pdf section_groups rosters

let () =
  let open Cmdliner in
  let parse f pp = (fun s -> `Ok (f s)), Option.pp pp in
  let parse_string = parse Option.pure String.pp in
  let generate =
    let doc = "generate rosters" in
    let lab = Arg.required @@ Arg.opt (parse Int.of_string Int.pp) None
      @@ Arg.info ~doc:"lab number" ["lab"] in
    let input = Arg.required @@ Arg.opt parse_string None
      @@ Arg.info ~doc:"path to canvas exported csv file" ["input"; "i"] in
    let output = Arg.value @@ Arg.opt parse_string None
      @@ Arg.info ~doc:"output directory" ["output"; "o"] in
    Cmd.v (Cmd.info ~doc "generate") Term.(const generate_rosters $ lab $ input $ output) in
  let configure =
    let doc = "open configuration file in text editor" in
    Cmd.v (Cmd.info ~doc "configure") Term.(const open_config_in_editor $ const ()) in
  let reset =
    let doc = "remove configuration" in
    Cmd.v (Cmd.info ~doc "reset") Term.(const remove_config $ const ()) in
  let main =
    let doc = "rosters" and version = "w22" in
    Cmd.(group (info ~doc ~version "rosters") [generate; configure; reset]) in
  exit @@ Cmd.eval_result main
