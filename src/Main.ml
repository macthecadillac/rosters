open Containers
open Fun

open Common

let section_map = StringMap.of_list
  ["Hanyi",  List.map Section.of_int [3;  5;  9;  12];
   "Keilan", List.map Section.of_int  [4;  6;  10;  11];
   "Sheikh", List.map Section.of_int  [1;  2;  7;  8]]

let generate_rosters lab data_path =
  let open Result in
  let write_xlsx checkpoints rosters =
    let xlsx = Roster.to_xlsx lab checkpoints rosters in
    Fpath.of_string "test.xlsx" >>= Fun.flip Xlsx.write xlsx in
  let write_pdf checkpoints rosters =
    let pdf = Pdf.to_bytes @@ List.map (Pdf.of_roster lab checkpoints) rosters in
    Fpath.of_string "test.pdf" >>= Fun.flip Bos.OS.File.write pdf in
  catch ~ok:(const ()) ~err:(fun (`Msg s) -> print_endline s) begin
    let* fpath = Fpath.of_string data_path in
    let* s = Bos.OS.File.read fpath in
    let rosters = Record.of_csv_string s |> Roster.of_data |> Seq.to_list in
    let checkpoints = ["A"; "B2"; "C4&C6"; "D10"] in
    let* () = write_xlsx checkpoints rosters in
    write_pdf checkpoints rosters
  end

let merge_data published_path unpublished_path =
  let open Result in
  catch ~ok:(const ()) ~err:(fun (`Msg s) -> print_endline s) begin
    let* latest =
      let* fpath = Fpath.of_string unpublished_path in
      let* xlsx = Xlsx.read fpath in
      Record.of_xlsx_sheets xlsx |> of_opt |> map_err (fun s -> `Msg s) in
    let* published =
      let* fpath = Fpath.of_string published_path in
      let+ s = Bos.OS.File.read fpath in
      Record.of_csv_string s in
    let merged = Record.update_grades published latest in
    let updated_ta_grade_sheet = Record.to_xlsx_sheets section_map merged in
    let to_be_published = Record.to_csv_string merged in
    let* xlsx_output_path = Fpath.of_string "updated-grades.xlsx" in
    let* () = Xlsx.write xlsx_output_path updated_ta_grade_sheet in
    let* csv_output_path = Fpath.of_string "updated-grades.csv" in
    Bos.OS.File.write csv_output_path to_be_published
  end

let new_spreadsheet exported_path =
  let open Result in
  let grade_spreadsheet = Fpath.of_string exported_path
    >>= Bos.OS.File.read
    >|= Record.of_csv_string
    >|= Record.to_xlsx_sheets section_map in
  Xlsx.write <$> Fpath.of_string "grades.xlsx" <*> grade_spreadsheet
  |> catch ~ok:(const ()) ~err:(fun (`Msg s) -> print_endline s)

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
  let main =
    let docs = "lab-tools" in
    Cmd.(group (info ~docs "lab-tools") [rosters; merge; new_spreadsheet]) in
  exit @@ Cmd.eval main
