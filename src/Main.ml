open Containers
open Fun.Infix

let () =
  let open Result in
  let csvStr = Fpath.of_string "/Users/maclee/Documents/code/lab_tools/test.csv"
    >>= Bos.OS.File.read in
  match csvStr with
  | Error (`Msg s) -> print_endline s
  | Ok s ->
      let rosters = Record.of_csv_string s |> Roster.of_data |> Seq.to_list in
      let lab = 1 in
      let checkpoints = ["A"; "B2"; "C4&C6"; "D10"] in
      let write_attendance_sheet =
        Roster.to_xlsx lab checkpoints
        %> Xlsx.write "test.xlsx" in 
      match write_attendance_sheet rosters with
      | Error e -> print_endline e
      | Ok _ -> ();
      let pdf = Pdf.write @@ List.map (Pdf.write_page lab checkpoints) rosters in
      let res = Fpath.of_string "/Users/maclee/Documents/code/lab_tools/test.pdf"
        >>= Fun.flip Bos.OS.File.write pdf in
      Result.get_exn res
