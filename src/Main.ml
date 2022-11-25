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
      (* let print_tex = *)
      (*   List.map (CodeGen.of_section 3 ["1"; "2"; "3"; "4"]) *)
      (*   %> List.fold_left CodeGen.(<+>) CodeGen.empty *)
      (*   %> CodeGen.to_string *)
      (*   %> Format.printf "%s\n" in *)
      let write_attendance_sheet =
        AttendanceSheet.of_rosters 3 ["1"; "2"; "3"; "4"]
        %> Xlsx.write "test.xlsx" in 
      (* print_tex rosters; *)
      match write_attendance_sheet rosters with
      | Error e -> print_endline e
      | Ok _ -> ()
