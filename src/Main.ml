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
      (* let write_attendance_sheet = *)
      (*   Roster.to_xlsx 3 ["1"; "2"; "3"; "4"] *)
      (*   %> Xlsx.write "test.xlsx" in *) 
      (* (1* print_tex rosters; *1) *)
      (* match write_attendance_sheet rosters with *)
      (* | Error e -> print_endline e *)
      (* | Ok _ -> () *)

      let pdf = Pdf.write (List.hd rosters) in
      let res = Fpath.of_string "/Users/maclee/Documents/code/lab_tools/test.pdf"
        >>= Fun.flip Bos.OS.File.write pdf in
      Result.get_exn res
