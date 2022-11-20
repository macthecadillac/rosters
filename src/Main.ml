open Containers

external hello_world: unit -> string = "hello_world"

let () =
  Data.Published.of_string "/Users/maclee/Documents/code/lab_tools/test.csv"
  |> Rosters.of_data
  |> Seq.map (CodeGen.of_section 3 ["1"; "2"; "3"; "4"])
  |> Seq.fold_left CodeGen.(<+>) CodeGen.empty
  |> CodeGen.to_string
  |> Format.printf "%s\n"
