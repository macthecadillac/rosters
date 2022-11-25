open Containers
open Fun.Infix

open Common

type t = Xlsx.sheet List.t

let summary_page lab sections =
  let open List in
  let header =
    Xlsx.text_cell <$> [Format.sprintf "Lab %i" lab; "Check if complete"] in
  let entries =
    let+ s = sections in
    Section.to_int s |> Format.sprintf "section %i" |> Xlsx.text_cell |> pure in
  let data = header :: entries in
  let name = "summary" in
  Xlsx.new_sheet name data

let of_roster checkpoints roster =
  let open List in
  let open Xlsx in
  let instructions = [
     "Under the \"Signature\" column, leave blank if present, enter \"Absent\" if absent, describe circumstances if student left soon after quiz";
     "Under the \"Late\" column, enter the amount of time if they are late" ] in
  let instrs =
    let+ s = instructions in
    text_cell s |> set_color Red |> set_type Bold |> pure in
  let header =
    let+ h = ["Signature"; "Late"; "Group"; "Student"; "TA Check"] @ checkpoints in
    text_cell h |> set_type Bold in
  let rows =
    let* (group, names) = IntMap.to_list @@ Roster.groups roster in
    let+ name = names in
    let g = Format.sprintf "%i" group in
    [empty_cell; empty_cell; text_cell g; text_cell @@ Name.canonical name] in
  let data = instrs @ [header] @ rows in
  let name = Format.sprintf "section %i" @@ Section.to_int @@ Roster.section roster in
  new_sheet name data

let of_rosters lab checkpoints rosters =
  let summary = summary_page lab (List.map Roster.section rosters) in
  let sheets = List.map (of_roster checkpoints) rosters in
  summary :: sheets
