open Containers
open Fun
open Fun.Infix
open Common

type t = { section : Section.t;
           groups : Name.t List.t IntMap.t }

let section t = t.section

let groups t = t.groups

let shuffle l =
  let open Random in
  let step gen (i, x) =
    let* m = gen in
    let+ j = int i in
    IntMap.add i (IntMap.find j m) m |> IntMap.add j x in
  match l with
  | [] -> []
  | hd::tl ->
      let gen = pure (IntMap.singleton 0 hd) in
      let numbered = List.mapi (fun i -> Pair.make (i + 1)) tl in
      run (List.fold_left step gen numbered)
        |> IntMap.to_list
        |> List.map (fun (_, x) -> x)

let new_roster section students =
  let n = List.length students in
  let shuffled = shuffle students in
  let ngroups = Int.min 6 @@ n / 5 + if n mod 5 = 0 then 0 else 1 in
  let inds = List.init ngroups ((+) 1) |> Seq.(of_list %> cycle %> take n %> to_list) in
  let groups = List.combine_shortest inds shuffled
    |> List.fold_left (fun acc (i, n) ->
        IntMap.add i (n :: IntMap.get_or ~default:[] i acc) acc) IntMap.empty
  in { section; groups }

let of_data l =
  Seq.of_list l
  |> Seq.map (fun x -> Record.section x, [Record.name x])
  |> SectionMap.(add_seq_with ~f:(fun _ a b -> a @ b) empty)
  |> SectionMap.to_seq
  |> Seq.map (fun (s, l) -> new_roster s l)

let to_xlsx lab checkpoints rosters =
  let open List in
  let open Xlsx in
  let summary_page lab sections =
    let header =
      text_cell <$> [Format.sprintf "Lab %i" lab; "Check if complete"] in
    let entries =
      let+ s = sections in
      Section.to_int s |> Format.sprintf "section %i" |> text_cell |> pure in
    let data = header :: entries in
    let name = "summary" in
    Xlsx.new_sheet name data in
  let to_sheets checkpoints roster =
    let instructions = [
       "Under the \"Signature\" column, leave blank if present, enter \"Absent\" if absent, describe circumstances if student left soon after quiz";
       "Under the \"Late\" column, enter the amount of time if they are late" ] in
    let instrs =
      let+ s = instructions in
      text_cell s |> set_color Red |> set_type Bold |> pure in
    let header =
      let+ h = ["Signature"; "Late"; "Group"; "Student"] @ checkpoints @ ["TA Check"] in
      text_cell h |> set_type Bold in
    let rows =
      let* (group, names) = IntMap.to_list roster.groups
        |> List.sort (fun (i, _) (j, _) -> Int.compare i j) in
      let+ name = names in
      let g = Format.sprintf "%i" group in
      [empty_cell; empty_cell; text_cell g; text_cell @@ Name.canonical name] in
    let data = instrs @ [header] @ rows in
    let name = Format.sprintf "section %i" @@ Section.to_int roster.section in
    new_sheet name data in
  let summary = summary_page lab (section <$> rosters) in
  let sheets = (to_sheets checkpoints) <$> rosters in
  summary :: sheets
