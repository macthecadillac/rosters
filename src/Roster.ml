open Containers
open Fun.Infix
open Common

type t = Section.t * (Name.t List.t IntMap.t)

let section = fst

let groups = snd

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
  in section, groups

let of_csv_string =
  let of_assoc l =
    let open Option.Infix in
    let* name = List.assoc_opt ~eq:String.(=) "Student" l >>= Name.of_string in
    let+ section = List.assoc_opt ~eq:String.(=) "Section" l >>= Section.of_string in
    section, [name] in
  Csv.of_string ~has_header:true
  %> Csv.Rows.input_all
  %> List.filter_map (Csv.Row.to_assoc %> of_assoc)
  %> SectionMap.(add_list_with ~f:(fun _ a b -> a @ b) empty)
  %> SectionMap.to_list
  %> List.map (fun (s, l) -> new_roster s l)
  %> List.rev

let to_xlsx rosters =
  let open Monad.Reader in
  let open Xlsx in
  let summary_page sections =
    let+ { lab; _ } = ask in
    let header =
      List.(text_cell <$> [Format.sprintf "Lab %i" lab; "Check if complete"]) in
    let entries =
      List.(pure % text_cell % Format.sprintf "section %i" % Section.to_int <$> sections) in
    let data = header :: entries in
    let name = "summary" in
    Xlsx.new_sheet name data in
  let to_sheets (section, groups) =
    let instructions = [
       "Under the \"Signature\" column, leave blank if present, enter " ^
       "\"Absent\" if absent, describe circumstances if student left soon after quiz";
       "Under the \"Late\" column, enter the amount of time if they are late" ] in
    let instrs = List.(pure % set_type Bold % set_color Red % text_cell <$> instructions) in
    let+ { checkpoints; _ } = ask in
    let header =
      List.map (set_type Bold % text_cell)
      @@ ["Signature"; "Late"; "Group"; "Student"] @ checkpoints @ ["TA Check"] in
    let rows =
      let open List.Infix in
      let* group, names = IntMap.to_list groups |> List.rev in
      let+ name = names in
      let g = Format.sprintf "%i" group in
      [empty_cell; empty_cell; text_cell g; text_cell @@ Name.canonical name] in
    let data = instrs @ [header] @ rows in
    let name = Format.sprintf "section %i" @@ Section.to_int section in
    new_sheet name data in
  let* summary = summary_page @@ List.map section rosters in
  let+ sheets = traverse_l to_sheets rosters in
  summary :: sheets
