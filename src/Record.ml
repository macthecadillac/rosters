open Containers
open Common
open Fun
open Fun.Infix

type t = { name : Name.t;
           id : String.t Option.t;
           sis_user_id : String.t Option.t;
           sis_login_id : String.t Option.t;
           section : Section.t;
           grades : (String.t * Float.t Option.t) List.t }

let compare t1 t2 = Name.compare t1.name t2.name

let name { name; _ } = name

let section { section; _ } = section

let grades { grades; _ } = grades

let of_csv_string =
  let of_assoc l =
    let open Option in
    let* name = List.assoc_opt ~eq:String.(=) "Student" l >>= Name.of_string in
    let id = List.assoc_opt ~eq:String.(=) "ID" l in
    let sis_user_id = List.assoc_opt ~eq:String.(=) "SIS User ID" l in
    let sis_login_id = List.assoc_opt ~eq:String.(=) "SIS Login ID" l in
    let+ section = List.assoc_opt ~eq:String.(=) "Section" l >>= Section.of_string in
    let grades =
      let f (s, r) =
        if String.mem ~sub:"Conclusion " s then Some (s, Float.of_string_opt r)
        else None in
      List.filter_map f l in
    { name; id; sis_user_id; sis_login_id; section; grades } in
  Csv.of_string ~has_header:true
  %> Csv.Rows.input_all
  %> List.filter_map (Csv.Row.to_assoc %> of_assoc)

module NameMap = Map.Make (Name)

let to_csv_string records =
  let open Option in
  let to_csv_row record = 
    let name = Name.to_string record.name in
    let qname = if String.mem ~sub:"," name then Format.sprintf "\"%s\"" name else name in
    let* id = record.id in
    let* sis_user_id = record.sis_user_id in
    let* sis_login_id = record.sis_login_id in
    let+ section = Section.to_string record.section in
    let meta = [qname; id; sis_user_id; sis_login_id; section] in
    let grades = List.map (get_or ~default:"" % map Float.to_string % snd) record.grades in
    String.concat "," (meta @ grades) in
  let csv =
    let+ data = sequence_l @@ List.map to_csv_row records in
    let headers = "Student,ID,SIS User ID,SIS Login ID,Section" ^
      String.concat "," (List.map fst (List.hd records).grades) in
    String.concat "\n" (headers :: data) in
  Option.to_result "missing necessarily columns. unable to export" csv

let of_xlsx_sheets sheets =
  let row_to_record section header row =
    let open Option in
    let* record = flip List.combine row <$> header in
    let+ name = List.assoc_opt ~eq:String.(=) "Student" record
      >>= Xlsx.content %> Xlsx.Content.to_string
      >>= Name.of_string in
    let grades =
      let f (s, r) =
        if String.mem ~sub:"Conclusion " s then Some Xlsx.(s, Content.to_float @@ content r)
        else None in
      List.filter_map f record in
    { name; section; grades; id = None; sis_user_id = None; sis_login_id = None } in
  let record_list_of_sheet sheet =
    let open List in
    let* i = Option.(Xlsx.name sheet |> String.split_on_char ' '
                                     |> flip nth_opt 1 >>= Int.of_string
                                     |> to_list) in
    let section = Section.of_int i in
    let data = Xlsx.data sheet in
    let header = hd data |> map Xlsx.(content %> Content.to_string) |> Option.sequence_l in
    let+ row = tl data in
    row_to_record section header row in
  Option.sequence_l (List.flat_map record_list_of_sheet sheets)
  |> Option.to_result "unreocgnized xlsx format. abort"

let to_xlsx_sheets section_map records =
  let module SecM = SectionMap in
  let module X = Xlsx in
  let module O = Option in
  let m = List.map (fun x -> x.section, [x]) records
    |> SecM.add_list_with ~f:(fun _ a b -> a @ b) SecM.empty
    |> SecM.to_list
    |> List.sort (fun (a, _) (b, _) -> Section.compare a b) in
  (* take the first record of each sheet and extract the list of assignemnts *)
  let assignments = List.map fst @@ grades @@ List.hd @@ snd @@ List.hd m in
  let to_cells record = 
    let name = X.text_cell @@ Name.canonical @@ record.name in
    let to_cell = snd %> O.map X.float_cell %> O.get_or ~default:X.empty_cell in
    let grades = List.map to_cell record.grades in
    name :: grades in
  let grade_sheets =
    let sheet_of_records section records =
      let data = List.map to_cells @@ List.sort compare records in
      let header = List.map X.text_cell @@ "Student" :: assignments in
      let sheet =
        let sheet_name = Format.sprintf "Section %i" @@ Section.to_int section in
        X.new_sheet sheet_name (header :: data) |> X.freeze_row 1 |> X.freeze_col 1 in
      section, sheet in
    List.map (uncurry sheet_of_records) m in
  let nrows = SecM.of_list @@ List.map (Pair.map_snd X.num_rows) grade_sheets in
  let row_counts sections =
    let count_rows s = Option.map (Pair.make s) @@ SecM.find_opt s nrows in
    List.filter_map count_rows sections in
  let formula xlsx_f sections i _ =
    let ranges =
      let col = Char.chr @@ Char.code 'B' + i in
      let f s n = Format.sprintf "'Section %a'!%c%i:%c%i" Section.pp s col 2 col n in
      List.map (uncurry f) @@ row_counts sections in
    X.formula_cell @@ Format.sprintf "=%s(%s)" xlsx_f (String.concat "," ranges) in
  let ta_stats assignments ta sections =
    let assignment_stats func = List.mapi (formula func sections) assignments in
    [X.text_cell ta :: X.text_cell "AVG" :: assignment_stats "AVERAGE"]
    @ [X.empty_cell :: X.text_cell "STDEV" :: assignment_stats "STDEV"] in
  let summary_page = 
    let header = X.(text_cell "TA" :: empty_cell :: List.map text_cell assignments) in
    let data =
      List.flat_map (uncurry (ta_stats assignments)) @@ StringMap.to_list section_map in
    X.new_sheet "Summary" (header :: data) in
  summary_page :: List.map snd grade_sheets

let update_grades published =
  let m = NameMap.of_list (List.map (fun x -> x.name, x) published) in
  let f x = Option.map (fun t -> { t with grades = x.grades }) in
  List.fold_left (fun acc x -> NameMap.update x.name (f x) acc) m
  %> NameMap.to_list
  %> List.map snd
