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
  let open List in
  let data =
    let+ record = records in
    let name = Name.to_string record.name in
    let qname = if String.mem ~sub:"," name then Format.sprintf "\"%s\"" name else name in
    String.concat "," ([qname; Option.get_exn_or "" record.id;
                        Option.get_exn_or "" record.sis_user_id;
                        Option.get_exn_or "" record.sis_login_id;
                        Section.to_string record.section
                        |> Option.get_exn_or "unknown section"]
       @ List.map (Float.to_string % Option.get_or ~default:0.0 % snd) record.grades) in
  let headers = "Student,ID,SIS User ID,SIS Login ID,Section" ^
    String.concat "," (List.map fst (List.hd records).grades) in
  String.concat "\n" (headers :: data)

let of_xlsx_sheets t =
  let open List in
  let l =
    let* sheet = t in
    let section = Xlsx.name sheet |> String.split_on_char ' '
                               |> flip nth 1 |> Int.of_string
                               |> Option.get_exn_or "Invalid section"
                               |> Section.of_int in
    let data = Xlsx.data sheet in
    let header = hd data |> map Xlsx.(content %> Content.to_string) |> Option.sequence_l in
    let+ row = tl data in
    let open Option in
    let* record = flip combine row <$> header in
    let* name = List.assoc_opt ~eq:String.(=) "Student" record
      >>= Xlsx.content %> Xlsx.Content.to_string
      >>= Name.of_string in
    let grades =
      let f (s, r) =
        if String.mem ~sub:"Conclusion " s then Some Xlsx.(s, Content.to_float @@ content r)
        else None in
      List.filter_map f record in
    pure { name; section; grades; id = None; sis_user_id = None; sis_login_id = None } in
  Option.sequence_l l

let to_xlsx_sheets records =
  let m = List.map (fun x -> x.section, [x]) records
    |> SectionMap.add_list_with ~f:(fun _ a b -> a @ b) SectionMap.empty
    |> SectionMap.to_list in
  let comp = compare in
  let open List in
  let+ (section, records) = m in
  let data =
    let* record = sort comp records in
    let name = record.name |> Name.canonical |> Xlsx.text_cell in
    let grades = 
      let+ (_, grade) = record.grades in
      Option.map Xlsx.float_cell grade |> Option.get_or ~default:Xlsx.empty_cell in
    pure @@ name :: grades in
  let header = hd m |> snd |> hd |> grades >|= fst |> cons "Name" >|= Xlsx.text_cell in
  Xlsx.new_sheet (Format.sprintf "section %i" @@ Section.to_int section) (header :: data)
    |> Xlsx.freeze_row 1
    |> Xlsx.freeze_col 1

let update_grades published =
  let m = NameMap.of_list (List.map (fun x -> x.name, x) published) in
  let f x = Option.map (fun t -> { t with grades = x.grades }) in
  List.fold_left (fun acc x -> NameMap.update x.name (f x) acc) m
  %> NameMap.to_list
  %> List.map snd
