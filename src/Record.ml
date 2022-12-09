open Containers
open Common
open Fun
open Fun.Infix

type t = { name : (Name.t, String.t) Result.t;
           id : (String.t, String.t) Result.t;
           sis_user_id : String.t;
           sis_login_id : (String.t, String.t) Result.t;
           section : (Section.t, String.t) Result.t;
           grades : (String.t * Float.t Option.t) List.t }

let pp fmt t =
  Format.fprintf fmt "{\n";
  Format.fprintf fmt "  name : %a\n" (Result.pp Name.pp) t.name;
  Format.fprintf fmt "  id : %a\n" (Result.pp String.pp) t.id;
  Format.fprintf fmt "  sis_user_id : %s\n" t.sis_user_id;
  Format.fprintf fmt "  sis_login_id : %a\n" (Result.pp String.pp) t.sis_login_id;
  Format.fprintf fmt "  section : %a\n" (Result.pp Section.pp) t.section;
  Format.fprintf fmt "  grades : %a\n" (List.pp (Pair.pp String.pp (Option.pp Float.pp))) t.grades;
  Format.fprintf fmt "}"

let compare t1 t2 =
  match t1.name, t2.name, t1.section, t2.section with
  | Ok n1, Ok n2, Ok s1, Ok s2 ->
    Pair.compare Section.compare Name.compare (s1, n1) (s2, n2)
  | _ -> String.compare t1.sis_user_id t2.sis_user_id

let name { name; _ } = name

let section { section; _ } = section

let grades { grades; _ } = grades

let no_valid_records_msg =
  "No valid records found. Make sure that you are using the csv file exported " ^
  "directly from canvas with all the necesssary columns."

let of_csv_string =
  let of_assoc l =
    let open Option.Infix in
    let* name = List.assoc_opt ~eq:String.(=) "Student" l >>= Name.of_string in
    let id = List.assoc_opt ~eq:String.(=) "ID" l
      |> Option.to_result "ID not found" in
    let* sis_user_id = List.assoc_opt ~eq:String.(=) "SIS User ID" l in
    let sis_login_id = List.assoc_opt ~eq:String.(=) "SIS Login ID" l
      |> Option.to_result "SIS Login ID not found" in
    let+ section = List.assoc_opt ~eq:String.(=) "Section" l >>= Section.of_string in
    let grades =
      let f (s, r) =
        if String.mem ~sub:"Conclusion " s then Some (s, Float.of_string_opt r)
        else None in
      List.filter_map f l in
    { name = Ok name; id; sis_user_id; sis_login_id; section = Ok section; grades } in
  Csv.of_string ~has_header:true
  %> Csv.Rows.input_all
  %> List.filter_map (Csv.Row.to_assoc %> of_assoc)

module NameMap = Map.Make (Name)

let to_csv_string = function
  | [] -> Error no_valid_records_msg
  | (hd :: _) as records ->
      let open Result.Infix in
      let to_csv_row record = 
        let* name = Name.to_string <$> record.name in
        let qname = if String.mem ~sub:"," name
                    then Format.sprintf "\"%s\"" name
                    else name in
        let row =
          let* id = record.id in
          let sis_user_id = record.sis_user_id in
          let* sis_login_id = record.sis_login_id in
          let+ section = record.section >>= Section.to_string
            %> Option.to_result "no section string found" in
          let meta = [qname; id; sis_user_id; sis_login_id; section] in
          let grades = List.map Option.(get_or ~default:"" % map Float.to_string % snd)
                                record.grades in
          String.concat "," (meta @ grades) in
        Result.add_ctx (Format.sprintf "%a: " pp record) row in
      let+ data = Result.map_l to_csv_row records in
      let headers = "Student,ID,SIS User ID,SIS Login ID,Section" ^
        String.concat "," (List.map fst hd.grades) in
      String.concat "\n" (headers :: data)

let of_xlsx_sheets sheets =
  let pp =
    let pp s fmt () = Format.fprintf fmt s in
    let pair_pp = Pair.pp ~pp_start:(pp "") ~pp_stop:(pp "") ~pp_sep:(pp ": ") in
    let list_pp = List.pp ~pp_start:(pp "{ ") ~pp_stop:(pp " }") ~pp_sep:(pp "; ") in
    list_pp @@ pair_pp String.pp Xlsx.cell_pp in
  let module C = Xlsx.Content in
  let open Result.Infix in
  let row_to_record header row =
    let* record = flip List.combine row <$> header in
    let str_opt s p =
      let open Option.Infix in
      let to_string = Format.sprintf "%s cannot be parsed from Excel record: %a" s pp in
      List.assoc_opt ~eq:String.(=) s record
        >>= Xlsx.content %> p
        |> Option.to_result (to_string record) in
    let name =
      let* s = str_opt "Student" C.to_string in
      Name.of_string s |> Option.to_result (Format.sprintf "%s is not a valid name" s) in
    let+ sis_user_id = str_opt "ID" C.to_string in
    let section = str_opt "Section" C.to_float >|= (Int.of_float %> Section.of_int) in
    let grades =
      let f (s, r) =
        if String.mem ~sub:"Conclusion " s then Some Xlsx.(s, C.to_float @@ content r)
        else None in
      List.filter_map f record in
    { name; section; grades; id = Error "ID unknown" ;
      sis_user_id; sis_login_id = Error "SIS Login ID unknown" } in
  let record_list_of_sheet sheet =
    match Xlsx.data sheet with
    | [] -> Error "Empty sheet"
    | hd :: tl ->
        let header =
          let f c = Xlsx.content c
            |> C.to_string
            |> Option.to_result (Format.sprintf "%a is not a valid header" Xlsx.cell_pp c) in
          Result.map_l f hd in
        Result.map_l (row_to_record header) tl in
  let data =
    let f x = match Xlsx.name x with "Summary" -> None | n -> Some x in
    List.filter_map f sheets in
  List.flatten <$> Result.map_l record_list_of_sheet data

let to_xlsx_sheets section_map records =
  let module SecM = SectionMap in
  let module X = Xlsx in
  let module O = Option in
  let module L = List in
  let open Result.Infix in
  let tas = L.rev @@ L.map fst @@ StringMap.to_list section_map in
  let* m = Result.map_l (fun x -> x.section >>= fun s -> Ok (s, [x])) records
    >|= SecM.add_list_with ~f:(fun _ a b -> a @ b) SecM.empty in
  let ml = m |> SecM.to_list |> L.rev in
  (* take the first record of each sheet and extract the list of assignemnts *)
  let* assignments =
    O.(L.head_opt ml >>= snd %> L.head_opt >|= grades %> L.map fst)
    |> Option.to_result no_valid_records_msg in
  let to_cells record = 
    let* name = X.text_cell % Name.canonical <$> record.name in
    let+ section = X.float_cell % Float.of_int % Section.to_int <$> record.section in
    let sid = X.text_cell @@ record.sis_user_id in
    let to_cell = snd %> O.map X.float_cell %> O.get_or ~default:X.empty_cell in
    let grades = L.map to_cell record.grades in
    name :: sid :: section :: grades in
  let* grade_sheets =
    let sheet_of_records records ta =
      let* sections = StringMap.get ta section_map
        |> O.to_result (Format.sprintf "%s is not a TA in the configuration" ta) in
      let header = L.map X.text_cell ("Student" :: "ID" :: "Section" :: assignments) in
      let records_to_rows records = Result.map_l to_cells @@ L.sort compare records in
      L.map (flip SecM.get records) sections
        |> O.sequence_l
        |> O.to_result ("Some sections defined in your configuration are not " ^
                        "in the canvas csv file. Are you sure this is the " ^
                        "right input file?")
        >|= L.flatten >>= records_to_rows >|= L.cons header
        %> X.new_sheet ta %> X.freeze_col 3 %> X.freeze_row 1 %> Pair.make ta in
    Result.map_l (sheet_of_records m) tas in
  let nrows = StringMap.of_list @@ L.map (Pair.map_snd X.num_rows) grade_sheets in
  let row_count s = StringMap.find_opt s nrows in
  let formula xlsx_f ta i _ =
    let range =
      let col = Char.chr @@ Char.code 'D' + i in
      let f n = Format.sprintf "'%s'!%c%i:%c%i" ta col 2 col n in
      f <$> O.to_result (Format.sprintf "%s is not a TA in the configuration" ta)
      @@ row_count ta in
    X.formula_cell % Format.sprintf "=%s(%s)" xlsx_f <$> range in
  let ta_stats assignments ta sections =
    let assignment_stats func = Result.flatten_l @@ L.mapi (formula func ta) assignments in
    let* avg_stats = assignment_stats "AVERAGE" in
    let+ std_stats = assignment_stats "STDEV" in
    [X.text_cell ta :: X.text_cell "AVG" :: avg_stats]
    @ [X.empty_cell :: X.text_cell "STDEV" :: std_stats] in
  let+ summary_page = 
    let header = X.(text_cell "TA" :: empty_cell :: L.map text_cell assignments) in
    let+ data = Result.map_l (uncurry @@ ta_stats assignments)
      @@ L.rev
      @@ StringMap.to_list section_map in
    X.new_sheet "Summary" (header :: L.flatten data) in
  summary_page :: L.map snd grade_sheets

let update_grades published =
  let m = StringMap.of_list (List.map (fun x -> x.sis_user_id, x) published) in
  let f x = Option.map (fun t -> { t with grades = x.grades }) in
  List.fold_left (fun acc x -> StringMap.update x.sis_user_id (f x) acc) m
  %> StringMap.to_list
  %> List.map snd
