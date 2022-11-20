open Containers
open Common
open Fun
open Fun.Infix

let pps s fmt () = Format.fprintf fmt s
let pair_pp = Pair.pp ~pp_start:(pps "(")
                      ~pp_stop:(pps ")")
                      String.pp
                      (Option.pp Float.pp)
let list_pp = (List.pp ~pp_start:(pps "[")
                       ~pp_stop:(pps "]")
                       ~pp_sep:(pps ";\n                   ")
                       pair_pp)

module Published = struct
  type t = { name : Name.t;
             id : String.t;
             sis_user_id : String.t;
             sis_login_id : String.t;
             section : Section.t;
             grades : (String.t * Float.t Option.t) List.t }

  let pp fmt { name; id; sis_user_id; sis_login_id; section; grades } = 
    Format.fprintf fmt "Record { name : %a;\n         " Name.pp name;
    Format.fprintf fmt "id : %a;\n         " String.pp id;
    Format.fprintf fmt "sis_user_id : %a;\n         " String.pp sis_user_id;
    Format.fprintf fmt "sis_login_id : %a;\n         " String.pp sis_login_id;
    Format.fprintf fmt "section : %a;\n         " Section.pp section;
    Format.fprintf fmt "grades : @[<h>%a@] }" list_pp grades

  let of_assoc l =
    let open Option in
    let* name = List.assoc_opt ~eq:String.(=) "Student" l >>= Name.of_string in
    let* id = List.assoc_opt ~eq:String.(=) "ID" l in
    let* sis_user_id = List.assoc_opt ~eq:String.(=) "SIS User ID" l in
    let* sis_login_id = List.assoc_opt ~eq:String.(=) "SIS Login ID" l in
    let+ section = Section.of_string <$> List.assoc_opt ~eq:String.(=) "Section" l in
    let grades =
      let f (s, r) =
        if String.mem ~sub:"Conclusion " s then Some (s, Float.of_string_opt r)
        else None in
      List.filter_map f l in
    { name; id; sis_login_id; sis_user_id; section; grades }

  let of_string =
    Csv.Rows.load ~has_header:true
    %> List.map Csv.Row.to_assoc 
    %> List.map of_assoc
    %> List.filter_map id
end

module TAGradeSheet = struct
  type t = { name : Common.Name.t;
             grades : (String.t * Float.t Option.t) List.t }

  let pp fmt { name; grades } = 
    Format.fprintf fmt "Record { name : %a;\n         " Name.pp name;
    Format.fprintf fmt "grades : @[<h>%a@] }" list_pp grades

  let of_assoc l =
    let open Option in
    let+ name = List.assoc_opt ~eq:String.(=) "Student" l >>= Name.of_string in
    let grades =
      let f (s, r) =
        if String.mem ~sub:"Conclusion " s then Some (s, Float.of_string_opt r)
        else None in
      List.filter_map f l in
    { name; grades }
end
