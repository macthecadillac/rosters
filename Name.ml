open CCFun.Infix

exception ImpossibleBranch

type u = { first : String.t; last : String.t }

(* fnf = full name, first name first *)
type t = { name : u Option.t; fnf : String.t Option.t; str : String.t }

let to_string { str; _ } = str

let of_string s =
  let open String in
  match String.split_on_char ',' s with
  | [name] -> Ok { name = None; fnf = Some (trim name); str = s }
  | [f; l] -> Ok { name = Some { first = trim f; last = trim l };
                                 fnf = None;
                                 str = s }
  | _ -> Error "Unknown format for names"

let canonical = function
  | { name = Some { first; last }; _ } -> first ^ " " ^ last
  | { fnf = Some name; _ } -> name
  | _ -> raise ImpossibleBranch

let canonicalize = canonical %> of_string %> CCResult.get_exn

let compare a b = String.compare (canonical a) (canonical b)
