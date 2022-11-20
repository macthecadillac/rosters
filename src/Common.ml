open Containers

type 'a printer = Format.formatter -> 'a -> unit

module IntMap = Map.Make(Int)

module Name = struct
  open Fun.Infix

  exception ImpossibleBranch

  type u = { first : String.t; last : String.t }

  (* fnf = full name, first name first *)
  type t = { name : u Option.t; fnf : String.t Option.t; str : String.t }

  let to_string { str; _ } = str

  let of_string s =
    let open String in
    match String.split_on_char ',' s with
    | [name] -> Some { name = None; fnf = Some (trim name); str = s }
    | [l; f] -> Some { name = Some { first = trim f; last = trim l };
                       fnf = None;
                       str = s }
    | _ -> None

  let canonical = function
    | { name = Some { first; last }; _ } -> Printf.sprintf "%s %s" first last
    | { fnf = Some name; _ } -> name
    | _ -> raise ImpossibleBranch

  let canonicalize = canonical %> of_string %> Option.get_exn_or ""

  let compare a b = String.compare (canonical a) (canonical b)

  let pp fmt t = Format.fprintf fmt "%a" String.pp (canonical t)
end

module Section = struct
  open Fun

  type t = String.t

  let of_string = id

  let to_string = id

  let compare = String.compare

  let to_int t =
    let open Option in
    String.split_on_char ' ' t |> List.rev |> flip List.nth_opt 1 >>= Int.of_string

  let validate t = Option.(const t <$> to_int t)

  let pp fmt = Format.fprintf fmt "%a" String.pp
end

module SectionMap = Map.Make (Section)
