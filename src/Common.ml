open Containers

type 'a printer = Format.formatter -> 'a -> unit

module IntMap = Map.Make (Int)

module Name = struct
  open Fun.Infix

  type u = { first : String.t; last : String.t }

  (* String.t is the original string *)
  type t = FirstLast of u * String.t | Formatted of String.t

  let to_string = function FirstLast (_, s)  | Formatted s -> s

  let of_string s =
    let open String in
    match split_on_char ',' s with
    | [name] -> Some (Formatted name) 
    | [l; f] -> Some (FirstLast ({ first = trim f; last = trim l }, s))
    | _ -> None

  let canonical = function
    | FirstLast ({ first; last }, _) -> Printf.sprintf "%s %s" first last
    | Formatted name -> String.trim name

  let canonicalize = canonical %> of_string %> Option.get_exn_or ""

  let compare a b = String.compare (canonical a) (canonical b)

  let pp fmt t = Format.fprintf fmt "%a" String.pp (canonical t)
end

module Section = struct
  open Fun

  type t = { num : Int.t; str : String.t Option.t }

  let compare t1 t2 = Option.compare String.compare t1.str t2.str

  let to_int { num; _ } = num

  let of_int num = { num; str = None }

  let of_string str =
    let open Option in
    String.split_on_char ' ' str
      |> List.rev
      |> flip List.nth_opt 1
      >>= Int.of_string
      >>= fun num -> pure { num; str = Some str }

  let to_string { str; _ } = str

  let pp fmt t = Format.fprintf fmt "%i" @@ to_int t
end

module SectionMap = Map.Make (Section)
