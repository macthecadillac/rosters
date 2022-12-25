open Containers
open Fun

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
  type t = Int.t

  let compare = Int.compare

  let to_int t = t

  let of_int n = n

  let of_string str =
    let open Option in
    String.split_on_char ' ' str
      |> List.rev
      |> flip List.nth_opt 1
      >>= Int.of_string

  let pp fmt t = Format.fprintf fmt "%i" @@ to_int t
end

module SectionMap = Map.Make (Section)
module StringMap = Map.Make (String)

module Length = struct
  include Float
  let zero = 0.
  let of_in = id
  let of_mm t = t /. 25.4
  let of_pt t = of_mm (t *. 0.34)
  let to_mm t = t *. 25.4
  let ( *.. ) = ( *. )
end

type weight = Bold | Regular

type font_size = FS of Length.t

type env = { l : Length.t;
             h : Length.t;
             margin : Length.t;
             origin : Length.t * Length.t;
             lwidth1 : Length.t;
             lwidth2 : Length.t;
             weight : weight;
             font_size : font_size;
             lab : Int.t;
             checkpoints : String.t List.t }
