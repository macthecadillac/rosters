type t

val empty : t

val ( <+> ) : t -> t -> t

val of_section : Int.t -> String.t List.t -> Rosters.t -> t

val to_string : t -> String.t
