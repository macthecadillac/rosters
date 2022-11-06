type t

val compare : t -> t -> Int.t

val to_string : t -> String.t

val of_string : String.t -> (t, String.t) Result.t

val canonical : t -> String.t

val canonicalize : t -> t
