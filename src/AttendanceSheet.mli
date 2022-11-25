type t = Xlsx.sheet List.t

val of_rosters : Int.t -> String.t List.t -> Roster.t List.t -> t
