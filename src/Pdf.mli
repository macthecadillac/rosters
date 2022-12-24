type page

val of_roster : Roster.t -> (Common.env, page) Monad.Reader.t

val to_bytes : page List.t -> (Common.env, String.t) Monad.Reader.t
