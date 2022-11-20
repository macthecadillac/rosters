open Containers

type t

val section : t -> Int.t

val groups : t -> Common.Name.t List.t Common.IntMap.t 

val of_data : Data.Published.t List.t -> t Seq.t
