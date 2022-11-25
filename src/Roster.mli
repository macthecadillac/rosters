open Containers
open Common

type t

val section : t -> Section.t

val groups : t -> Name.t List.t IntMap.t 

val of_data : Record.t list -> t Seq.t
