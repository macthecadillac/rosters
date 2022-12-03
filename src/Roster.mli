open Containers
open Common

type t

val section : t -> Section.t

val groups : t -> Name.t List.t IntMap.t 

val of_data : Record.t list -> t Seq.t

val to_xlsx : Int.t -> String.t List.t -> t List.t -> Xlsx.sheet List.t
