open Common

type t

val name : t -> Name.t

val section : t -> Section.t

val grades : t -> (String.t * Float.t Option.t) List.t

val compare : t -> t -> Int.t

val of_csv_string : String.t -> t List.t

val to_csv_string : t List.t -> String.t

val of_xlsx_sheets : Xlsx.sheet List.t -> t List.t Option.t

val to_xlsx_sheets : Section.t List.t StringMap.t -> t List.t -> Xlsx.sheet List.t

val update_grades : t List.t -> t List.t -> t List.t
