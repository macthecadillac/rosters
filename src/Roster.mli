open Common

type t

val section : t -> Section.t

val groups : t -> Name.t List.t IntMap.t 

val of_csv_string : String.t -> t List.t

val to_xlsx : t List.t -> (env, Xlsx.workbook) Monad.Reader.t
