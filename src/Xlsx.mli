open Common
type typography = Bold | Italic | Underline
type color = Red | Blue | Green | Black
module Content : sig
  type t = Text of String.t | Float of Float.t
  val to_string : t -> String.t Option.t
  val to_float : t -> Float.t Option.t
end
type cell
type sheet
val new_cell : typography Option.t -> color -> String.t -> Content.t -> cell
val new_sheet : String.t -> cell List.t List.t -> sheet
val empty_cell : cell
val set_color : color -> cell -> cell
val set_type : typography -> cell -> cell
val freeze_col : Int.t -> sheet -> sheet
val freeze_row : Int.t -> sheet -> sheet
val text_cell : String.t -> cell
val float_cell : Float.t -> cell
val name : sheet -> String.t
val data : sheet -> cell List.t List.t
val content : cell -> Content.t
val write : String.t -> sheet List.t -> (Unit.t, String.t) result
val read : String.t -> (sheet List.t, String.t) Result.t
