type typography = Bold | Italic | Underline

type color = Red | Blue | Green | Black

module Content = struct
  type t = Text of String.t | Float of Float.t

  let to_string = function Text s -> Some s | _ -> None

  let to_float = function Float f -> Some f | _ -> None
end

type cell = { typography : typography Option.t;
                   color : color;
                   font : String.t;
                   content : Content.t}

type sheet = { name : String.t;
                    freeze_row : Int.t Option.t;
                    freeze_col : Int.t Option.t;
                    data : cell List.t List.t }

external write: String.t -> sheet List.t -> (unit, String.t) Result.t = "write_xlsx"

external read: String.t -> (sheet List.t, String.t) Result.t = "read_xlsx"

let empty_cell =
  let typography = None in
  let color = Black in
  let font = "Calibri" in
  let content = Content.Text "" in
  { typography; color; font; content }

let new_cell typography color font content = { typography; color; font; content }

let new_sheet name data = { name; freeze_row = None; freeze_col = None; data }

let freeze_row row t = { t with freeze_row = Some row }

let freeze_col col t = { t with freeze_col = Some col }

let text_cell t = { empty_cell with content = Text t }

let set_color color t = { t with color }

let set_type typography t = { t with typography = Some typography }

let name { name; _ } = name

let data { data; _ } = data

let content { content; _ } = content

let float_cell n = { empty_cell with content = Float n }
