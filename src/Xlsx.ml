open Containers
open Fun.Infix

(* TODO: Cell should contain cell indices *)
type typography = Bold | Italic | Underline

type color = Red | Blue | Green | Black

module Content = struct
  type t = Text of String.t | Float of float | Formula of String.t | Empty

  let to_string = function Text s -> Some s | _ -> None

  let to_float = function Float f -> Some f | _ -> None

  let pp fmt = function
    | Text s | Formula s -> Format.fprintf fmt "%s" s
    | Float f -> Format.fprintf fmt "%f" f
    | Empty -> Format.fprintf fmt ""
end

type cell = { typography : typography Option.t;
              color : color;
              font : String.t;
              content : Content.t}

type sheet = { name : String.t;
               freeze_row : Int.t Option.t;
               freeze_col : Int.t Option.t;
               data : cell List.t List.t }

type workbook = { sheets : sheet List.t }

let cell_pp fmt { content; _ } = Format.fprintf fmt "%a" Content.pp content

external _write: String.t -> workbook -> (unit, String.t) Result.t = "write_xlsx"

let write path sheets = Result.map_err (fun s -> "xlsxwriter-rs: " ^ s)
  @@ _write (Fpath.to_string path) { sheets }

external _read: String.t -> (workbook, String.t) Result.t = "read_xlsx"

let read = Fpath.to_string %> _read
                           %> Result.map_err (fun s -> "xlsxwriter-rs: " ^ s)
                           %> Result.map (fun wb -> wb.sheets)

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

let num_rows { data; _ } = List.length data

let text_cell t = { empty_cell with content = Text t }

let formula_cell t = { empty_cell with content = Formula t }

let set_color color t = { t with color }

let set_type typography t = { t with typography = Some typography }

let name { name; _ } = name

let data { data; _ } = data

let content { content; _ } = content

let float_cell n = { empty_cell with content = Float n }
