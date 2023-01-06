open Containers
open Ctypes
open Fun.Infix

module LibXlsxWriter : sig
  type workbook
  type worksheet
  type format
  val workbook_new : String.t -> workbook
  val workbook_close : workbook -> Int.t
  val workbook_add_worksheet : workbook -> String.t -> worksheet
  val workbook_add_format : workbook -> format
  val worksheet_write_string : worksheet -> Unsigned.UInt32.t -> Unsigned.UInt16.t -> String.t -> format -> Int.t
  val worksheet_set_column : worksheet -> Unsigned.UInt16.t -> Unsigned.UInt16.t -> Float.t -> format -> Int.t
  val format_set_font_name : format -> String.t -> unit
  val format_set_font_size : format -> Float.t -> unit
  val format_set_font_color : format -> Unsigned.UInt32.t -> unit
  val format_set_bold : format -> unit
  val format_set_italic : format -> unit
end = struct
  open Foreign
  type workbook = unit ptr
  type worksheet = unit ptr
  type format = unit ptr
  let workbook : workbook typ = ptr void
  let worksheet : worksheet typ = ptr void
  let format : format typ = ptr void
  let workbook_new = foreign "workbook_new" (string @-> returning workbook)
  let workbook_close = foreign "workbook_close" (workbook @-> returning int)
  let workbook_add_worksheet = foreign "workbook_add_worksheet"
    (workbook @-> string @-> returning worksheet)
  let workbook_add_format = foreign "workbook_add_format" (workbook @-> returning format)
  let worksheet_write_string = foreign "worksheet_write_string"
    (worksheet @-> uint32_t @-> uint16_t @-> string @-> format @-> returning int)
  let worksheet_set_column = foreign "worksheet_set_column"
    (worksheet @-> uint16_t @-> uint16_t @-> double @-> format @-> returning int)
  let format_set_font_name = foreign "format_set_font_name" (format @-> string @-> returning void)
  let format_set_font_size = foreign "format_set_font_size" (format @-> double @-> returning void)
  let format_set_font_color = foreign "format_set_font_color" (format @-> uint32_t @-> returning void)
  let format_set_bold = foreign "format_set_bold" (format @-> returning void)
  let format_set_italic = foreign "format_set_italic" (format @-> returning void)
end

type workbook = Fpath.t -> (LibXlsxWriter.workbook, String.t) Monad.LazyIOResult.t

type worksheet = Fpath.t -> workbook -> (LibXlsxWriter.worksheet, String.t) Monad.LazyIOResult.t

type cell = Unsigned.UInt32.t
         -> Unsigned.UInt16.t
         -> LibXlsxWriter.format
         -> Fpath.t
         -> workbook
         -> worksheet
         -> (unit, String.t) Monad.LazyIOResult.t

type row = cell List.t

type typography = Normal | Bold | Italic

type color = Red | Black

let exit_code_to_result = function
  | 0 -> Ok (())
  | e -> Error (Format.sprintf "libxlsxwriter error code: %i" e)

let workbook_of_worksheets (worksheets : worksheet list) : workbook =
  fun path ->
    let workbook = LibXlsxWriter.workbook_new @@ Fpath.to_string path in
    let a = List.(worksheets <*> [path] <*> [fun _ () -> Ok workbook]) in
    let open Monad.LazyIOResult in
    let+ _ = Monad.LazyIOResult.sequence_l a in
    workbook

let write_workbook path workbook =
  let open Result.Infix in
  let* wb = workbook path () in
  exit_code_to_result @@ LibXlsxWriter.workbook_close wb

let worksheet_of_rows (name : string) (data : row list) : worksheet =
  fun path workbook ->
    let open Monad.LazyIOResult in
    let* wb = workbook path in
    let ws = LibXlsxWriter.workbook_add_worksheet wb name in
    let default_format = LibXlsxWriter.workbook_add_format wb in
    let+ _ = sequence_l @@
      List.mapi (fun i l ->
        let row = Unsigned.UInt32.of_int i in
        sequence_l @@
        List.mapi (fun j cell ->
          let col = Unsigned.UInt16.of_int j in
          cell row col default_format path workbook (fun _ _ () -> Ok ws))
        l)
      data in
    ws

let text_cell (text : string) : cell =
  fun row col format path (workbook : workbook) (worksheet : worksheet) ->
    let open Monad.LazyIOResult in
    let* ws = worksheet path workbook in
    fun () ->
      LibXlsxWriter.worksheet_write_string ws row col text format
      |> exit_code_to_result

let empty_cell = text_cell ""

let set f cell row col format path workbook worksheet () = Ok (f format)

let set_color color =
  let c = Unsigned.UInt32.of_int @@ match color with
  | Red -> 16711680
  | Black -> 0 in
  set (Fun.flip LibXlsxWriter.format_set_font_color c)

let set_type = function
  | Normal -> Fun.id
  | Bold -> set LibXlsxWriter.format_set_bold
  | Italic -> set LibXlsxWriter.format_set_italic

(* module LibXlsxWriter (F : FOREIGN) *)
(* (1* : sig *1) *)
(* (1*   type t *1) *)
(* (1*   val workbook_new : String.t -> t *1) *)
(* (1*   val workbook_close : t -> Int.t *1) *)
(* (1* end *1) *)
(* = struct *)
(*   type t = unit ptr *)
(*   let t : t typ = ptr void *)
(*   let workbook_new = F.foreign "workbook_new" F.(string @-> returning t) *)
(*   let workbook_close = F.foreign "workbook_close" F.(t @-> returning int) *)
(* end *)

(* let cpp fmt () = Cstubs.write_c fmt ~prefix:"expat" (module LibXlsxWriter) *)
(* let c_src = Format.sprintf "%a" cpp () *)
(* let () = print_string c_src *)

(* let mlpp fmt () = Cstubs.write_ml fmt ~prefix:"expat" (module LibXlsxWriter) *)
(* let ml_src = Format.sprintf "%a" mlpp () *)
(* let () = print_string ml_src *)
