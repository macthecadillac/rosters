(* Wrapper around libxlsxwriter. All functions are pure except for
   `write_workbook` where the side effects are executed. Every other function
   composes the side-effects of C code inside a LazyIO monad. *)
type workbook

type worksheet

type cell

type row = cell List.t

type typography = Normal | Bold | Italic

type color = Red | Black

val workbook_of_worksheets : worksheet List.t -> workbook

val write_workbook : Fpath.t -> workbook -> (unit, String.t) Result.t

val worksheet_of_rows : String.t -> row List.t -> worksheet

val text_cell : String.t -> cell

val empty_cell : cell

val set_color : color -> cell -> cell

val set_type : typography -> cell -> cell
