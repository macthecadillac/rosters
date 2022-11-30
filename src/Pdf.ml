open Gg
open Vg

type ('a, 'b) t = 'a -> 'a * 'b

let l = 8.5 *. 25.4
let h = 11.0 *. 25.4

let canvas_size = Size2.v l h (* mm *)

let canvas_view = Box2.v P2.o canvas_size

let top_left = V2.v 0.0 h

let text_box anchor size line_width =
  let black = I.const Color.black in
  let text_path =
    let text = "Abcde" in
    let name = "Times" in
    let slant = `Normal in
    let weight = `W400 in
    let size = 5.5 in
    let font = Font.{ name; slant; weight; size } in
    let glyphs = CCUtf8_string.of_string_exn text
      |> CCUtf8_string.to_list
      |> List.map Uchar.to_int in
    I.cut_glyphs ~text font glyphs black |> I.move V2.(anchor + V2.v 0.0 1.5) in
  let box_path =
    let path = P.empty |> P.rect (Box2.v anchor size) in
    let area = `O { P.o with P.width = line_width } in
    I.cut ~area path black in
  I.blend text_path box_path

let page =
  let white = I.const Color.white in
  let anchor = P2.v 20.54 (h -. 30.54) in
  let size = Size2.v 20.0 7.0 in
  let fg = text_box anchor size 0.1 in
  I.blend fg white

(* let write () = () *)
let write () =
  let title = "Vgr_pdf minimal example" in
  let description = "Emerald Color" in
  let xmp = Vgr.xmp ~title ~description () in
  let warn w = Vgr.pp_warning Format.err_formatter w in
  let r = Vgr.create ~warn (Vgr_pdf.target ~xmp ()) (`Channel stdout) in
  ignore (Vgr.render r (`Image (canvas_size, canvas_view, page)));
  ignore (Vgr.render r `End)
