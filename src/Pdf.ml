open Containers
open Fun
open Fun.Infix

open Common

let font_regular = `Font [%blob "fonts/Carlito-Regular.otf"]
let font_bold = `Font [%blob "fonts/Carlito-Bold.otf"]

let resolve_otf = function `Bold -> font_bold | `Regular -> font_regular
let otf_name = function `Bold -> "cb" | `Regular -> "cr"

let l = 8.5 *. 25.4
let h = 11.0 *. 25.4

let canvas_size = Gg.Size2.v l h (* mm *)

let canvas_view = Gg.Box2.v Gg.P2.o canvas_size

let top_left = Gg.V2.v 0.0 h

module IntPair = struct
  type t = Int.t * Int.t
  let compare = CCPair.compare Int.compare Int.compare
end

module PairSet = Set.Make (IntPair)
module IntMap = Map.Make (Int)

let string_to_glyphs =
  Utf8_string.of_string_exn
    %> Utf8_string.to_list
      (* not sure why we need to shift by 29 to get the correct output but here
         we go *)
    %> List.map (Uchar.to_int %> flip (-) 29)

let text_length font text =
  let glyph_count = string_to_glyphs text |> List.map (flip Pair.make 1)
    |> IntMap.add_list_with ~f:(fun _ a b -> a + b) IntMap.empty in
  let `Font f = resolve_otf font in
  let decoder = Otfm.decoder (`String f) in
  let f acc gid adv _ = acc + IntMap.get_or gid glyph_count ~default:0 * adv in
  (* 2048 is the font design unit per em (UPM) value of most fonts *)
  Otfm.hmtx decoder f 0 |> Result.get_exn |> Float.of_int |> flip ( /. ) 2048.

let text_height font =
  let `Font f = resolve_otf font in
  let decoder = Otfm.decoder (`String f) in
  let head = Otfm.head decoder |> Result.get_exn in
  Float.of_int (head.head_ymax - head.head_ymin) /. 2048.

let ymin font =
  let `Font f = resolve_otf font in
  let decoder = Otfm.decoder (`String f) in
  let head = Otfm.head decoder |> Result.get_exn in
  Float.of_int head.head_ymin /. 2048.

let coord_shift vec = Gg.V2.(v (x vec) (h -. y vec))

let auto_box_width font font_size text =
  let tl = text_length font text *. font_size in
  (* give 6pts of padding on both sides of the text. 1pt = 0.34mm *)
  let pad = 2. *. 6. *. 0.34 in
  tl +. pad

let auto_box_height font font_size =
  let th = text_height font *. font_size in
  (* give 3pts of padding on above and below text. 1pt = 0.34mm *)
  let pad = 2. *. 2.5 *. 0.34 in
  th +. pad

(* anchors at upper left corner *)
let text_box text font font_size anchor width box_height border_width placement =
  let black = Vg.I.const Gg.Color.black in
  (* let box_height = auto_box_height font font_size in *)
  let anchor = Gg.V2.(coord_shift anchor - v 0. box_height) in
  let text_path =
    let name = otf_name font in
    let slant = `Normal in
    let weight = `W400 in
    let size = font_size in
    let glyphs = string_to_glyphs text in
    (* compensate for glyphs that go under the base line *)
    let upshift = 2. *. ymin font +. 0.5 *. (box_height -. text_height font) in
    (* 6pts of padding to the left. 1pt = 0.34mm *)
    let left_padding = match width, placement with
      | _, `L | `A, _ -> 6. *. 0.34
      | `M w, `C -> 0.5 *. (w -. text_length font text *. font_size) in
    let font = Vg.Font.{ name; slant; weight; size } in
    Vg.I.cut_glyphs ~text font glyphs black
      |> Vg.I.move Gg.V2.(anchor + Gg.V2.v left_padding upshift) in
  let box_path =
    let bwidth, color = match border_width with
      | Some w -> w, Vg.I.const Gg.Color.black
      | None -> 0., Vg.I.const Gg.Color.white in
    let box_width = match width with
      | `A -> auto_box_width font font_size text
      | `M w -> w in
    let size = Gg.Size2.v box_width box_height in
    let path = Vg.P.empty |> Vg.P.rect (Gg.Box2.v anchor size) in
    let area = `O { Vg.P.o with Vg.P.width = bwidth } in
    Vg.I.cut ~area path color in
  Vg.I.blend text_path box_path

let write_column_header checkpoints font font_size anchor_y img =
  let checkpoints = List.map (fun x -> `M (Float.max 7. (auto_box_width font font_size x)), x) checkpoints in
  let left_headings = [`M 22., "Signature"; `A, "Late"; `A, "Group"] in
  let right_headings = (`A, "TA Check") :: checkpoints in
  let width = function
    | `A, s -> auto_box_width font font_size s
    | `M w, _ -> w in
  let sum_width = List.(map width %> fold_left (+.) 0.) in
  let lwidth = sum_width left_headings in
  let rwidth = sum_width right_headings in
  let mid_width = (8.5 -. 0.85 *. 2.) *. 25.4 -. lwidth -. rwidth in
  let widths = List.map width left_headings @ [mid_width] @ List.map width right_headings in
  let row_height = auto_box_height `Bold font_size in
  let f (m, s) (anchor_x, img) =
    let anchor = Gg.V2.v anchor_x anchor_y in
    let tb = text_box s font font_size anchor m row_height (Some 0.3) `C in
    anchor_x +. width (m, s), Vg.I.blend tb img in
  let center_box (anchor_x, img) =
    let anchor = Gg.V2.v anchor_x anchor_y in
    let tb = text_box "Student" font font_size anchor (`M mid_width) row_height (Some 0.3) `C in
    anchor_x +. mid_width, Vg.I.blend tb img in
  let img = text_box "" `Bold font_size (Gg.V2.v (0.85 *. 25.4) anchor_y) (`M (25.4 *. (8.5 -. 0.85 *. 2.))) row_height (Some 0.6) `C
    |> flip Vg.I.blend img
  in
  let _, img' = List.fold_left (fun acc x -> f x acc) (0.85 *. 25.4, img) left_headings
      |> center_box
      |> flip (List.fold_left (fun acc x -> f x acc)) right_headings in
  anchor_y +. (auto_box_height font font_size), (widths, img')

let write_group font font_size n names anchor_y (widths, img) =
  let write_row nlines name (anchor_y, widths, img) =
    let nchkpts = List.length widths - 4 in
    let row = [Some (1, `Regular, `C), "";
               Some (1, `Regular, `C), "";
               Option.map (fun x -> x, `Bold, `C) nlines, string_of_int n;
               Some (1, `Regular, `L), name]
      @ List.replicate nchkpts (Some (1, `Regular, `C), "") in
    let row = List.combine row @@ widths in
    let f ((cp, s), w) (anchor_x, img) =
      let anchor = Gg.V2.v anchor_x anchor_y in
      match cp with
      | None -> anchor_x +. w, img
      | Some (vlines, font, p) ->
        let box_height = auto_box_height `Regular font_size *. Float.of_int vlines in
        let tb = text_box s font font_size anchor (`M w) box_height (Some 0.3) p in
        anchor_x +. w, Vg.I.blend tb img in
    let _, img' = List.fold_left (fun acc x -> f x acc) (0.85 *. 25.4, img) row in
    anchor_y +. (auto_box_height font font_size), widths, img' in
  match names with
  | [] -> anchor_y, widths, img
  | [name] -> write_row (Some 1) name (anchor_y, widths, img)
  | hd :: tl ->
      let res = write_row (Some (List.length tl + 1)) hd (anchor_y, widths, img) in
      List.fold_left (fun acc x -> write_row None x acc) res tl

let write_page_header lab section font_size img =
  let cells = [1.7 *. 25.4, `L, Format.sprintf "Lab %i" lab;
               3.1 *. 25.4, `C, Format.sprintf "Section %a" Section.pp section;
               1.7 *. 25.4, `L, "Date:"] in
  let row_height = auto_box_height `Bold font_size in
  let f (w, p, s) (anchor_x, img) =
    let anchor = Gg.V2.v anchor_x (0.85 *. 25.4) in
    let tb = text_box s `Bold font_size anchor (`M w) row_height None p in
    anchor_x +. w, Vg.I.blend tb img in
  snd @@ List.fold_left (fun acc x -> f x acc) (0.85 *. 25.4, img) cells

let write_section lab section checkpoints font_size img roster =
  let page_header = write_page_header lab section font_size img in
  let anchor_y = 0.85 *. 25.4 +. auto_box_height `Regular font_size in
  let column_header = write_column_header checkpoints `Bold font_size anchor_y page_header in
  let groups = Roster.groups roster
    |> IntMap.to_list
    |> List.sort (fun (i, _) (j, _) -> Int.compare i j)
    |> List.map (Pair.map_snd (List.map Name.canonical)) in
  List.fold_left (fun (x, y) (n, names) ->
    let a, b, c = write_group `Regular font_size n names x y
    in a, (b, c)) column_header groups

let page roster =
  let white = Vg.I.const Gg.Color.white in
  (* 11 point em size in mm *)
  let font_size = 11. *. 0.34 in
  let lab = 1 in
  let section = Section.of_int 5 in
  let checkpoints = ["A6"; "B11"; "C"; "D2"] in
  let anchor_y, (_, img) = write_section lab section checkpoints font_size white roster in
  text_box "" `Bold font_size (Gg.V2.v (0.85 *. 25.4) (0.85 *. 25.4)) (`M (25.4 *. (8.5 -. 0.85 *. 2.))) (anchor_y -. (0.85 *. 25.4)) (Some 0.6) `C
    |> flip Vg.I.blend img

exception ImpossibleBranch

let write roster =
  let title = "Rosters" in
  let description = "Rosters" in
  let xmp = Vg.Vgr.xmp ~title ~description () in
  let warn w = Vg.Vgr.pp_warning Format.err_formatter w in
  let font = Result.get_exn % function
    | Vg.Font.{ name = "cr"; _ } -> let `Font f = font_regular in Vgr_pdf.otf_font f
    | Vg.Font.{ name = "cb"; _ } -> let `Font f = font_bold in Vgr_pdf.otf_font f
    | _ -> raise ImpossibleBranch in
  let buf = Buffer.create 0 in
  let r = Vg.Vgr.create ~warn (Vgr_pdf.target ~font ~xmp ()) (`Buffer buf) in
  ignore (Vg.Vgr.render r (`Image (canvas_size, canvas_view, page roster)));
  ignore (Vg.Vgr.render r `End);
  Buffer.contents buf
