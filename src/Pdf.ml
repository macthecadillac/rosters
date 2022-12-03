open Containers
open Fun
open Fun.Infix

open Common

let font_regular = `Font [%blob "fonts/Carlito-Regular.otf"]
let font_bold = `Font [%blob "fonts/Carlito-Bold.otf"]

let resolve_otf = function `Bold -> font_bold | `Regular -> font_regular
let otf_name = function `Bold -> "cb" | `Regular -> "cr"

module U : sig
  type t
  val zero : t
  val of_in : Float.t -> t
  val of_mm : Float.t -> t
  val of_pt : Float.t -> t
  val of_float : Float.t -> t
  val of_int : Int.t -> t
  val to_in : t -> Float.t
  val to_mm : t -> Float.t
  val to_pt : t -> Float.t
  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val ( * ) : t -> t -> t
  val max : t -> t -> t
end = struct
  type t = Float.t
  let zero = 0.
  let of_in t = t
  let of_float t = t
  let of_int t = Float.of_int t
  let of_mm t = t /. 25.4
  let of_pt t = of_mm (t *. 0.34)
  let to_in t = t
  let to_mm t = t *. 25.4
  let to_pt t = to_mm t /. 0.34
  let ( + ) a b = a +. b
  let ( - ) a b = a -. b
  let ( * ) a b = a *. b
  let max a b = Float.max a b
end

let l = U.of_in 8.5
let h = U.of_in 11.0

let margin = U.of_in 0.85  (* in *)

let bw1 = U.of_mm 0.3
let bw2 = U.of_mm 0.6

let v2 a b = Gg.V2.v (U.to_mm a) (U.to_mm b)

let canvas_size = v2 l h

let canvas_view = Gg.Box2.v Gg.P2.o canvas_size

let top_left = v2 U.zero h

module IntMap = Map.Make (Int)

let string_to_glyphs =
  Utf8_string.of_string_exn
    %> Utf8_string.to_list
      (* not sure why we need to shift by 29 to get the correct output but here
         we go *)
    %> List.map (Uchar.to_int %> flip (-) 29)

let text_length font (`FS font_size) text =
  let glyph_count = string_to_glyphs text |> List.map (flip Pair.make 1)
    |> IntMap.add_list_with ~f:(fun _ a b -> a + b) IntMap.empty in
  let `Font f = resolve_otf font in
  let decoder = Otfm.decoder (`String f) in
  let f acc gid adv _ = acc + IntMap.get_or gid glyph_count ~default:0 * adv in
  (* 2048 is the font design unit per em (UPM) value of most fonts *)
  let multiple = Otfm.hmtx decoder f 0 |> Result.get_exn |> Float.of_int |> flip (/.) 2048. in
  U.(of_float multiple * font_size)

let text_height font (`FS font_size) =
  let `Font f = resolve_otf font in
  let decoder = Otfm.decoder (`String f) in
  let head = Otfm.head decoder |> Result.get_exn in
  let multiple = Float.of_int (head.head_ymax - head.head_ymin) |> flip (/.) 2048. in
  U.(of_float multiple * font_size)

let ymin font (`FS font_size) =
  let `Font f = resolve_otf font in
  let decoder = Otfm.decoder (`String f) in
  let head = Otfm.head decoder |> Result.get_exn in
  U.(of_float (Float.of_int head.head_ymin /. 2048.) * font_size)

let coord_shift vec = Gg.V2.(v (x vec) (U.to_mm h -. y vec))

let auto_box_width font font_size text =
  (* give 6pts of padding on both sides of the text. 1pt = 0.34mm *)
  U.(text_length font font_size text + of_pt (2. *. 6.))

let auto_box_height font font_size =
  (* give 2.5pts of padding on above and below text. 1pt = 0.34mm *)
  U.(text_height font font_size + of_pt (2. *. 2.5))

(* anchors at upper left corner *)
let text_box text font font_size anchor width (`H box_height) border_width placement =
  let black = Vg.I.const Gg.Color.black in
  let anchor = Gg.V2.(coord_shift anchor - v2 U.zero box_height) in
  let text_path =
    let name = otf_name font in
    let slant = `Normal in
    let weight = `W400 in
    let `FS size_ = font_size in
    let glyphs = string_to_glyphs text in
    (* compensate for glyphs that go under the base line *)
    let upshift =
      U.(of_float 0.5 * (box_height - text_height font font_size) - ymin font font_size) in
    (* 6pts of padding to the left. 1pt = 0.34mm *)
    let left_padding = match width, placement with
      | _, `L | `A, _ -> U.of_pt 6.
      | `M w, `C -> U.(of_float 0.5 * (w - text_length font font_size text)) in
    let size = U.to_mm size_ in
    let font = Vg.Font.{ name; slant; weight; size } in
    Vg.I.cut_glyphs ~text font glyphs black
      |> Vg.I.move Gg.V2.(anchor + v2 left_padding upshift) in
  let box_path =
    let bwidth, color = match border_width with
      | Some w -> w, Vg.I.const Gg.Color.black
      | None -> U.zero, Vg.I.const Gg.Color.white in
    let box_width = match width with
      | `A -> auto_box_width font font_size text
      | `M w -> w in
    let size = v2 box_width box_height in
    let path = Vg.P.empty |> Vg.P.rect (Gg.Box2.v anchor size) in
    let area = `O { Vg.P.o with Vg.P.width = U.to_mm bwidth } in
    Vg.I.cut ~area path color in
  Vg.I.blend text_path box_path

let write_column_header checkpoints font font_size y img =
  let chkpt_w x = `M U.(max (of_mm 7.) (auto_box_width font font_size x)) in
  let checkpoints = List.map (fun x -> chkpt_w x, x) checkpoints in
  let left_headings = [`M (U.of_mm 22.), "Signature"; `A, "Late"; `A, "Group"] in
  let right_headings = (`A, "TA Check") :: checkpoints in
  let width = function
    | `A, s -> auto_box_width font font_size s
    | `M w, _ -> w in
  let sum_width = List.(map width %> fold_left U.(+) U.zero) in
  let lwidth = sum_width left_headings in
  let rwidth = sum_width right_headings in
  let mid_width = U.((l - margin * (of_float 2.)) - lwidth - rwidth) in
  let widths = List.map width left_headings @ [mid_width] @ List.map width right_headings in
  let row_height = `H (auto_box_height `Bold font_size) in
  let f (m, s) (x, img) =
    let anchor = v2 x y in
    let tb = text_box s font font_size anchor m row_height (Some bw1) `C in
    U.(x + width (m, s)), Vg.I.blend tb img in
  let center_box (x, img) =
    let anchor = v2 x y in
    let tb = text_box "Student" font font_size anchor (`M mid_width) row_height (Some bw1) `C in
    U.(x + mid_width), Vg.I.blend tb img in
  let img = text_box "" `Bold font_size (v2 margin y) (`M U.(l - margin - margin)) row_height (Some bw2) `C
    |> flip Vg.I.blend img
  in
  let _, img' = List.fold_left (fun acc x -> f x acc) (margin, img) left_headings
      |> center_box
      |> flip (List.fold_left (fun acc x -> f x acc)) right_headings in
  U.(y + auto_box_height font font_size), (widths, img')

let write_group font (`FS fs as font_size) n names y (widths, img) =
  let write_row nlines name (y, widths, img) =
    let nchkpts = List.length widths - 4 in
    let row = [Some (1, `Regular, `C), "";
               Some (1, `Regular, `C), "";
               Option.map (fun x -> x, `Bold, `C) nlines, string_of_int n;
               Some (1, `Regular, `L), name]
      @ List.replicate nchkpts (Some (1, `Regular, `C), "") in
    let row = List.combine row @@ widths in
    let f ((cp, s), (w : U.t)) (x, img) =
      let anchor = v2 x y in
      match cp with
      | None -> U.(x + w), img
      | Some (vlines, font, p) ->
          let box_height = `H U.(auto_box_height `Regular font_size * of_int vlines) in
          let tb = text_box s font font_size anchor (`M w) box_height (Some bw1) p in
          U.(x + w), Vg.I.blend tb img in
    let _, img' = List.fold_left (fun acc x -> f x acc) (margin, img) row in
    U.(y + auto_box_height font font_size), widths, img' in
  match names with
  | [] -> y, widths, img
  | [name] -> write_row (Some 1) name (y, widths, img)
  | hd :: tl ->
      let res = write_row (Some (List.length tl + 1)) hd (y, widths, img) in
      List.fold_left (fun acc x -> write_row None x acc) res tl

let write_page_header lab section font_size img =
  let cells = [U.of_in 1.7, `L, Format.sprintf "Lab %i" lab;
               U.of_in 3.1, `C, Format.sprintf "Section %a" Section.pp section;
               U.of_in 1.7, `L, "Date:"] in
  let row_height = `H (auto_box_height `Bold font_size) in
  let f (w, p, s) (x, img) =
    let anchor = v2 x margin in
    let tb = text_box s `Bold font_size anchor (`M w) row_height None p in
    U.(x + w), Vg.I.blend tb img in
  snd @@ List.fold_left (fun acc x -> f x acc) (margin, img) cells

let write_section lab section checkpoints font_size img roster =
  let page_header = write_page_header lab section font_size img in
  let anchor_y = U.(margin + auto_box_height `Regular font_size) in
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
  let font_size = `FS (U.of_pt (11.)) in
  let lab = 1 in
  let section = Section.of_int 5 in
  let checkpoints = ["A6"; "B11"; "C"; "D2"] in
  let y, (_, img) = write_section lab section checkpoints font_size white roster in
  text_box "" `Bold font_size (v2 margin margin) (`M U.(l - margin - margin)) (`H U.(y - margin)) (Some bw2) `C
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
