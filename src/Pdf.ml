(* TODO: put bold boders around groups as well for better visibility *)
open Containers

open Common

module SR = Monad.StateReader
module R = Monad.Reader

open Fun
open Fun.Infix

module Length : sig
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
  val ( *.. ) : Float.t -> t -> t
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
  let ( *.. ) a b = a *. b
  let max a b = Float.max a b
end

type page = Vg.image

type font_type = Bold | Regular
type font_size = FS of Length.t
type font = Font of String.t
type placement = C | L
type box_height = H of Length.t
type box_width = A | M of Length.t

let resolve_otf = function
  | Bold -> Font [%blob "fonts/Carlito-Bold.otf"]
  | Regular -> Font [%blob "fonts/Carlito-Regular.otf"]

let otf_name = function Bold -> "cb" | Regular -> "cr"

let l = Length.of_in 8.5
let h = Length.of_in 11.0

let margin = Length.of_in 0.85

let origin = margin, margin

let bw1 = Length.of_mm 0.3
let bw2 = Length.of_mm 0.7

let v2 a b = Gg.V2.v (Length.to_mm a) (Length.to_mm b)

let canvas_size = v2 l h

let canvas_view = Gg.Box2.v Gg.P2.o canvas_size

let top_left = v2 Length.zero h

module IntMap = Map.Make (Int)

let string_to_glyphs =
  Utf8_string.of_string_exn
    %> Utf8_string.to_list
      (* not sure why we need to shift by 29 to get the correct output but here
         we go *)
    %> List.map (Uchar.to_int %> flip (-) 29)

let map_fst, map_snd = Pair.(map_fst, map_snd)

let text_length text =
  let open R in
  let glyph_count = string_to_glyphs text |> List.map (flip Pair.make 1)
    |> IntMap.add_list_with ~f:(fun _ a b -> a + b) IntMap.empty in
  let+ font, FS font_size = ask in
  let Font f = resolve_otf font in
  let decoder = Otfm.decoder (`String f) in
  let f acc gid adv _ = acc + IntMap.get_or gid glyph_count ~default:0 * adv in
  (* 2048 is the font design unit per em (UPM) value of most fonts *)
  let multiple = Otfm.hmtx decoder f 0
    |> Result.get_exn
    |> Float.of_int
    |> flip (/.) 2048. in
  Length.(multiple *.. font_size)

let text_height =
  let open R in
  let+ font, FS font_size = ask in
  let Font f = resolve_otf font in
  let decoder = Otfm.decoder (`String f) in
  let head = Otfm.head decoder |> Result.get_exn in
  let multiple = Float.of_int (head.head_ymax - head.head_ymin) |> flip (/.) 2048. in
  Length.(multiple *.. font_size)

let ymin =
  let open R in
  let+ font, FS font_size = ask in
  let Font f = resolve_otf font in
  let decoder = Otfm.decoder (`String f) in
  let head = Otfm.head decoder |> Result.get_exn in
  Length.(Float.of_int head.head_ymin /. 2048. *.. font_size)

let coord_shift vec = Gg.V2.(v (x vec) (Length.to_mm h -. y vec))

let auto_box_width text =
  (* give 6pts of padding on both sides of the text. 1pt = 0.34mm *)
  R.(Length.((+) (of_pt (2. *. 6.))) <$> text_length text)

let auto_box_height =
  (* give 2.5pts of padding on above and below text *)
  R.(Length.((+) (of_pt (2. *. 2.5))) <$> text_height)

(* anchors at upper left corner *)
let text_box text placement width (H box_height) border_width =
  let open SR in
  let black = Vg.I.const Gg.Color.black in
  let* ax, ay = get in
  let anchor = Gg.V2.(coord_shift (v2 ax ay) - v2 Length.zero box_height) in
  let* font, font_size = ask in
  let* text_path =
    let name = otf_name font in
    let slant = `Normal in
    let weight = `W400 in
    let FS size_ = font_size in
    let glyphs = string_to_glyphs text in
    (* compensate for glyphs that go under the base line *)
    let* upshift =
      let* ym = lift_reader ymin in
      let+ th = lift_reader text_height in
      Length.(0.5 *.. (box_height - th) - ym) in
    let+ left_padding =
      let+ tl = lift_reader (text_length text) in
      match width, placement with
      | _, L | A, _ -> Length.of_pt 6.  (* 6pts of padding to the left *)
      | M w, C -> Length.(0.5 *.. (w - tl)) in
    let size = Length.to_mm size_ in
    let font = Vg.Font.{ name; slant; weight; size } in
    Vg.I.cut_glyphs ~text font glyphs black
      |> Vg.I.move Gg.V2.(anchor + v2 left_padding upshift) in
  let+ box_path =
    let bwidth, color = match border_width with
      | Some w -> w, Vg.I.const Gg.Color.black
      | None -> Length.zero, Vg.I.const Gg.Color.white in
    let+ box_width = match width with
      | A -> lift_reader (auto_box_width text)
      | M w -> pure w in
    let size = v2 box_width box_height in
    let path = Vg.P.empty |> Vg.P.rect (Gg.Box2.v anchor size) in
    let area = `O { Vg.P.o with Vg.P.width = Length.to_mm bwidth } in
    Vg.I.cut ~area path color in
  Vg.I.blend text_path box_path

let write_column_header chkpts img =
  let open SR in
  let* x, y = get in
  let* box_height = lift_reader auto_box_height in
  let* checkpoints =
    let chkpt_w x =
      let box_width = lift_reader (auto_box_width x) in
      (fun x -> M x) % Length.max box_height <$> box_width in
    List.map (fun x -> Pair.make <$> chkpt_w x <*> pure x) chkpts |> sequence_l in
  let left_headings = [M (Length.of_mm 22.), "Signature"; A, "Late"; A, "Group"] in
  let right_headings = checkpoints @ [A, "TA Check"]in
  let width = function
    | A, s -> lift_reader (auto_box_width s)
    | M w, _ -> pure w in
  let accum_m f =
    let g acc x = Length.(+) <$> acc <*> x in
    List.fold_left g (pure Length.zero) % List.map width in
  let sum_width = accum_m Length.(+) in
  let* lwidth = sum_width left_headings in
  let* rwidth = sum_width right_headings in
  let mid_width = Length.((l - 2. *.. margin) - lwidth - rwidth) in
  let* widths = sequence_l @@ List.map width left_headings
                            @ [pure mid_width]
                            @ List.map width right_headings in
  let row_height = H box_height in
  let f (m, s) img =
    let* w = width (m, s) in
    let* tb = text_box s C m row_height (Some bw1) in
    Vg.I.blend tb img <$ puts Length.(map_fst ((+) w)) in
  let center_box img =
    let width = M mid_width in
    let* tb = text_box "Student" C width row_height (Some bw1) in
    Vg.I.blend tb img <$ puts Length.(map_fst ((+) mid_width)) in
  let* img =
    let* img =
      let width = M Length.(l - 2. *.. margin) in
      flip Vg.I.blend img <$> text_box "" C width row_height (Some bw2) in
    let g acc x = acc >>= f x in
    List.fold_left g (pure img) left_headings >>= center_box
      |> flip (List.fold_left g) right_headings in
  (widths, img) <$ put Length.(x, y + box_height)

let write_group n names widths img =
  let open SR in
  let write_row nlines name img =
    let* x, y = get in
    let nchkpts = List.length widths - 4 in
    (* TODO: make string input optional *)
    let row = [Some (1, None, C, "");
               Some (1, None, C, "");
               Option.map (fun x -> x, Some Bold, C, string_of_int n) nlines;
               Some (1, None, L, name)]
      @ List.replicate nchkpts (Some (1, None, C, "")) in
    let row = List.combine row @@ widths in
    let* box_height = lift_reader auto_box_height in
    let f (x, w) img = match x with
      | None -> img <$ puts Length.(map_fst ((+) w))
      | Some (vlines, font, p, s) ->
          let bh = H Length.(box_height * of_int vlines) in
          let* tb =
            let gen = text_box s p (M w) bh (Some bw1) in
            match font with
            | None -> gen
            | Some ft -> local (map_fst @@ const ft) gen in
            Vg.I.blend tb img <$ puts Length.(map_fst @@ (+) w) in
    let g acc x = acc >>= f x in
    List.fold_left g (pure img) row <* put Length.(x, y + box_height) in
  match names with
  | [] -> pure img
  | [name] -> write_row (Some 1) name img
  | hd :: tl ->
      let f acc name = acc >>= write_row None name in
      let res = write_row (Some (List.length tl + 1)) hd img in
      List.fold_left f res tl

let write_page_header lab section img =
  let open SR in
  let cells = [Length.of_in 1.7, L, Format.sprintf "Lab %i" lab;
               Length.of_in 3.1, C, Format.sprintf "Section %a" Section.pp section;
               Length.of_in 1.7, L, "Date:"] in
  let* row_height = lift_reader auto_box_height in
  let* x, y = get in
  let f (w, p, s) img =
    let* tb = text_box s p (M w) (H row_height) None in
    Vg.I.blend tb img <$ puts Length.(map_fst ((+) w)) in
  let g acc x = acc >>= f x in
  List.fold_left g (pure img) cells <* put Length.(x, y + row_height)

let write_section lab section checkpoints font_size img roster =
  let page_header = write_page_header lab section img in
  let headers_r = SR.(join (write_column_header checkpoints <$> page_header)) in
  let gs = Roster.groups roster
    |> IntMap.to_list
    |> List.sort (fun (i, _) (j, _) -> Int.compare i j)
    |> List.map (map_snd (List.map Name.canonical)) in
  let anchor, (widths, headers) = R.run (SR.run headers_r origin) (Bold, font_size) in
  let f acc (n, names) = SR.(acc >>= write_group n names widths) in
  R.run SR.(run (List.fold_left f (pure headers) gs) anchor) (Regular, font_size)

let of_roster lab checkpoints roster =
  let white = Vg.I.const Gg.Color.white in
  let font_size = FS (Length.of_pt (11.)) in
  let section = Roster.section roster in
  let (_, y), img = write_section lab section checkpoints font_size white roster in
  let width = M Length.(l - 2. *.. margin) in
  let height = H Length.(y - margin) in
  let borders_r = text_box "" C width height (Some bw2) in
  flip Vg.I.blend img @@ R.run (SR.eval borders_r origin) (Bold, font_size)

exception ImpossibleBranch

let to_bytes document =
  let title = "1L Rosters" in
  let description = "1L Rosters" in
  let xmp = Vg.Vgr.xmp ~title ~description () in
  let warn w = Vg.Vgr.pp_warning Format.err_formatter w in
  let font = Result.get_exn % function
    | Vg.Font.{ name = "cr"; _ } -> let Font f = resolve_otf Regular in Vgr_pdf.otf_font f
    | Vg.Font.{ name = "cb"; _ } -> let Font f = resolve_otf Bold in Vgr_pdf.otf_font f
    | _ -> raise ImpossibleBranch in
  let buf = Buffer.create 0 in
  let r = Vg.Vgr.create ~warn (Vgr_pdf.target ~font ~xmp ()) (`Buffer buf) in
  let render_page page = ignore (Vg.Vgr.render r (`Image (canvas_size, canvas_view, page))) in
  List.iter render_page document;
  ignore (Vg.Vgr.render r `End);
  Buffer.contents buf
