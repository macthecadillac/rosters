open Containers
open Fun
open Fun.Infix

open Common

module Reader : sig
  type ('r, 'a) t
  val reader : ('r -> ('a * 'r)) -> ('r, 'a) t
  val run : ('r, 'a) t -> 'r -> 'a
  val ask : ('r, 'r) t
  val asks : ('r -> 'b) -> ('r, 'b) t
  val fmap : ('a -> 'b) -> ('r, 'a) t -> ('r, 'b) t
  val pure : 'a -> ('r, 'a) t
  val join : ('r, ('r, 'a) t) t -> ('r, 'a) t
  val ( <$> ) : ('a -> 'b) -> ('r, 'a) t -> ('r, 'b) t
  val ( <*> ) : ('r, 'a -> 'b) t -> ('r, 'a) t -> ('r, 'b) t
  val ( >>= ) : ('r, 'a) t -> ('a -> ('r, 'b) t) -> ('r, 'b) t
  val ( let* ) : ('r, 'a) t -> ('a -> ('r, 'b) t) -> ('r, 'b) t
  val ( let+ ) : ('r, 'a) t -> ('a -> 'b) -> ('r, 'b) t
  val sequence_l : ('r, 'a) t List.t -> ('r, 'a List.t) t
end = struct
  type ('r, 'a) t = 'r -> ('a * 'r)
  let reader a = a
  let run f r = fst (f r)
  let ask r = r, r
  let asks f r = f r, r
  let fmap f fa r = let a = (run fa) r in f a, r
  let ( <$> ) = fmap
  let ( let+ ) a f = fmap f a
  let ( <*> ) fa fb r = let f = (run fa) r in let b = (run fb) r in f b, r
  let pure a r = a, r
  let ( >>= ) fa f r = let fb = (run fa) r in f fb r
  let ( let* ) = ( >>= )
  let join a r = run a r r
  let sequence_l l = List.fold_left (fun acc x -> List.cons <$> x <*> acc) (pure []) l
    |> fmap List.rev
end

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

let resolve_otf = function
  | Bold -> Font [%blob "fonts/Carlito-Bold.otf"]
  | Regular -> Font [%blob "fonts/Carlito-Regular.otf"]

let otf_name = function Bold -> "cb" | Regular -> "cr"

let l = Length.of_in 8.5
let h = Length.of_in 11.0

let margin = Length.of_in 0.85  (* in *)

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

open Reader

let text_length text =
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
  let+ font, FS font_size = ask in
  let Font f = resolve_otf font in
  let decoder = Otfm.decoder (`String f) in
  let head = Otfm.head decoder |> Result.get_exn in
  let multiple = Float.of_int (head.head_ymax - head.head_ymin) |> flip (/.) 2048. in
  Length.(multiple *.. font_size)

let ymin =
  let+ font, FS font_size = ask in
  let Font f = resolve_otf font in
  let decoder = Otfm.decoder (`String f) in
  let head = Otfm.head decoder |> Result.get_exn in
  Length.(Float.of_int head.head_ymin /. 2048. *.. font_size)

let coord_shift vec = Gg.V2.(v (x vec) (Length.to_mm h -. y vec))

let auto_box_width text =
  (* give 6pts of padding on both sides of the text. 1pt = 0.34mm *)
  Length.((+) (of_pt (2. *. 6.))) <$> text_length text

let auto_box_height =
  (* give 2.5pts of padding on above and below text *)
  Length.((+) (of_pt (2. *. 2.5))) <$> text_height

(* anchors at upper left corner *)
let text_box text anchor width (`H box_height) border_width placement =
  let black = Vg.I.const Gg.Color.black in
  let anchor = Gg.V2.(coord_shift anchor - v2 Length.zero box_height) in
  let* font, font_size = ask in
  let* text_path =
    let name = otf_name font in
    let slant = `Normal in
    let weight = `W400 in
    let FS size_ = font_size in
    let glyphs = string_to_glyphs text in
    (* compensate for glyphs that go under the base line *)
    let* upshift =
      let* ym = ymin in
      let+ th = text_height in
      Length.(0.5 *.. (box_height - th) - ym) in
    let+ left_padding =
      let+ tl = text_length text in
      match width, placement with
      | _, `L | `A, _ -> Length.of_pt 6.  (* 6pts of padding to the left *)
      | `M w, `C -> Length.(0.5 *.. (w - tl)) in
    let size = Length.to_mm size_ in
    let font = Vg.Font.{ name; slant; weight; size } in
    Vg.I.cut_glyphs ~text font glyphs black
      |> Vg.I.move Gg.V2.(anchor + v2 left_padding upshift) in
  let+ box_path =
    let bwidth, color = match border_width with
      | Some w -> w, Vg.I.const Gg.Color.black
      | None -> Length.zero, Vg.I.const Gg.Color.white in
    let+ box_width = match width with
      | `A -> auto_box_width text
      | `M w -> pure w in
    let size = v2 box_width box_height in
    let path = Vg.P.empty |> Vg.P.rect (Gg.Box2.v anchor size) in
    let area = `O { Vg.P.o with Vg.P.width = Length.to_mm bwidth } in
    Vg.I.cut ~area path color in
  Vg.I.blend text_path box_path

let write_column_header chkpts y img =
  let* box_height = auto_box_height in
  let* checkpoints =
    let chkpt_w x = (fun x -> `M x) % Length.max box_height <$> (auto_box_width x) in
    List.map (fun x -> Pair.make <$> chkpt_w x <*> pure x) chkpts |> sequence_l in
  let left_headings = [`M (Length.of_mm 22.), "Signature"; `A, "Late"; `A, "Group"] in
  let right_headings = checkpoints @ [`A, "TA Check"]in
  let width = function
    | `A, s -> auto_box_width s
    | `M w, _ -> pure w in
  let accum_m f =
    let g acc x = Length.(+) <$> acc <*> x in
    List.fold_left g (pure Length.zero) % List.map width in
  let sum_width = accum_m Length.(+) in
  let* lwidth = sum_width left_headings in
  let* rwidth = sum_width right_headings in
  let mid_width = Length.((l - 2. *.. margin) - lwidth - rwidth) in
  let* widths = List.map width left_headings @ [pure mid_width] @ List.map width right_headings
    |> sequence_l in
  let row_height = `H box_height in
  let f (m, s) (x, img) =
    let anchor = v2 x y in
    let* w = width (m, s) in
    let+ tb = text_box s anchor m row_height (Some bw1) `C in
    Length.(x + w), Vg.I.blend tb img in
  let center_box (x, img) =
    let anchor = v2 x y in
    let width = `M mid_width in
    let+ tb = text_box "Student" anchor width row_height (Some bw1) `C in
    Length.(x + mid_width), Vg.I.blend tb img in
  let* img =
    let width = `M Length.(l - 2. *.. margin) in
    let anchor = v2 margin y in
    flip Vg.I.blend img <$> text_box "" anchor width row_height (Some bw2) `C in
  let+ _, img' =
    let g acc x = acc >>= f x in
    List.fold_left g (pure (margin, img)) left_headings
      >>= center_box
      |> flip (List.fold_left g) right_headings in
  Length.(y + box_height), widths, img'

let write_group n names y widths img =
  let write_row nlines name y widths img =
    let nchkpts = List.length widths - 4 in
    let row = [Some (1, Regular, `C), "";
               Some (1, Regular, `C), "";
               Option.map (fun x -> x, Bold, `C) nlines, string_of_int n;
               Some (1, Regular, `L), name]
      @ List.replicate nchkpts (Some (1, Regular, `C), "") in
    let row = List.combine row @@ widths in
    let* box_height = auto_box_height in
    let f ((cp, s), (w : Length.t)) (x, img) =
      let anchor = v2 x y in
      match cp with
      | None -> pure (Length.(x + w), img)
      | Some (vlines, f, p) ->
          let bh = `H Length.(box_height * of_int vlines) in
          let+ fs = asks snd in
          let tb = run (text_box s anchor (`M w) bh (Some bw1) p) (f, fs) in
          Length.(x + w), Vg.I.blend tb img in
    let g acc x = acc >>= f x in
    let+ _, img' = List.fold_left g (pure (margin, img)) row in
    Length.(y + box_height), widths, img' in
  match names with
  | [] -> pure (y, widths, img)
  | [name] -> write_row (Some 1) name y widths img
  | hd :: tl ->
      let f acc x = let* y, ws, m = acc in write_row None x y ws m in
      let res = write_row (Some (List.length tl + 1)) hd y widths img in
      List.fold_left f res tl

let write_page_header lab section img =
  let cells = [Length.of_in 1.7, `L, Format.sprintf "Lab %i" lab;
               Length.of_in 3.1, `C, Format.sprintf "Section %a" Section.pp section;
               Length.of_in 1.7, `L, "Date:"] in
  let* row_height = auto_box_height in
  let f (w, p, s) (x, img) =
    let anchor = v2 x margin in
    let+ tb = text_box s anchor (`M w) (`H row_height) None p in
    Length.(x + w), Vg.I.blend tb img in
  let g acc x = acc >>= f x in
  snd <$> List.fold_left g (pure (margin, img)) cells

let write_section lab section checkpoints font_size img roster =
  let page_header = write_page_header lab section img in
  let y = Length.((+) margin <$> auto_box_height) in
  let headers_r = join (write_column_header checkpoints <$> y <*> page_header) in
  let gs = Roster.groups roster
    |> IntMap.to_list
    |> List.sort (fun (i, _) (j, _) -> Int.compare i j)
    |> List.map (Pair.map_snd (List.map Name.canonical)) in
  let f acc (n, names) = let* x, y, z = acc in write_group n names x y z in
  let headers = run headers_r (Bold, font_size) in
  run (List.fold_left f (pure headers) gs) (Regular, font_size)

let write_page lab checkpoints roster =
  let white = Vg.I.const Gg.Color.white in
  let font_size = FS (Length.of_pt (11.)) in
  let section = Roster.section roster in
  let y, _, img = write_section lab section checkpoints font_size white roster in
  let width = `M Length.(l - 2. *.. margin) in
  let height = `H Length.(y - margin) in
  let borders_r = text_box "" (v2 margin margin) width height (Some bw2) `C in
  flip Vg.I.blend img @@ run borders_r (Bold, font_size)

exception ImpossibleBranch

let write document =
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
