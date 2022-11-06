open Containers
open Rosters
open Fun

module IntMap = Map.Make(Int)

type t = { lab : Int.t;
           checkpoints : String.t List.t;
           code : String.t List.t IntMap.t }

let header = ["\\documentclass[11pt]{article}";
              "\\usepackage[margin=0.85in]{geometry}";
              "\\usepackage{multirow}";
              "\\usepackage{tabularx}";
              "\\usepackage{makecell}";
              "\\usepackage[sfdefault; lf]{carlito}";
              "\\newcolumntype{?}{!{\\vrule width 2pt}}";
              "\\newcolumntype{C}{>{\\centering\\arraybackslash}X}";
              "\\setlength\\arrayrulewidth{1pt}";
              "\\renewcommand{\\arraystretch}{1.3}";
              "\\pagenumbering{gobble}";
              "\\begin{document}"]

let footer = ["\\end{document}"]

let empty = { lab = 0; checkpoints = []; code = IntMap.empty }

let of_code code lab checkpoints = { code; lab; checkpoints }

let add_group t section group = function
  | [] -> t
  | (first_student :: rest) as l ->
      let nrows = List.length l in
      let n = List.length t.checkpoints in
      let seps = String.repeat "& " (n - 1) in
      let section_code = IntMap.find section t.code in
      let first_row = Printf.sprintf "      & & \\multirow{{%i}}{{*}}{{\\textbf{{%i}}}} & %s & & %s\\\\" nrows group first_student seps in
      let rows = List.flat_map (fun student ->
        [Printf.sprintf "      \\cline{{1-2}}\\cline{{4-%i}}" (n + 5);
         Printf.sprintf "      & & & %s & & %s\\\\" student seps])
        rest in
      let end_ = ["      \\hline"] in
      let code = IntMap.add section (section_code @ first_row :: rows @ end_) t.code in
      { t with code }

let section_codegen t roster =
  let n = List.length t.checkpoints in
  let begin_ = [
      "\\begin{center}";
      "  \\begin{tabularx}{\\textwidth}{?>{\\hsize=5em}C|c|c|X|c|" ^
      (* adjust the table column setup based on the number of checkpoints *)
      (String.concat "|" @@ List.replicate n "c");
      (* "|".join(["c"] * n) + "?}"; *)
      "    \\Xhline{2pt}";
      (Printf.sprintf "    \\multicolumn{{3}}{{?l}}{{\\textbf{{Lab %i}}}} & " t.lab) ^
      (* (Printf.sprintf "\\multicolumn{{1}}{{C}}{{\\textbf{{Section %i}}}} " roster.section) ^ *)
      Printf.sprintf "& \\multicolumn{{%i}}{{l?}}{{\\textbf{{Date:}}}} \\\\" (n + 1);
      "    \\Xhline{2pt}";
      "    \\textbf{Signature} & \\textbf{Late} & \\textbf{Group} & " ^
      "\\multicolumn{1}{c|}{\\textbf{Student}} & \\textbf{TA Check} & " ^
      (* put in the checkpoints in the table column headings *)
      (* " & ".join("\\textbf{{{}}}".format(c).replace("&"; "\&") for c in self.checkpoints) + *)
      " \\\\";
      "    \\Xhline{2pt}"] in
  (* LaTeX code at the end of a table *)
  let end_ = [
      "    \\Xhline{2pt}";
      "  \\end{tabularx}";
      "\\end{center}";
      "\\newpage"]
  in ()

(* TODO: remove last \newpage *)
let run { code; _ } =
  IntMap.to_list code
    |> List.sort (fun (a, _) (b, _) -> Int.compare a b)
    |> List.map (fun (_, b) -> b)
    |> List.concat
    |> String.concat "\n"

let ( + ) l r =
  let code = IntMap.union (fun _ v1 _ -> Some v1) l.code r.code in
  let checkpoints = if List.is_empty l.checkpoints then r.checkpoints else l.checkpoints in
  let lab = if l.lab <> 0 then l.lab else l.lab in
  { lab; code; checkpoints }

(* let init roster lab checkpoints = { *)
