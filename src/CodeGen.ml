open Containers
open Fun

open Common

type t = String.t List.t SectionMap.t

let header = ["\\documentclass[11pt]{article}";
              "\\usepackage[margin=0.85in]{geometry}";
              "\\usepackage{multirow}";
              "\\usepackage{tabularx}";
              "\\usepackage{makecell}";
              "\\usepackage[sfdefault, lf]{carlito}";
              "\\newcolumntype{?}{!{\\vrule width 2pt}}";
              "\\newcolumntype{C}{>{\\centering\\arraybackslash}X}";
              "\\setlength\\arrayrulewidth{1pt}";
              "\\renewcommand{\\arraystretch}{1.3}";
              "\\pagenumbering{gobble}";
              "\\begin{document}"]

let footer = ["\\end{document}"]

let empty = SectionMap.empty

let ( <+> ) = SectionMap.union (fun _ v1 _ -> Some v1)

let add_section_code pos section c =
  SectionMap.update section (match pos with
    | `Pre -> (function Some l -> Some (c @ l) | None -> Some c)
    | `Post -> (function Some l -> Some (l @ c) | None -> Some c))

let add_group t section checkpoints group students =
  match List.map Name.canonical students with
  | [] -> t
  | (first_student :: rest) as l ->
      let nrows = List.length l in
      let n = List.length checkpoints in
      let seps = String.repeat "& " (n - 1) in
      let first_row = Printf.sprintf "      & & \\multirow{%i}{*}{\\textbf{%i}} & %s & & %s\\\\"
                                     nrows group first_student seps in
      let rows = List.flat_map (fun student ->
        [Printf.sprintf "      \\cline{1-2}\\cline{4-%i}" (n + 5);
         Printf.sprintf "      & & & %s & & %s\\\\" student seps])
        rest in
      let end_ = ["      \\hline"] in
      add_section_code `Post section (first_row :: rows @ end_) t

let of_section lab checkpoints roster =
  let n = List.length checkpoints in
  let section = Roster.section roster in
  let begin_ = [
      "\\begin{center}";
      "  \\begin{tabularx}{\\textwidth}{?>{\\hsize=5em}C|c|c|X|c|" ^
      (* adjust the table column setup based on the number of checkpoints *)
      (String.concat "|" @@ List.replicate n "c") ^ "?}";
      "    \\Xhline{2pt}";
      (Printf.sprintf "    \\multicolumn{3}{?l}{\\textbf{Lab %i}} & " lab) ^
      (* (Printf.sprintf "\\multicolumn{1}{C}{\\textbf{Section %a}} " Section.pp section) ^ *)
      Printf.sprintf "& \\multicolumn{%i}{l?}{\\textbf{Date:}} \\\\" (n + 1);
      "    \\Xhline{2pt}";
      "    \\textbf{Signature} & \\textbf{Late} & \\textbf{Group} & " ^
      "\\multicolumn{1}{c|}{\\textbf{Student}} & \\textbf{TA Check} & " ^
      (* put in the checkpoints in the table column headings *)
      (checkpoints
      |> List.map (Printf.sprintf "\\textbf{%s}" %> String.replace ~which:`All ~sub:"&" ~by:"\\&")
      |> String.concat " & ");
      " \\\\";
      "    \\Xhline{2pt}"] in
  (* LaTeX code at the end of a table *)
  let end_ = [
      "    \\Xhline{2pt}";
      "  \\end{tabularx}";
      "\\end{center}";
      "\\newpage"] in
  let groups = Roster.groups roster in
  IntMap.fold (fun n l acc -> add_group acc section checkpoints n l) groups empty
  |> add_section_code `Pre section begin_
  |> add_section_code `Post section end_

(* TODO: remove last \newpage *)
let to_string =
  SectionMap.to_list
  %> List.sort (fun (a, _) (b, _) -> Section.compare a b)
  %> List.map (fun (_, b) -> b)
  %> List.concat
  %> (fun l -> header @ l @ footer)
  %> String.concat "\n"
