module P : sig
  type record = (String.t * String.t) List.t
  val of_string : Int.t -> String.t -> (record List.t, String.t) Result.t
end = struct
  open Angstrom
  type record = (String.t * String.t) List.t

  let sepP = char ','

  let charP = satisfy (fun c -> Char.(c <> ',' && c <> '\n' && c <> '\r' && c <> '"'))

  let spacesP = many (char ' ')

  let oneOfCharP = choice % List.map char % String.to_list

  let ( ++ ) a b = let* s1 = a and+ s2 = b in return (s1 @ s2)

  let nonnewlineP = satisfy (fun c -> Char.(c <> '\n' && c <> '\r' && c <> '"'))

  let quoteP = char '"'

  let strP p = String.of_list <$> many p

  let cellP p = spacesP *> p <* spacesP

  let cellP' =
    let quotedCellP p = quoteP *> cellP p <* quoteP in
    quotedCellP (strP nonnewlineP) <|> cellP (strP charP)

  let rec rowP p =
    let* cell = p in
    let endCellP = return [cell] <* (end_of_line <|> end_of_input) in
    let midCellP = return cell <* sepP in
    endCellP <|> (List.cons <$> midCellP <*> rowP p)

  let headerP = rowP cellP'

  let csvP skip =
    let* header = headerP in
    let recordP =
      let+ row = rowP cellP' in
      List.combine_shortest header row in
    let skip_line = take_while (fun c -> Char.(c <> '\n')) <* end_of_line in
    count skip skip_line *> many recordP

  let of_string skip = parse_string ~consume:Consume.All (csvP skip)
end

