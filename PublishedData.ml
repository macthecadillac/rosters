open Containers
open Fun.Infix
module A = Angstrom

type cell =
  | Name of Name.t
  | Numeric of Float.t
  | String of String.t

let sepP = A.char ','
let charP = A.not_char ','
let spacesP = A.many (A.char ' ')
let numericCharP = A.choice @@ List.map A.char (String.to_list "0123456789")
let numericP = A.many numericCharP
let intP = A.(Int.of_string_exn % String.of_list <$> numericP)
(* use single '.' *)
let floatP = A.(Float.of_string_exn % String.of_list <$> A.many (numericCharP <|> A.char '.'))
let newlineP = A.char '\n'
let nonnewlineP = A.not_char '\n'
let quoteP = A.char '"'
let cellP =
  let open A in
  (quoteP *> many nonnewlineP <* quoteP) <|> (spacesP *> many charP <* spacesP)
let rowP = A.many cellP
let csvP = A.many rowP
