open Containers
open Fun
open Fun.Infix
open Common

type t = { section : Int.t;
           groups : Name.t List.t IntMap.t }

let section t = t.section

let groups t = t.groups

let shuffle l =
  let open Random in
  let step gen (i, x) =
    let* m = gen in
    let+ j = int i in
    IntMap.add i (IntMap.find j m) m |> IntMap.add j x in
  match l with
  | [] -> []
  | hd::tl ->
      let gen = pure (IntMap.singleton 0 hd) in
      let numbered = List.mapi (fun i -> Pair.make (i + 1)) tl in
      run (List.fold_left step gen numbered)
        |> IntMap.to_list
        |> List.map (fun (_, x) -> x)

let new_roster section students =
  let n = List.length students in
  let shuffled = shuffle students in
  let ngroups = Int.min 6 @@ n / 5 + if n mod 5 = 0 then 0 else 1 in
  let inds = List.init ngroups ((+) 1) |> Seq.(of_list %> cycle %> take n %> to_list) in
  let groups = List.combine_shortest inds shuffled
    |> List.fold_left (fun acc (i, n) ->
        IntMap.add i (n :: IntMap.get_or ~default:[] i acc) acc) IntMap.empty
  in { section; groups }

let of_data =
  let open Data.Published in
  let open Option in
  Seq.of_list
  %> Seq.map (fun x -> x.section, [x.name])
  %> SectionMap.add_seq_with ~f:(fun _ a b -> a @ b) SectionMap.empty
  %> SectionMap.to_seq
  %> Seq.filter_map (fun (s, l) -> Section.to_int s >>= pure % flip new_roster l)
