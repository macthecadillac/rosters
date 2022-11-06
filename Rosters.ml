open Containers
open Fun.Infix

module IntMap = Map.Make(Int)

type t = { section : Int.t;
           groups : Name.t List.t IntMap.t }

let shuffle l =
  let open Random in
  let step gen (i, x) =
    let* m = gen in
    let+ j = int i in
    IntMap.add i (IntMap.find j m) m |> IntMap.add j x in
  match l with
  | [] -> []
  | hd::tl ->
      run (List.fold_left step (pure (IntMap.singleton 0 hd)) (List.mapi Pair.make tl))
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
