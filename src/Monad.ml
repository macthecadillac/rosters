open Containers

module Reader = struct
  type ('r, 'a) t = 'r -> ('a * 'r)
  let run f r = fst (f r)
  let ask r = r, r
  let asks f r = f r, r
  let local f a r = a (f r)
  let fmap f fa r = f (run fa r), r
  let ( <$> ) = fmap
  let ( let+ ) a f = fmap f a
  let ( <*> ) fa fb r = let f = (run fa) r in let b = (run fb) r in f b, r
  let pure a r = a, r
  let ( >>= ) fa f r = f (run fa r) r
  let ( let* ) = ( >>= )
  let sequence_l l = List.fold_left (fun acc x -> List.cons <$> x <*> acc) (pure []) l
    |> fmap List.rev
  let traverse_l f l = sequence_l @@ List.map f l
end

module StateReader = struct
  type ('s, 'r, 'a) t = 's -> ('r, 's * 'a) Reader.t
  let eval f s = Reader.fmap snd (f s)
  let run f s = f s
  let get s = Reader.pure (s, s)
  let gets f s = Reader.pure (s, f s)
  let put s _ = Reader.pure (s, ())
  let puts f s = Reader.pure (f s, ())
  let fmap f m sa = Reader.(let* sb, b = m sa in pure (sb, f b))
  let pure a s = Reader.pure (s, a)
  let lift_reader r s = Reader.(Pair.make s <$> r)
  let ask s = lift_reader Reader.ask s
  let asks f = lift_reader (Reader.asks f)
  let join m s r =
    let s', m' = Reader.run (m s) r in
    let s'', a = Reader.run (m' s') r in
    (s'', a), r
  let local f a s = Reader.local f (run a s)
  let ( <$> ) = fmap
  let ( <$ ) a b = Fun.const a <$> b
  let ( <*> ) af a s = Reader.(let* sa, f = af s in let+ sb, b = a sa in sb, f b)
  let ( <* ) a b = Fun.const <$> a <*> b
  let ( >>= ) m k s = Reader.(let* sa, a = m s in (k a) sa)
  let ( let* ) = ( >>= )
  let ( let+ ) a f = f <$> a
  let sequence_l l = List.fold_left (fun acc x -> List.cons <$> x <*> acc) (pure []) l
    |> fmap List.rev
  let traverse_l f l = sequence_l @@ List.map f l
end
