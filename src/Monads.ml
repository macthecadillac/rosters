open Containers

module Reader = struct
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
  let join m s = Reader.reader (fun r ->
    let s', m' = Reader.run (m s) r in
    let s'', a = Reader.run (m' s') r in
    ((s'', a), r))
  let ( <$> ) = fmap
  let ( $> ) a b = Fun.const b <$> a
  let ( <*> ) af a s = Reader.(
    let* sa, f = af s in
    let* sb, b = a sa in
    pure (sb, f b))
  let ( *> ) a b = Fun.const <$> b <*> a
  let ( >>= ) m k s = Reader.(let* sa, a = m s in (k a) sa)
  let ( let* ) = ( >>= )
  let ( let+ ) a f = f <$> a
  let sequence_l l = List.fold_left (fun acc x -> List.cons <$> x <*> acc) (pure []) l
    |> fmap List.rev
end
