open TypeClasses
open Monads

module StateT = struct
  module type S = sig
    type ('s, 'a) t
    type 'a m
    include Functor.S2 with type ('s, 'a) t := ('s, 'a) t
    include Applicative.S2 with type ('s, 'a) t := ('s, 'a) t
    include Monad.S2 with type ('s, 'a) t := ('s, 'a) t
    val state : ('s -> ('a * 's) m) -> ('s, 'a) t
    val run : ('s, 'a) t -> 's -> ('a * 's) m
    val eval : ('s, 'a) t -> 's -> 'a m
    val exec : ('s, 'a) t -> 's -> 's m
    val get : ('s, 's) t
    val put : 's -> ('s, unit) t
  end

  module Make (M : Monad.S) : S
    with type 'a m = 'a M.t = struct
    module Base = struct
      type ('s, 'a) t = 's -> ('a * 's) M.t
      let fmap f s sa = M.(let* b, sb = s sa in return (f b, sb))
    end

    module StateTFunctor = struct
      type ('s, 'a) t = 's -> ('a * 's) M.t
      include Functor.Make2 (Base)

      let pure a s = M.return (a, s)
      let apply af a s =
        let open M in
        let* f, sa = af s in
        let* b, sb = a sa in
        return (f b, sb)
    end

    module StateTApplicative = struct
      include Applicative.Make2 (StateTFunctor)
      let bind m k s = M.(let* a, sa = m s in (k a) sa)
    end

    type 'a m = 'a M.t
    type ('s, 'a) t = 's -> ('a * 's) m
    include Monad.Make2 (StateTApplicative)

    let state f = f
    let run s = s
    let eval st s = M.(fst <$> run st s)
    let exec st s = M.(snd <$> run st s)
    let get s = M.return (s, s)
    let put s = fun _ -> M.return ((), s)
  end
end

module State = StateT.Make (Identity)

module ExceptT = struct
  module type S = sig
    type ('e, 'a) t
    type 'a m
    include Functor.S2 with type ('e, 'a) t := ('e, 'a) t
    include Applicative.S2 with type ('e, 'a) t := ('e, 'a) t
    include Monad.S2 with type ('e, 'a) t := ('e, 'a) t
    val run : ('e, 'a) t -> ('e, 'a) Result.t m
  end

  module Make (M : Monad.S) : S
  with type 'a m = 'a M.t = struct
    module Base = struct
      type ('e, 'a) t = ('e, 'a) Result.t M.t
      let fmap f ex = M.fmap (Result.fmap f) ex
    end

    module ExceptTFunctor = struct
      type ('e, 'a) t = ('e, 'a) Result.t M.t
      include Functor.Make2 (Base)

      let pure a = M.return @@ Result.return a
      let apply af a =
        let open M in
        let* f = af in
        let* b = a in
        return Result.(f <*> b)
    end

    module ExceptTApplicative = struct
      include Applicative.Make2 (ExceptTFunctor)
      let bind m f =
        let open M in
        let* a = m in
        match a with
        | Ok x -> f x
        | Error e -> return @@ Error e
    end

    type 'a m = 'a M.t
    type ('e, 'a) t = ('e, 'a) Result.t M.t
    include Monad.Make2 (ExceptTApplicative)

    let run a = a
  end
end

module Monoid = struct
  module type O = sig
    type 'a t
    val mempty : 'a t
    val mappend : 'a t -> 'a t -> 'a t
  end

  module type O2 = sig
    type ('a, 'b) t
    val mempty : ('a, 'b) t
    val mappend : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
    val mconcat : ('a, 'b) t list -> ('a, 'b) t
  end

  module type O3 = sig
    type ('a, 'b, 'c) t
    val mempty : ('a, 'b, 'c) t
    val mappend : ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
    val mconcat : ('a, 'b, 'c) t list -> ('a, 'b, 'c) t
  end

  module type S = sig
    include O
    val mconcat : 'a t list -> 'a t
    val (<+>) : 'a t -> 'a t -> 'a t
  end

  module type S2 = sig
    include O2
    val mconcat : ('a, 'b) t list -> ('a, 'b) t
    val (<+>) : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
  end

  module type S3 = sig
    include O3
    val mconcat : ('a, 'b, 'c) t list -> ('a, 'b, 'c) t
    val (<+>) : ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  end
end

module WriterT = struct
  module type S = sig
    type ('w, 'a) t
    type 'a w
    type 'a m
    include Monad.S2 with type ('w, 'a) t := ('w, 'a) t
    val run : ('w, 'a) t -> ('w w * 'a) m
    val exec : ('w, 'a) t -> 'w w m
  end

  module Make (M : Monad.S) (W : Monoid.S) : S
  with type 'a w = 'a W.t
   and type 'a m = 'a M.t = struct
    module Base = struct
      type 'a m = 'a M.t
      type ('w, 'a) t = ('w W.t * 'a) m
      let fmap f m = M.(let* w, a = m in return (w, f a))
    end

    module WriterTFunctor = struct
      include Base
      include Functor.Make2 (Base)
      let pure a = M.return (W.mempty, a)
      let apply w1 w2 =
        let open M in
        let* wa, f = w1 in
        let* wb, b = w2 in
        return W.(wa <+> wb, f b)
    end

    module WriterApplicative = struct
      include Base
      include Applicative.Make2 (WriterTFunctor)
      let bind w f =
        let open M in
        let* wa, a = w in
        let* wb, b = f a in
        return W.(wa <+> wb, b)
    end

    type 'a m = 'a M.t
    type 'a w = 'a W.t
    type ('w, 'a) t = ('w W.t * 'a) m
    include Monad.Make2 (WriterApplicative)
    let run w = w
    let exec w = M.fmap fst w
  end
end

module Writer = WriterT.Make (Identity)
