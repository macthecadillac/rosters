module Util : sig
  val const : 'a -> 'b -> 'a
  val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
  val id : 'a -> 'a
  val delay : (unit -> 'a -> 'b) -> 'a -> 'b
  val some : 'a -> 'a option
end = struct
  let const a _ = a
  let flip g b a = g a b
  let id x = x
  (* TODO: see if lazy works here *)
  let delay f cs = f () cs
  let some a = Some a
end

module Functor = struct
  module type O = sig
    type 'a t
    val fmap : ('a -> 'b) -> 'a t -> 'b t
  end

  module type O2 = sig
    type ('x, 'a) t
    val fmap : ('a -> 'b) -> ('x, 'a) t -> ('x, 'b) t
  end

  module type O3 = sig
    type ('x, 'y, 'a) t
    val fmap : ('a -> 'b) -> ('x, 'y, 'a) t -> ('x, 'y, 'b) t
  end

  module type S = sig
    include O
    val (<$>) : ('a -> 'b) -> 'a t -> 'b t
    val (<$ ) : 'a -> 'b t -> 'a t
    val ( $>) : 'a t -> 'b -> 'b t
    val (<&>) : 'a t -> ('a -> 'b) -> 'b t
    val void : 'a t -> unit t
  end

  module type S2 = sig
    include O2
    val (<$>) : ('a -> 'b) -> ('x, 'a) t -> ('x, 'b) t
    val (<$ ) : 'a -> ('x, 'b) t -> ('x, 'a) t
    val ( $>) : ('x, 'a) t -> 'b -> ('x, 'b) t
    val (<&>) : ('x, 'a) t -> ('a -> 'b) -> ('x, 'b) t
    val void : ('x, 'a) t -> ('x, unit) t
  end

  module type S3 = sig
    include O3
    val (<$>) : ('a -> 'b) -> ('x, 'y, 'a) t -> ('x, 'y, 'b) t
    val (<$ ) : 'a -> ('x, 'y, 'b) t -> ('x, 'y, 'a) t
    val ( $>) : ('x, 'y, 'a) t -> 'b -> ('x, 'y, 'b) t
    val (<&>) : ('x, 'y, 'a) t -> ('a -> 'b) -> ('x, 'y, 'b) t
    val void : ('x, 'y, 'a) t -> ('x, 'y, unit) t
  end

  module Make3 (M : O3) : S3
  with type ('x, 'y, 'a) t = ('x, 'y, 'a) M.t = struct
    open Util
    include M
    let (<$>) = fmap
    let (<$ ) a fb = const a <$> fb
    let ( $>) fa b = const b <$> fa
    let (<&>) a f = fmap f a
    let void a = const () <$> a
  end

  module Make2 (M : O2) : S2
  with type ('a, 'b) t := ('a, 'b) M.t = Make3 (struct
    type ('x, 'a, 'b) t = ('a, 'b) M.t
    include (M : O2 with type ('a, 'b) t := ('a, 'b) M.t)
  end)

  module Make (M : O) : S
  with type 'a t := 'a M.t = Make3 (struct
    type ('x, 'y, 'a) t = 'a M.t
    include (M : O with type 'a t := 'a M.t)
  end)
end

module Applicative = struct
  module type O = sig
    include Functor.S
    val pure : 'a -> 'a t
    val apply : ('a -> 'b) t -> 'a t -> 'b t
  end

  module type O2 = sig
    include Functor.S2
    val pure : 'a -> ('x, 'a) t
    val apply : ('x, 'a -> 'b) t -> ('x, 'a) t -> ('x, 'b) t
  end

  module type O3 = sig
    include Functor.S3
    val pure : 'a -> ('x, 'y, 'a) t
    val apply : ('x, 'y, 'a -> 'b) t -> ('x, 'y, 'a) t -> ('x, 'y, 'b) t
  end

  module type S = sig
    include O
    val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
    val ( *>) : 'a t -> 'b t -> 'b t
    val (<* ) : 'a t -> 'b t -> 'a t
    val (<**>) : 'a t -> ('a -> 'b) t -> 'b t
    val lifta : ('a -> 'b) -> 'a t -> 'b t
    val lifta2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    val lifta3 : ('a -> 'b -> 'c -> 'd) -> 'a t-> 'b t -> 'c t -> 'd t
    val conditional : bool -> unit t -> unit t
    (** [when] in Haskell *)

    val unless : bool -> unit t -> unit t
    val (let+) : ('a -> 'b) -> 'a t -> 'b t
    val (and+) : 'a t -> 'b t -> ('a * 'b) t
  end

  module type S2 = sig
    include O2
    val (<*>) : ('x, 'a -> 'b) t -> ('x, 'a) t -> ('x, 'b) t
    val ( *>) : ('x, 'a) t -> ('x, 'b) t -> ('x, 'b) t
    val (<* ) : ('x, 'a) t -> ('x, 'b) t -> ('x, 'a) t
    val (<**>) : ('x, 'a) t -> ('x, 'a -> 'b) t -> ('x, 'b) t
    val lifta : ('a -> 'b) -> ('x, 'a) t -> ('x, 'b) t
    val lifta2 : ('a -> 'b -> 'c) -> ('x, 'a) t -> ('x, 'b) t -> ('x, 'c) t
    val lifta3 : ('a -> 'b -> 'c -> 'd) -> ('x, 'a) t-> ('x, 'b) t -> ('x, 'c) t -> ('x, 'd) t
    (* `when` in Haskell *)
    val conditional : bool -> ('x, unit) t -> ('x, unit) t
    val unless : bool -> ('x, unit) t -> ('x, unit) t
    val (let+) : ('a -> 'b) -> ('x, 'a) t -> ('x, 'b) t
    val (and+) : ('x, 'a) t -> ('x, 'b) t -> ('x, 'a * 'b) t
  end

  module type S3 = sig
    include O3
    val (<*>) : ('x, 'y, 'a -> 'b) t -> ('x, 'y, 'a) t -> ('x, 'y, 'b) t
    val ( *>) : ('x, 'y, 'a) t -> ('x, 'y, 'b) t -> ('x, 'y, 'b) t
    val (<* ) : ('x, 'y, 'a) t -> ('x, 'y, 'b) t -> ('x, 'y, 'a) t
    val (<**>) : ('x, 'y, 'a) t -> ('x, 'y, 'a -> 'b) t -> ('x, 'y, 'b) t
    val lifta : ('a -> 'b) -> ('x, 'y, 'a) t -> ('x, 'y, 'b) t
    val lifta2 : ('a -> 'b -> 'c) -> ('x, 'y, 'a) t -> ('x, 'y, 'b) t -> ('x, 'y, 'c) t
    val lifta3 : ('a -> 'b -> 'c -> 'd) -> ('x, 'y, 'a) t-> ('x, 'y, 'b) t -> ('x, 'y, 'c) t -> ('x, 'y, 'd) t
    (* `when` in Haskell *)
    val conditional : bool -> ('x, 'y, unit) t -> ('x, 'y, unit) t
    val unless : bool -> ('x, 'y, unit) t -> ('x, 'y, unit) t
    val (let+) : ('a -> 'b) -> ('x, 'y, 'a) t -> ('x, 'y, 'b) t
    val (and+) : ('x, 'y, 'a) t -> ('x, 'y, 'b) t -> ('x, 'y, 'a * 'b) t
  end

  module Make2 (M : O2) : S2
  with type ('x, 'a) t = ('x, 'a) M.t = struct
    open Util
    include M
    let (<*>) = apply
    let ( *>) a b = const <$> b <*> a
    let (<* ) a b = const <$> a <*> b
    let (<**>) b af = af <*> b
    let lifta = fmap
    let lifta2 af b c = af <$> b <*> c
    let lifta3 af b c d = af <$> b <*> c <*> d
    let conditional c a = if c then a else pure ()
    let unless c a = if c then pure () else a
    let (let+) = (<$>)
    let (and+) a b = (fun x y -> x, y) <$> a <*> b
  end

  module Make (M : O) : S
  with type 'a t := 'a M.t = Make2 (struct
    type ('x, 'a) t = 'a M.t
    include (M : O with type 'a t := 'a M.t)
  end)
end

module Alternative = struct
  module type O = sig
    include Applicative.S
    val empty : 'a t
    val choose : 'a t -> 'a t -> 'a t
  end

  module type O2 = sig
    include Applicative.S2
    val empty : ('x, 'a) t
    val choose : ('x, 'a) t -> ('x, 'a) t -> ('x, 'a) t
  end

  module type O3 = sig
    include Applicative.S3
    val empty : ('x, 'y, 'a) t
    val choose : ('x, 'y, 'a) t -> ('x, 'y, 'a) t -> ('x, 'y, 'a) t
  end

  module type S = sig
    include O
    val (<|>) : 'a t -> 'a t -> 'a t
    (* val some : 'a t -> 'a list t *)
    (* val many : 'a t -> 'a list t *)
    val optional : 'a t -> 'a option t
    val guard : bool -> unit t
  end

  module type S2 = sig
    include O2
    val (<|>) : ('x, 'a) t -> ('x, 'a) t -> ('x, 'a) t
    (* val some : ('x, 'a) t -> ('x, 'a list) t *)
    (* val many : ('x, 'a) t -> ('x, 'a list) t *)
    val optional : ('x, 'a) t -> ('x, 'a option) t
    val guard : bool -> ('x, unit) t
  end

  module type S3 = sig
    include O3
    val (<|>) : ('x, 'y, 'a) t -> ('x, 'y, 'a) t -> ('x, 'y, 'a) t
    (* val some : ('x, 'y, 'a) t -> ('x, 'y, 'a list) t *)
    (* val many : ('x, 'y, 'a) t -> ('x, 'y, 'a list) t *)
    val optional : ('x, 'y, 'a) t -> ('x, 'y, 'a option) t
    val guard : bool -> ('x, 'y, unit) t
  end

  module Make3 (M : O3) : S3
  with type ('x, 'y, 'a) t = ('x, 'y, 'a) M.t = struct
    open Util
    include M
    let (<|>) = choose
    let optional v = some <$> v <|> pure None
    let guard b = if b then pure () else empty
  end

  module Make2 (M : O2) : S2
  with type ('a, 'b) t := ('a, 'b) M.t = Make3 (struct
    type ('x, 'a, 'b) t = ('a, 'b) M.t
    include (M : O2 with type ('a, 'b) t := ('a, 'b) M.t)
  end)

  module Make (M : O) : S
  with type 'a t := 'a M.t = Make2 (struct
    type ('x, 'a) t = 'a M.t
    include (M : O with type 'a t := 'a M.t)
  end)
end

module Monad = struct
  module type O = sig
    include Applicative.S
    val bind : 'a t -> ('a -> 'b t) -> 'b t
  end

  module type O2 = sig
    include Applicative.S2
    val bind : ('x, 'a) t -> ('a -> ('x, 'b) t) -> ('x, 'b) t
  end

  module type O3 = sig
    include Applicative.S3
    val bind : ('x, 'y, 'a) t -> ('a -> ('x, 'y, 'b) t) -> ('x, 'y, 'b) t
  end

  module type S = sig
    include O
    val return : 'a -> 'a t
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    val (=<<) : ('a -> 'b t) -> 'a t -> 'b t
    val (>>) : 'a t -> 'b t -> 'b t
    val (<<) : 'a t -> 'b t -> 'a t
    val (>=>) : ('a -> 'b t) -> ('b -> 'c t) -> 'a -> 'c t
    val (<=<) : ('b -> 'c t) -> ('a -> 'b t) -> 'a -> 'c t
    val forever : 'a t -> 'b t
    val join : 'a t t -> 'a t
    val liftm : ('a -> 'b) -> 'a t -> 'b t
    val liftm2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    val liftm3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
    val liftm4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t
    val liftm5 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t
    val ap : ('a -> 'b) t -> 'a t -> 'b t
    val (let*) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module type S2 = sig
    include O2
    val return : 'a -> ('x, 'a) t
    val (>>=) : ('x, 'a) t -> ('a -> ('x, 'b) t) -> ('x, 'b) t
    val (=<<) : ('a -> ('x, 'b) t) -> ('x, 'a) t -> ('x, 'b) t
    val (>>) : ('x, 'a) t -> ('x, 'b) t -> ('x, 'b) t
    val (<<) : ('x, 'a) t -> ('x, 'b) t -> ('x, 'a) t
    val (>=>) : ('a -> ('x, 'b) t) -> ('b -> ('x, 'c) t) -> 'a -> ('x, 'c) t
    val (<=<) : ('b -> ('x, 'c) t) -> ('a -> ('x, 'b) t) -> 'a -> ('x, 'c) t
    val forever : ('x, 'a) t -> ('x, 'b) t
    val join : ('x, ('x, 'a) t) t -> ('x, 'a) t
    val liftm : ('a -> 'b) -> ('x, 'a) t -> ('x, 'b) t
    val liftm2 : ('a -> 'b -> 'c) -> ('x, 'a) t -> ('x, 'b) t -> ('x, 'c) t
    val liftm3 : ('a -> 'b -> 'c -> 'd) -> ('x, 'a) t -> ('x, 'b) t -> ('x, 'c) t -> ('x, 'd) t
    val liftm4 : ('a -> 'b -> 'c -> 'd -> 'e) -> ('x, 'a) t -> ('x, 'b) t -> ('x, 'c) t -> ('x, 'd) t -> ('x, 'e) t
    val liftm5 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> ('x, 'a) t -> ('x, 'b) t -> ('x, 'c) t -> ('x, 'd) t -> ('x, 'e) t -> ('x, 'f) t
    val ap : ('x, ('a -> 'b)) t -> ('x, 'a) t -> ('x, 'b) t
    val (let*) : ('x, 'a) t -> ('a -> ('x, 'b) t) -> ('x, 'b) t
  end

  module type S3 = sig
    include O3
    val return : 'a -> ('x, 'y, 'a) t
    val (>>=) : ('x, 'y, 'a) t -> ('a -> ('x, 'y, 'b) t) -> ('x, 'y, 'b) t
    val (=<<) : ('a -> ('x, 'y, 'b) t) -> ('x, 'y, 'a) t -> ('x, 'y, 'b) t
    val (>>) : ('x, 'y, 'a) t -> ('x, 'y, 'b) t -> ('x, 'y, 'b) t
    val (<<) : ('x, 'y, 'a) t -> ('x, 'y, 'b) t -> ('x, 'y, 'a) t
    val (>=>) : ('a -> ('x, 'y, 'b) t) -> ('b -> ('x, 'y, 'c) t) -> 'a -> ('x, 'y, 'c) t
    val (<=<) : ('b -> ('x, 'y, 'c) t) -> ('a -> ('x, 'y, 'b) t) -> 'a -> ('x, 'y, 'c) t
    val forever : ('x, 'y, 'a) t -> ('x, 'y, 'b) t
    val join : ('x, 'y, ('x, 'y, 'a) t) t -> ('x, 'y, 'a) t
    val liftm : ('a -> 'b) -> ('x, 'y, 'a) t -> ('x, 'y, 'b) t
    val liftm2 : ('a -> 'b -> 'c) -> ('x, 'y, 'a) t -> ('x, 'y, 'b) t -> ('x, 'y, 'c) t
    val liftm3 : ('a -> 'b -> 'c -> 'd) -> ('x, 'y, 'a) t -> ('x, 'y, 'b) t -> ('x, 'y, 'c) t -> ('x, 'y, 'd) t
    val liftm4 : ('a -> 'b -> 'c -> 'd -> 'e) -> ('x, 'y, 'a) t -> ('x, 'y, 'b) t -> ('x, 'y, 'c) t -> ('x, 'y, 'd) t -> ('x, 'y, 'e) t
    val liftm5 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> ('x, 'y, 'a) t -> ('x, 'y, 'b) t -> ('x, 'y, 'c) t -> ('x, 'y, 'd) t -> ('x, 'y, 'e) t -> ('x, 'y, 'f) t
    val ap : ('x, 'y, ('a -> 'b)) t -> ('x, 'y, 'a) t -> ('x, 'y, 'b) t
    val (let*) : ('x, 'y, 'a) t -> ('a -> ('x, 'y, 'b) t) -> ('x, 'y, 'b) t
  end

  module Make3 (M : O3) : S3
  with type ('x, 'y, 'a) t = ('x, 'y, 'a) M.t = struct
    open Util
    include M
    let return = pure
    let (>>=) = bind
    let (let*) = (>>=)
    let (=<<) f a = a >>= f
    let (>>) = ( *>)
    let (<<) = (<* )
    let (>=>) bs cs a = bs a >>= cs
    let (<=<) bs cs a = cs a >>= bs
    (* FIXME: not tail recursive *)
    let rec forever a = a >> (forever a)
    let join mma = mma >>= id
    let liftm = fmap
    let liftm2 f a b = f <$> a <*> b
    let liftm3 f a b c = f <$> a <*> b <*> c
    let liftm4 f a b c d = f <$> a <*> b <*> c <*> d
    let liftm5 f a b c d e = f <$> a <*> b <*> c <*> d <*> e
    let ap = (<*>)
  end

  module Make2 (M : O2) : S2
  with type ('a, 'b) t := ('a, 'b) M.t = Make3 (struct
    type ('x, 'a, 'b) t = ('a, 'b) M.t
    include (M : O2 with type ('a, 'b) t := ('a, 'b) M.t)
  end)

  module Make (M : O) : S
  with type 'a t := 'a M.t = Make2 (struct
    type ('x, 'a) t = 'a M.t
    include (M : O with type 'a t := 'a M.t)
  end)
end
