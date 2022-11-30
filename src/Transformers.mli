open TypeClasses
open Monads

module StateT : sig
  module type S = sig
    type ('s, 'a) t
    type 'a m
    include Monad.S2 with type ('s, 'a) t := ('s, 'a) t
    val state : ('s -> ('a * 's) m) -> ('s, 'a) t
    val run : ('s, 'a) t -> 's -> ('a * 's) m
    val eval : ('s, 'a) t -> 's -> 'a m
    val exec : ('s, 'a) t -> 's -> 's m
    val get : ('s, 's) t
    val put : 's -> ('s, unit) t
  end

  module Make (M : Monad.S) : S
    with type 'a m = 'a M.t
end

module State : sig
  type ('s, 'a) t
  type 'a m = 'a
  include Monad.S2 with type ('s, 'a) t := ('s, 'a) t
  val run : ('s, 'a) t -> 's -> ('a * 's)
  val eval : ('s, 'a) t -> 's -> 'a
  val exec : ('s, 'a) t -> 's -> 's
  val get : ('s, 's) t
  val put : 's -> ('s, unit) t
end

module ExceptT : sig
  module type S = sig
    type ('e, 'a) t
    type 'a m
    include Monad.S2 with type ('e, 'a) t := ('e, 'a) t
    val run : ('e, 'a) t -> ('e, 'a) Result.t m
  end

  module Make (M : Monad.S) : S
    with type 'a m = 'a M.t
end

module Monoid : sig
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

module WriterT : sig
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
     and type 'a m = 'a M.t
end

module Writer (W : Monoid.S) : WriterT.S
  with type 'a w = 'a W.t
