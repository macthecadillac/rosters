open TypeClasses

module Identity : sig
  (** The identity monad *)
  type 'a t = 'a
  include Monad.S with type 'a t := 'a t
end

module Fun : sig
  type ('a, 'b) t = 'a -> 'b
  include module type of Fun
  include Monad.S2 with type ('a, 'b) t := ('a, 'b) t
end

module Option : sig
  include module type of Option
  type 'a t = 'a option
  include Monad.S with type 'a t := 'a t
end

module Result : sig
  include module type of Result
  type ('e, 'a) t = ('a, 'e) result
  include Monad.S2 with type ('e, 'a) t := ('e, 'a) t
end

module List : sig
  include module type of List
  type 'a t = 'a list
  include Monad.S with type 'a t := 'a t
end
