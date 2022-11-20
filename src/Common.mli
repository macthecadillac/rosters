open Containers

type 'a printer = Format.formatter -> 'a -> unit

module IntMap : Map.S
  with type 'a t = 'a Map.Make(Int).t and type key = Int.t

module Name : sig
  type t
  val compare : t -> t -> Int.t
  val to_string : t -> String.t
  val of_string : String.t -> t Option.t
  val canonical : t -> String.t
  val canonicalize : t -> t
  val pp : t printer
end

module Section : sig
  type t
  val of_string : String.t -> t
  val to_string : t -> String.t
  val compare : t -> t -> int
  val to_int : t -> Int.t Option.t
  val validate : t -> t Option.t
  val pp : t printer
end

module SectionMap : Map.S
  with type 'a t = 'a Map.Make(Section).t and type key = Section.t
