open Containers

type 'a printer = Format.formatter -> 'a -> unit

module IntMap : Map.S
  with type 'a t = 'a Map.Make(Int).t
  and type key = Int.t

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
  val compare : t -> t -> int
  val to_int : t -> Int.t
  val of_int : Int.t -> t
  val of_string : String.t -> t Option.t
  val pp : t printer
end

module SectionMap : Map.S
  with type 'a t = 'a Map.Make(Section).t
  and type key = Section.t

module StringMap : Map.S
  with type 'a t = 'a Map.Make(String).t
  and type key = String.t

module Length : sig
  type t
  val zero : t
  val of_in : Float.t -> t
  val of_mm : Float.t -> t
  val of_pt : Float.t -> t
  val of_int : Int.t -> t
  val to_mm : t -> Float.t
  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( *.. ) : Float.t -> t -> t
  val max : t -> t -> t
end

type weight = Bold | Regular

type font_size = FS of Length.t

type env = { l : Length.t;
             h : Length.t;
             margin : Length.t;
             origin : Length.t * Length.t;
             lwidth1 : Length.t;
             lwidth2 : Length.t;
             weight : weight;
             font_size : font_size;
             lab : Int.t;
             checkpoints : String.t List.t }
