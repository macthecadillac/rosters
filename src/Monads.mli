module Reader : sig
  type ('r, 'a) t
  val reader : ('r -> ('a * 'r)) -> ('r, 'a) t
  val run : ('r, 'a) t -> 'r -> 'a
  val ask : ('r, 'r) t
  val asks : ('r -> 'b) -> ('r, 'b) t
  val fmap : ('a -> 'b) -> ('r, 'a) t -> ('r, 'b) t
  val pure : 'a -> ('r, 'a) t
  val join : ('r, ('r, 'a) t) t -> ('r, 'a) t
  val ( <$> ) : ('a -> 'b) -> ('r, 'a) t -> ('r, 'b) t
  val ( <*> ) : ('r, 'a -> 'b) t -> ('r, 'a) t -> ('r, 'b) t
  val ( >>= ) : ('r, 'a) t -> ('a -> ('r, 'b) t) -> ('r, 'b) t
  val ( let* ) : ('r, 'a) t -> ('a -> ('r, 'b) t) -> ('r, 'b) t
  val ( let+ ) : ('r, 'a) t -> ('a -> 'b) -> ('r, 'b) t
  val sequence_l : ('r, 'a) t List.t -> ('r, 'a List.t) t
end

module StateReader : sig
  type ('s, 'r, 'a) t
  val eval : ('s, 'r, 'a) t -> 's -> ('r, 'a) Reader.t
  val run : ('s, 'r, 'a) t -> 's -> ('r, 's * 'a) Reader.t
  val get : ('s, 'r, 's) t
  val gets : ('s -> 'b) -> ('s, 'r, 'b) t
  val put : 's -> ('s, 'r, unit) t
  val puts : ('s -> 's) -> ('s, 'r, unit) t
  val fmap : ('a -> 'b) -> ('s, 'r, 'a) t -> ('s, 'r, 'b) t
  val pure : 'a -> ('s, 'r, 'a) t
  val lift_reader : ('r, 'a) Reader.t -> ('s, 'r, 'a) t
  val ask : ('s, 'r, 'r) t
  val asks : ('r -> 'b) -> ('s, 'r, 'b) t
  val join : ('s, 'r, ('s, 'r, 'a) t) t -> ('s, 'r, 'a) t
  val ( <$> ) : ('a -> 'b) -> ('s, 'r, 'a) t -> ('s, 'r, 'b) t
  val ( $> ) : ('s, 'r, 'a) t -> 'b -> ('s, 'r, 'b) t
  val ( <*> ) : ('s, 'r, 'a -> 'b) t -> ('s, 'r, 'a) t -> ('s, 'r, 'b) t
  val ( *> ) : ('s, 'r, 'a) t -> ('s, 'r, 'b) t -> ('s, 'r, 'b) t
  val ( >>= ) : ('s, 'r, 'a) t -> ('a -> ('s, 'r, 'b) t) -> ('s, 'r, 'b) t
  val ( let* ) : ('s, 'r, 'a) t -> ('a -> ('s, 'r, 'b) t) -> ('s, 'r, 'b) t
  val ( let+ ) : ('s, 'r, 'a) t -> ('a -> 'b) -> ('s, 'r, 'b) t
  val sequence_l : ('s, 'r, 'a) t List.t -> ('s, 'r, 'a List.t) t
end
