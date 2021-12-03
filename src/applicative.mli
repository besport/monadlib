(** Applicative functors.

    With {! TagTree}, I noticed that the derived applicative library is much more
    efficient than the derived monad library. So in the {! Monad}, I have made sure
    that all of the applicative functions override the monad ones.

    @author Phil Scott
*)

(** {1 Base Modules} *)

module type T = sig
  type 'a m

  val return : 'a -> 'a m
  val ( <*> ) : ('a -> 'b) m -> 'a m -> 'b m
end

(** {1 Library Types } *)

module type S = sig
  include T

  val map : ('a -> 'b) -> 'a m -> 'b m
  val map2 : ('a -> 'b -> 'c) -> 'a m -> 'b m -> 'c m
  val map3 : ('a -> 'b -> 'c -> 'd) -> 'a m -> 'b m -> 'c m -> 'd m

  val map4
    :  ('a -> 'b -> 'c -> 'd -> 'e)
    -> 'a m
    -> 'b m
    -> 'c m
    -> 'd m
    -> 'e m

  val ( $ ) : ('a -> 'b) -> 'a m -> 'b m
  (** Alias for map *)

  val ( let$ ) : 'a m -> ('a -> 'b) -> 'b m
  (** Binding operator for map *)

  val sequence : 'a m list -> 'a list m
  val sequence_unit : unit m list -> unit m
  val map_list : ('a -> 'b m) -> 'a list -> 'b list m
  val optional : 'a m option -> 'a option m
  val map_option : ('a -> 'b m) -> 'a option -> 'b option m
  val ( <* ) : 'a m -> 'b m -> 'a m
  val ( *> ) : 'a m -> 'b m -> 'b m
  val ignore : 'a m -> unit m
  val onlyif : bool -> unit m -> unit m
  val unless : bool -> unit m -> unit m

  module Tuple2 : sig
    val map : ('a -> 'b m) -> ('c -> 'd m) -> 'a * 'c -> ('b * 'd) m
    val map1 : ('a -> 'b m) -> 'a * 'c -> ('b * 'c) m
    val map2 : ('a -> 'b m) -> 'c * 'a -> ('c * 'b) m
  end

  module Tuple3 : sig
    val map
      :  ('a -> 'b m)
      -> ('c -> 'd m)
      -> ('e -> 'f m)
      -> 'a * 'c * 'e
      -> ('b * 'd * 'f) m

    val map1 : ('a -> 'b m) -> 'a * 'c * 'd -> ('b * 'c * 'd) m
    val map2 : ('a -> 'b m) -> 'c * 'a * 'd -> ('c * 'b * 'd) m
    val map3 : ('a -> 'b m) -> 'c * 'd * 'a -> ('c * 'd * 'b) m
  end
end

(** {1 Library Creation} *)

module Make (T : T) : S with type 'a m = 'a T.m

(** {1 Transformer } *)

module Transform (T : T) (Inner : T) : T with type 'a m = 'a Inner.m T.m
