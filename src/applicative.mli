(** {{:https://en.wikipedia.org/wiki/Applicative_functor}Applicative functors} are data structures over one can map a function.

    From a specification {!T} which defines {return} (how to package a value into your functor) and {<*>} (how to apply a function in your functor) {!Make} generates a module of type {!S} which provides a number of useful functions.
*)

module type T = sig
  type 'a m

  val return : 'a -> 'a m
  val ( <*> ) : ('a -> 'b) m -> 'a m -> 'b m
end

module type S = sig
  include T

  val map : ('a -> 'b) -> 'a m -> 'b m
  val fmap : ('a -> 'b) m -> 'a -> 'b m
  val iter : ('a -> unit) -> 'a m -> unit

  module Infix : sig
    val ( <@> ) : ('a -> 'b) -> 'a m -> 'b m
    (** Alias for map *)

    val ( <*> ) : ('a -> 'b) m -> 'a m -> 'b m
    val ( <* ) : 'a m -> 'b m -> 'a m
    val ( *> ) : 'a m -> 'b m -> 'b m
  end

  include module type of Infix

  module Bindings : sig
    val ( let+ ) : 'a m -> ('a -> 'b) -> 'b m
    val ( and+ ) : 'a m -> 'b m -> ('a * 'b) m
  end

  include module type of Bindings

  val ignore : 'a m -> unit m
  val map2 : ('a -> 'b -> 'c) -> 'a m -> 'b m -> 'c m
  val map3 : ('a -> 'b -> 'c -> 'd) -> 'a m -> 'b m -> 'c m -> 'd m

  val map4
    :  ('a -> 'b -> 'c -> 'd -> 'e)
    -> 'a m
    -> 'b m
    -> 'c m
    -> 'd m
    -> 'e m

  (** {1 List functions} *)

  val sequence : (unit -> 'a m) list -> 'a list m
  val sequence_unit : (unit -> unit m) list -> unit m
  val list_map : ('a -> 'b m) -> 'a list -> 'b list m
  val list_iter : ('a -> unit m) -> 'a list -> unit m
  val list_filter : ('a -> bool m) -> 'a list -> 'a list m
  val list_filter_map : ('a -> 'b option m) -> 'a list -> 'b list m

  (** {1 Option functions} *)

  val option_map : ('a -> 'b m) -> 'a option -> 'b option m
  val optionally : ('a -> unit m) -> 'a option -> unit m

  (** {1 Boolean function} *)

  val optional : bool -> (unit -> 'a m) -> 'a option m
  val conditional : bool -> (unit -> unit m) -> unit m

  (** {1 Tuple functions *)

  val split : ('a * 'b) m -> 'a m * 'b m
end

module Make (A : T) : S with type 'a m = 'a A.m
module Trans (A : S) (Inner : S) : S with type 'a m = 'a Inner.m A.m
