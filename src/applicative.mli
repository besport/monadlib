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
  (** [map f x] applies a pure function [f] to a monadic value [x]. *)

  val fmap : ('a -> 'b) m -> 'a -> 'b m
  (** [map f x] applies a monadic function [f] to a pure value [x]. *)

  val iter : ('a -> unit) -> 'a m -> unit
  (** [iter f x] applies a pure function [f] to a monadic value [x] and discards the result. *)

  module Infix : sig
    val ( <@> ) : ('a -> 'b) -> 'a m -> 'b m
    (** Alias for [map]. [f <@> x] applies a pure function [f] to a monadic value [x]. *)

    val ( <*> ) : ('a -> 'b) m -> 'a m -> 'b m
    (** [f <*> x] applies a monadic function [f] to a monadic value [x].
        This operator is usually used in conjunction with [<@>] in order to accumulate monadic parameters.
        Example: [max <@> return 1 <*> return 2] will yield [return 2].
        Here [max] has arity 2, so we use [<*>] to add a second monadic parameter. *)

    val ( <* ) : 'a m -> 'b m -> 'a m
    (** [x <* y] ignores the result of [y]. *)

    val ( *> ) : 'a m -> 'b m -> 'b m
    (** [x <* y] ignores the result of [x]. *)
  end

  include module type of Infix

  module Bindings : sig
    val ( let+ ) : 'a m -> ('a -> 'b) -> 'b m
    (** binding operator for [map] *)

    val ( and+ ) : 'a m -> 'b m -> ('a * 'b) m
    (** binding operator for [map] *)
  end

  include module type of Bindings

  val ignore : 'a m -> unit m

  val map2 : ('a -> 'b -> 'c) -> 'a m -> 'b m -> 'c m
  (** [map2 f x y] is the same as the same as [f <@> x <*> y]. *)

  val map3 : ('a -> 'b -> 'c -> 'd) -> 'a m -> 'b m -> 'c m -> 'd m
  (** [map3 f x y z] is the same as [f <@> x <*> y <*> z]. *)

  val map4
    :  ('a -> 'b -> 'c -> 'd -> 'e)
    -> 'a m
    -> 'b m
    -> 'c m
    -> 'd m
    -> 'e m
  (** [map4 f w x y z] is the same as [f <@> w <*> x <*> y <*> z]. *)

  (** {1 List functions} *)

  val sequence : (unit -> 'a m) list -> 'a list m
  val sequence_unit : (unit -> unit m) list -> unit m

  val list_map : ('a -> 'b m) -> 'a list -> 'b list m
  (** like {!List.map} but for monadic functions. *)

  val list_iter : ('a -> unit m) -> 'a list -> unit m
  (** like {!List.iter} but for monadic functions. *)

  val list_filter : ('a -> bool m) -> 'a list -> 'a list m
  (** like {!List.filter} but for monadic functions. *)

  val list_filter_map : ('a -> 'b option m) -> 'a list -> 'b list m
  (** like {!List.filter_map} but for monadic functions. *)

  (** {1 Option functions} *)

  val option_map : ('a -> 'b m) -> 'a option -> 'b option m
  (** like {!Option.map} but for monadic functions. *)

  val optionally : ('a -> unit m) -> 'a option -> unit m
  (** [optionally f (Some x)] executes [f x]; [optionally f None] does nothing. *)

  (** {1 Boolean function} *)

  val optional : bool -> (unit -> 'a m) -> 'a option m
  (** [optional true f] executes [f] and applies [Some] to the result; [optional false f] returns [None]. *)

  val conditional : bool -> (unit -> unit m) -> unit m
  (** [conditional true f] executes [f]; [conditional false f] does nothing. *)

  (** {1 Tuple functions *)

  val split : ('a * 'b) m -> 'a m * 'b m
end

module Make (A : T) : S with type 'a m = 'a A.m
module Trans (A : S) (Inner : S) : S with type 'a m = 'a Inner.m A.m
