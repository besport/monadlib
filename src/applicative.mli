(** Applicative functors.

    With {! TagTree}, I noticed that the derived applicative library is much more
    efficient than the derived monad library. So in the {! Monad}, I have made sure
    that all of the applicative functions override the monad ones.

    @author Phil Scott
*)

(** {6 Base Modules} *)

module type Base = sig
  type 'a m

  val return : 'a -> 'a m
  val ( <*> ) : ('a -> 'b) m -> 'a m -> 'b m
end

(** {6 Library Types } *)

module type Applicative = sig
  include Base

  val lift1 : ('a -> 'b) -> 'a m -> 'b m
  val lift2 : ('a -> 'b -> 'c) -> 'a m -> 'b m -> 'c m
  val lift3 : ('a -> 'b -> 'c -> 'd) -> 'a m -> 'b m -> 'c m -> 'd m

  val lift4
    :  ('a -> 'b -> 'c -> 'd -> 'e)
    -> 'a m
    -> 'b m
    -> 'c m
    -> 'd m
    -> 'e m

  val ( <$> ) : ('a -> 'b) -> 'a m -> 'b m
  (** Alias for lift1. *)

  val map : ('a -> 'b) -> 'a m -> 'b m
  (** Alias for lift1. *)

  val sequence : 'a m list -> 'a list m
  val map_a : ('a -> 'b m) -> 'a list -> 'b list m
  val ( <* ) : 'a m -> 'b m -> 'a m
  val ( >* ) : 'a m -> 'b m -> 'b m
  val ignore : 'a m -> unit m
  val onlyif : bool -> unit m -> unit m
  val unless : bool -> unit m -> unit m
end

(** {6 Library Creation} *)
module Make (A : Base) : Applicative with type 'a m = 'a A.m

(** {6 Transformer } *)
module Transform (A : Base) (Inner : Base) :
  Base with type 'a m = 'a Inner.m A.m
