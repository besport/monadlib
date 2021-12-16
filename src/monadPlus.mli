(** Monads with additional monoid structure. *)
module type T = sig
  include BatInterfaces.Monad

  val zero : unit -> 'a m
  val plus : 'a m -> 'a m -> 'a m

  val null : 'a m -> bool
  (** null x implies that x is zero. If you do not want to or cannot
      answer whether a given x is zero, then null x should be false. I have
      provided this so that streams can be implemented more efficiently. *)
end

(** Library functions for monads with additional monoid structure. *)
module type S = sig
  include T
  include Monad.S with type 'a m := 'a m

  module Infix : sig
    val ( ++ ) : 'a m -> 'a m -> 'a m
    val ( +? ) : 'a option -> 'a m -> 'a m

    include module type of Infix
  end

  include module type of Infix

  val catch : ('a -> 'a m) -> 'a m -> 'a m
  val filter : ('a -> bool) -> 'a m -> 'a m

  (** {1 Option functions} *)

  val of_option : 'a option -> 'a m

  val optionally : ('a -> 'b m) -> 'a option -> 'b m
  (** overwrites {! Applicative.optionally} with a more general type.
      ATTENTION: changes semantics; {None} maps to {zero ()}, not {return ()}! *)

  (* {1 List functions} *)

  val of_list : 'a list -> 'a m
  val sum : 'a list m -> 'a m
  val msum : 'a m list -> 'a m

  (** {1 Boolean functions} *)

  val guard : bool -> unit m
  val only_if : bool -> (unit -> 'a) -> 'a m

  val conditional : bool -> (unit -> 'a m) -> 'a m
  (** overwrites {! Applicative.conditional} with a more general type.
      ATTENTION: changes semantics; {false} maps to {zero ()}, not {return ()}! *)

  (** {1 Miscellaneous} *)

  val transpose : 'a list m -> 'a m list
  (** Generalises matrix transposition. This will loop infinitely if
  {! BasePlus.null} cannot answer [true] for [zero]es. *)
end

module Make (M : T) : S with type 'a m = 'a M.m
