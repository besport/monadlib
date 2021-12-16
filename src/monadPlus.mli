(** Monads with additional monoid structure. *)
module type T = sig
  include BatInterfaces.Monad

  val zero : unit -> 'a m
  (** generates a neutral element with respect to [plus] *)

  val plus : 'a m -> 'a m -> 'a m
  (** add two monadic values *)

  val null : 'a m -> bool
  (** null x implies that x is zero. If you do not want to or cannot
      answer whether a given x is zero, then null x should raise an exception. I have
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
  (* [catch f x] can be used to recover from an error (when [x] is [null]). *)

  val filter : ('a -> bool) -> 'a m -> 'a m

  (** {1 Option functions} *)

  val of_option : 'a option -> 'a m
  (** [of_option (Some x)] = [return x]; [of_option None] = [zero ()] *)

  val optionally : ('a -> 'b m) -> 'a option -> 'b m
  (** [optionally f (Some x)] executes [f x]; [optionally f None] executes [zero ()].
      ATTENTION: this function overwrites {!Applicative.S.optionally} generalising its type and changing its semantics!
                 [None] maps to [zero ()] instead of [return ()]! *)

  (* {1 List functions} *)

  val of_list : 'a list -> 'a m
  val sum : 'a list m -> 'a m
  val msum : 'a m list -> 'a m

  (** {1 Boolean functions} *)

  val guard : bool -> unit m
  (** [guard true] does nothing; [guard false] yields [zero ()).
      This can be used to render a computation conditional.
      Example: [let* () = guard (a = b) in f]
      Here [f] is only executed if [a = b]. *)

  val only_if : bool -> (unit -> 'a) -> 'a m
  (** [only_if true f] = [f ()]; [only_if false f] = [zero ()] *)

  val conditional : bool -> (unit -> 'a m) -> 'a m
  (** [conditional true f] executes [f]; [conditional false f] executes [zero ()].
      ATTENTION: this function overwrites {!Applicative.S.conditional} generalising its type and changing its semantics!
                 [conditional false f] maps to [zero ()] instead of [return ()]! *)

  (** {1 Miscellaneous} *)

  val transpose : 'a list m -> 'a m list
  (** Generalises matrix transposition. Relies on [null]. *)
end

module Make (M : T) : S with type 'a m = 'a M.m
