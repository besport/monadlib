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

  val ( ++ ) : 'a m -> 'a m -> 'a m
  val ( +? ) : 'a m option -> 'a m -> 'a m
  val filter : ('a -> bool) -> 'a m -> 'a m
  val of_list : 'a list -> 'a m
  val sum : 'a list m -> 'a m
  val msum : 'a m list -> 'a m
  val guard : bool -> unit m
  val only_if : bool -> (unit -> 'a) -> 'a m
  val only_if_value : bool -> 'a -> 'a m

  val transpose : 'a list m -> 'a m list
  (** Generalises matrix transposition. This will loop infinitely if
  {! BasePlus.null} cannot answer [true] for [zero]es. *)
end

module Make (M : T) : S with type 'a m = 'a M.m
