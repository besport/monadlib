open Monad

(** S is another base module useful when the monad is a lazy data
    structure. We then allow the plus operation to be non-strict in its second
    argument, which makes it possible to use functions such as
    {! Monad.lsum} lazily. This is what you want for lazy lists. *)
module type T = sig
  include BatInterfaces.Monad

  val zero : unit -> 'a m
  val lplus : 'a m -> 'a m Lazy.t -> 'a m

  val null : 'a m -> bool
  (** null x implies that x is zero. If you do not want to or cannot
      answer whether a given x is zero, then null x should be false. *)
end

(** This is the counterpart for the lazy version of {! BasePlus}. *)
module type S = sig
  include T
  include MonadPlus with type 'a m := 'a m

  val of_llist : 'a LazyList.t -> 'a m
  val lsum : 'a LazyList.t m -> 'a m
  val lmsum : 'a m LazyList.t -> 'a m

  val ltranspose : 'a LazyList.t m -> 'a m LazyList.t
  (** Generalises matrix transposition. You don't necessarily have to worry about
  correctly implementing {! T.null} for this function, since the return
  value can happily be infinite. *)
end

module Make (M : T) : S with type 'a m = 'a M.m
module LazyList : S with type 'a m = 'a LazyList.t
