(** The monad library.

    {b Introduction}

    Monads in ocaml, as defined in the batteries library and lwt, are defined
    narrowly in terms of a type constructor, and two functions, [return] and
    [bind]. This misses the [i abstraction], which lies in the ability to write
    functions that apply generally to [i all] monads. This library defines modules
    for such functions.

    @author Phil Scott
 *)

(** {1 Base Modules}*)

module type Monoid = sig
  type t

  val zero : unit -> t
  val plus : t -> t -> t
end

(** Monads with additional monoid structure. *)
module type BasePlus = sig
  include BatInterfaces.Monad

  val zero : unit -> 'a m
  val plus : 'a m -> 'a m -> 'a m

  val null : 'a m -> bool
  (** null x implies that x is zero. If you do not want to or cannot
      answer whether a given x is zero, then null x should be false. I have
      provided this so that streams can be implemented more efficiently. *)
end

(** LazyPlus is another base module useful when the monad is a lazy data
    structure. We then allow the plus operation to be non-strict in its second
    argument, which makes it possible to use functions such as
    {! Monad.lsum} lazily. This is what you want for lazy lists. *)
module type BaseLazyPlus = sig
  include BatInterfaces.Monad

  val zero : unit -> 'a m
  val lplus : 'a m -> 'a m Lazy.t -> 'a m

  val null : 'a m -> bool
  (** null x implies that x is zero. If you do not want to or cannot
      answer whether a given x is zero, then null x should be false. *)
end

(** {1 Library Types } *)

(** Your basic library functions for monads. *)
module type Monad = sig
  include BatInterfaces.Monad
  include Applicative.Applicative with type 'a m := 'a m

  val ( >>= ) : 'a m -> ('a -> 'b m) -> 'b m
  val ( let> ) : 'a m -> ('a -> 'b m) -> 'b m
  val ( >=> ) : ('a -> 'b m) -> ('b -> 'c m) -> 'a -> 'c m
  val ( <=< ) : ('b -> 'c m) -> ('a -> 'b m) -> 'a -> 'c m
  val join : 'a m m -> 'a m
  val filter_m : ('a -> bool m) -> 'a list -> 'a list m
end

(** Library functions for monads with additional monoid structure. *)
module type MonadPlus = sig
  include BasePlus
  include Monad with type 'a m := 'a m

  val filter : ('a -> bool) -> 'a m -> 'a m
  val of_list : 'a list -> 'a m
  val sum : 'a list m -> 'a m
  val msum : 'a m list -> 'a m
  val guard : bool -> unit m

  val transpose : 'a list m -> 'a m list
  (** Generalises matrix transposition. This will loop infinitely if
  {! BasePlus.null} cannot answer [true] for [zero]es. *)
end

(** This is the counterpart for the lazy version of {! BasePlus}. *)
module type LazyPlus = sig
  include BaseLazyPlus
  include MonadPlus with type 'a m := 'a m

  val of_llist : 'a LazyList.t -> 'a m
  val lsum : 'a LazyList.t m -> 'a m
  val lmsum : 'a m LazyList.t -> 'a m

  val ltranspose : 'a LazyList.t m -> 'a m LazyList.t
  (** Generalises matrix transposition. You don't necessarily have to worry about
  correctly implementing {! BaseLazyPlus.null} for this function, since the return
  value can happily be infinite. *)
end

(** {1 Library Creation} *)

module Make (M : BatInterfaces.Monad) : Monad with type 'a m = 'a M.m
module MakePlus (M : BasePlus) : MonadPlus with type 'a m = 'a M.m
module MakeLazyPlus (M : BaseLazyPlus) : LazyPlus with type 'a m = 'a M.m

(** {1 Specific monads} *)

module Identity : Monad with type 'a m = 'a

module LazyM : Monad with type 'a m = 'a Lazy.t
(** The lazy monad. Automatically wraps calls lazily and forces as needed. *)

module List : MonadPlus with type 'a m = 'a list
module LazyListM : LazyPlus with type 'a m = 'a LazyList.t
module Option : MonadPlus with type 'a m = 'a option

(** For the incorruptible programmer:

    A continuation of type 'a has type [('a -> 'r) -> 'r]

    The first argument to this continuation is a final value of type 'r which
    depends on some intermediate value of type 'a. In other words, we can
    understand the continuation as a result of type 'r which depends on the {i
    future} of an intermediate value.

    The function [return] just {i throws} its return value to the future in
    order to produce the final result. The [bind] intercedes in the future,
    inserting a computation.

    Call-with-current-continuation allows one to effectively reflect into one's own
    future. That is, callCC is a computation which depends on another computation
    taking the future as a first-class value. One can store this future, and at any
    time, throw it a return value to reinstate it.

    If you are into the Curry-Howard Isomorphism,
    call-with-current-continuation has a type which corresponds to a law of
    classical logic (Pierce's Law). Writing your code in the continuation
    monad corresponds to embedding classical logic intuitionistically. Allowing
    callCC corresponds to assuming a classical hypothesis.
 *)
module Continuation (T : sig
  type r
end) : sig
  include Monad with type 'a m = ('a -> T.r) -> T.r

  val callCC : (('a -> 'r m) -> 'a m) -> 'a m
end

module Result (E : sig
  type e

  val defaultError : e
end) : sig
  type 'a err = ('a, E.e) result

  include MonadPlus

  val throw : E.e -> 'a m
  val catch : 'a m -> (E.e -> 'a m) -> 'a m
  val run_error : 'a m -> 'a err
end

(** {1 Transformers} *)

module LazyT (M : BatInterfaces.Monad) : sig
  include Monad with type 'a m = 'a Lazy.t M.m

  val lift : 'a M.m -> 'a m
end

(** The list monad transformer will add non-determinism to computations. I
    have not provided a transformer for lazy lists, since I'm not yet sure how
    to implement it. It would probably need a lazy version of map_m, but it's
    not clear to me how to write this, since whether the computations are
    strict will determine whether the argument has to be completely forced. *)
module ListT (M : BatInterfaces.Monad) : sig
  include Monad with type 'a m = 'a list M.m

  val lift : 'a M.m -> 'a m
end

module OptionT (M : BatInterfaces.Monad) : sig
  include Monad with type 'a m = 'a option M.m

  val lift : 'a M.m -> 'a m
end

module ResultT (E : sig
  type e

  val defaultError : e
end)
(M : BatInterfaces.Monad) : sig
  type 'a err = ('a, E.e) result

  include Monad

  val throw : E.e -> 'a m
  val catch : 'a m -> (E.e -> 'a m) -> 'a m
  val lift : 'a M.m -> 'a m
  val run_error : 'a m -> 'a err M.m
end
