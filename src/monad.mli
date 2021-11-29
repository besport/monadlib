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

(** {1 Library Types } *)

(** Your basic library functions for monads. *)
module type Monad = sig
  include BatInterfaces.Monad
  include Applicative.S with type 'a m := 'a m

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

(** {1 Library Creation} *)

module Make (M : BatInterfaces.Monad) : Monad with type 'a m = 'a M.m
module MakePlus (M : BasePlus) : MonadPlus with type 'a m = 'a M.m

(** {1 Specific monads} *)

module Identity : Monad with type 'a m = 'a

module LazyM : Monad with type 'a m = 'a Lazy.t
(** The lazy monad. Automatically wraps calls lazily and forces as needed. *)

module List : MonadPlus with type 'a m = 'a list
module Option : MonadPlus with type 'a m = 'a option

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
