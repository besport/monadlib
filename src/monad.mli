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
  val filter_map_list : ('a -> 'b option m) -> 'a list -> 'b list m
  val filter_list : ('a -> bool m) -> 'a list -> 'a list m
end

(** {1 Library Creation} *)

module Make (M : BatInterfaces.Monad) : Monad with type 'a m = 'a M.m
