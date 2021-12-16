(** The monad library.

  {b Introduction}

  Monads in ocaml, as defined in the batteries library and lwt, are defined
  narrowly in terms of a type constructor, and two functions, [return] and
  [bind]. This misses the [i abstraction], which lies in the ability to write
  functions that apply generally to [i all] monads. This library defines modules
  for such functions.

  @author Phil Scott
*)

module type T = BatInterfaces.Monad

module type S = sig
  include BatInterfaces.Monad
  module Ap : Applicative.S with type 'a m := 'a m
  include Applicative.S with type 'a m := 'a m

  module Infix : sig
    val ( >>= ) : 'a m -> ('a -> 'b m) -> 'b m
    val ( >=> ) : ('a -> 'b m) -> ('b -> 'c m) -> 'a -> 'c m
    val ( <=< ) : ('b -> 'c m) -> ('a -> 'b m) -> 'a -> 'c m

    include module type of Ap.Infix
  end

  include module type of Infix

  module Bindings : sig
    val ( let* ) : 'a m -> ('a -> 'b m) -> 'b m

    include module type of Ap.Bindings
  end

  include module type of Bindings

  val join : 'a m m -> 'a m

  (* {1 List functions} *)

  val list_fold_left : ('a -> 'b -> 'a m) -> 'a -> 'b list -> 'a m
  val list_fold_right : ('a -> 'b -> 'b m) -> 'b -> 'a list -> 'b m
end

module Make (M : BatInterfaces.Monad) : S with type 'a m = 'a M.m
