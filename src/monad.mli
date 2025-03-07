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
    (** an operator for [bind]; The monadic [bind] operator passes the result of a monadic computation to the next monadic computation. *)

    val ( >=> ) : ('a -> 'b m) -> ('b -> 'c m) -> 'a -> 'c m
    (** a monadic function composition; [f >=> g] applies [f] to some argument and then [g] to the resulting value. *)

    val ( <=< ) : ('b -> 'c m) -> ('a -> 'b m) -> 'a -> 'c m
    (** a monadic function composition; [g <=< f] applies [f] to some argument and then [g] to the resulting value. *)

    include module type of Ap.Infix
  end

  include module type of Infix

  module Bindings : sig
    val ( let* ) : 'a m -> ('a -> 'b m) -> 'b m
    (** a binding operator for the monadic [bind]. [let* y = x in f y] corresponds to [bind x f]. *)

    include module type of Ap.Bindings
  end

  include module type of Bindings

  val join : 'a m m -> 'a m

  (* {1 List functions} *)

  val list_fold_left : ('a -> 'b -> 'a m) -> 'a -> 'b list -> 'a m
  (** like [List.fold_left] but for monadic functions *)

  val list_fold_lefti : ('a -> int -> 'b -> 'a m) -> 'a -> 'b list -> 'a m
  (** like [List.fold_lefti] but for monadic functions *)

  val list_fold_right : ('a -> 'b -> 'b m) -> 'b -> 'a list -> 'b m
  (** like [List.fold_right] but for monadic functions *)

  val list_fold_righti : ('a -> int -> 'b -> 'b m) -> 'b -> 'a list -> 'b m
  (** like [List.fold_righti] but for monadic functions *)

  module Tuple2 : sig
    include module type of Ap.Tuple2

    val join : 'a m * 'b m -> ('a * 'b) m
    val bind1 : ('a * 'b) m -> ('a -> 'c m) -> ('c * 'b) m
    val bind2 : ('a * 'b) m -> ('b -> 'c m) -> ('a * 'c) m
  end

  module Tuple3 : sig
    include module type of Ap.Tuple3

    val join : 'a m * 'b m * 'c m -> ('a * 'b * 'c) m
    val bind1 : ('a * 'b * 'c) m -> ('a -> 'd m) -> ('d * 'b * 'c) m
    val bind2 : ('a * 'b * 'c) m -> ('b -> 'd m) -> ('a * 'd * 'c) m
    val bind3 : ('a * 'b * 'c) m -> ('c -> 'd m) -> ('a * 'b * 'd) m
  end

  module Tuple4 : sig
    include module type of Ap.Tuple4

    val join : 'a m * 'b m * 'c m * 'd m -> ('a * 'b * 'c * 'd) m
    val bind1 : ('a * 'b * 'c * 'd) m -> ('a -> 'e m) -> ('e * 'b * 'c * 'd) m
    val bind2 : ('a * 'b * 'c * 'd) m -> ('b -> 'e m) -> ('a * 'e * 'c * 'd) m
    val bind3 : ('a * 'b * 'c * 'd) m -> ('c -> 'e m) -> ('a * 'b * 'e * 'd) m
    val bind4 : ('a * 'b * 'c * 'd) m -> ('d -> 'e m) -> ('a * 'b * 'c * 'e) m
  end

  module Tuple5 : sig
    include module type of Ap.Tuple5

    val join : 'a m * 'b m * 'c m * 'd m * 'e m -> ('a * 'b * 'c * 'd * 'e) m

    val bind1
      :  ('a * 'b * 'c * 'd * 'e) m
      -> ('a -> 'f m)
      -> ('f * 'b * 'c * 'd * 'e) m

    val bind2
      :  ('a * 'b * 'c * 'd * 'e) m
      -> ('b -> 'f m)
      -> ('a * 'f * 'c * 'd * 'e) m

    val bind3
      :  ('a * 'b * 'c * 'd * 'e) m
      -> ('c -> 'f m)
      -> ('a * 'b * 'f * 'd * 'e) m

    val bind4
      :  ('a * 'b * 'c * 'd * 'e) m
      -> ('d -> 'f m)
      -> ('a * 'b * 'c * 'f * 'e) m

    val bind5
      :  ('a * 'b * 'c * 'd * 'e) m
      -> ('e -> 'f m)
      -> ('a * 'b * 'c * 'd * 'f) m
  end
end

module Make (M : BatInterfaces.Monad) : S with type 'a m = 'a M.m
