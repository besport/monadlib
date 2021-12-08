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

  module Operators : sig
    val ( ++ ) : 'a m -> 'a m -> 'a m
    val ( +? ) : 'a m option -> 'a m -> 'a m

    include module type of Operators
  end

  include module type of Operators

  val catch : ('a -> 'a m) -> 'a m -> 'a m
  val filter : ('a -> bool) -> 'a m -> 'a m
  val of_list : 'a list -> 'a m
  val sum : 'a list m -> 'a m
  val msum : 'a m list -> 'a m

  (** {1 Boolean functions} *)

  val guard : bool -> unit m
  val only_if : bool -> (unit -> 'a m) -> 'a m

  val conditional : bool -> (unit -> 'a m) -> 'a m
  (** overwrites {! Applicative.conditional} with a more general type. *)

  (** {1 Miscellaneous} *)

  val transpose : 'a list m -> 'a m list
  (** Generalises matrix transposition. This will loop infinitely if
  {! BasePlus.null} cannot answer [true] for [zero]es. *)
end

module Make (M : T) : S with type 'a m = 'a M.m = struct
  include Monad.Make (M)

  let zero () = M.zero ()
  let plus = M.plus

  module Operators = struct
    let ( ++ ) = M.plus
    let ( +? ) x y = match x with None -> y | Some x -> x ++ y

    include Operators
  end

  include Operators

  let null = M.null
  let catch f x = if null x then x >>= f else x
  let filter p xs = xs >>= fun x -> if p x then return x else zero ()
  let of_list xs = BatList.fold_left (fun x y -> plus x (return y)) (zero ()) xs

  let sum xs =
    xs >>= fun xs -> BatList.fold_right plus (BatList.map return xs) (zero ())

  let msum xs = BatList.fold_left plus (zero ()) xs
  let guard b = if b then return () else zero ()
  let conditional p f = guard p >>= f
  let only_if b f = if b then f () else zero ()

  let rec transpose xs =
    let hds = sum (map (BatList.take 1) xs) in
    if null hds then [] else hds :: transpose (map (BatList.drop 1) xs)
end
