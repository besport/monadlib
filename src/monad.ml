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
  module Ap : Applicative.S
  include Applicative.S
  include BatInterfaces.Monad with type 'a m := 'a m

  val ( >>= ) : 'a m -> ('a -> 'b m) -> 'b m
  val ( let> ) : 'a m -> ('a -> 'b m) -> 'b m
  val ( >=> ) : ('a -> 'b m) -> ('b -> 'c m) -> 'a -> 'c m
  val ( <=< ) : ('b -> 'c m) -> ('a -> 'b m) -> 'a -> 'c m
  val join : 'a m m -> 'a m

  (* {1 List functions} *)
  (** Note that {! Ap.list_map} is overwritten with {! list_map_s} *)

  val list_fold_left : ('a -> 'b -> 'a m) -> 'a -> 'b list -> 'a m
  val list_fold_right : ('a -> 'b -> 'b m) -> 'b -> 'a list -> 'b m
  val list_filter : ('a -> bool m) -> 'a list -> 'a list m
  val list_filter_map : ('a -> 'b option m) -> 'a list -> 'b list m
end

module Make (M : BatInterfaces.Monad) : S with type 'a m = 'a M.m = struct
  include M

  let ( >>= ) = bind
  let ( let> ) = bind
  let ( >=> ) g f x = g x >>= f
  let ( <=< ) f g x = g x >>= f

  module Ap = Applicative.Make (struct
    include M

    let ( <*> ) f x = bind f (fun f -> bind x (fun x -> return (f x)))
  end)

  (* With {! TagTree}, I noticed that the derived applicative library is much more
     efficient than the derived monad library. So in the {! Monad}, I have made sure
     that all of the applicative functions override the monad ones. @author Phil Scott *)
  include (Ap : Applicative.S with type 'a m := 'a m)

  let join m = m >>= fun x -> x

  let list_fold_left f acc l =
    let rec loop acc = function
      | [] -> return acc
      | h :: t -> f acc h >>= fun h' -> loop h' t
    in
    loop acc l

  let list_fold_right f acc l =
    let rec loop acc = function
      | [] -> return acc
      | h :: t -> loop acc t >>= f h
    in
    loop acc l

  let list_filter p l =
    let f acc x = p x >>= fun b -> return @@ if b then x :: acc else acc in
    list_fold_left f [] l

  let list_filter_map p xs =
    let> l = sequence @@ BatList.map p xs in
    return (BatList.filter_map BatPervasives.identity l)
end
