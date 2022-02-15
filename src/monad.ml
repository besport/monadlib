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
  val list_fold_lefti : ('a -> int -> 'b -> 'a m) -> 'a -> 'b list -> 'a m
  val list_fold_right : ('a -> 'b -> 'b m) -> 'b -> 'a list -> 'b m
  val list_fold_righti : ('a -> int -> 'b -> 'b m) -> 'b -> 'a list -> 'b m

  module Tuple2 : sig
    include module type of Ap.Tuple2

    val join : 'a m * 'b m -> ('a * 'b) m
    val bind1 : ('a * 'b) m -> ('a -> 'c m) -> ('c * 'b) m
    val bind2 : ('a * 'b) m -> ('b -> 'c m) -> ('a * 'c) m
  end
end

module Make (M : BatInterfaces.Monad) : S with type 'a m = 'a M.m = struct
  include M

  module Ap = Applicative.Make (struct
    include M

    let ( <*> ) f x = bind f (fun f -> bind x (fun x -> return (f x)))
  end)

  include (Ap : Applicative.S with type 'a m := 'a m)

  module Infix = struct
    let ( >>= ) = bind
    let ( >=> ) g f x = g x >>= f
    let ( <=< ) f g x = g x >>= f

    include Ap.Infix
  end

  include Infix

  module Bindings = struct
    let ( let* ) = bind

    include Ap.Bindings
  end

  include Bindings

  let join m = m >>= fun x -> x

  let list_fold_left f init l =
    let rec loop acc = function
      | [] -> return acc
      | h :: t -> f acc h >>= fun acc' -> loop acc' t
    in
    loop init l

  let list_fold_lefti f init l =
    let rec loop i acc = function
      | [] -> return acc
      | h :: t -> f acc i h >>= fun acc' -> loop (i + 1) acc' t
    in
    loop 0 init l

  let list_fold_right f init l =
    let rec loop acc = function
      | [] -> return acc
      | h :: t -> loop acc t >>= f h
    in
    loop init l

  let list_fold_righti f init l =
    let li_inv = BatList.fold_lefti (fun acc i x -> (i, x) :: acc) [] l in
    list_fold_left (fun acc (i, x) -> f x i acc) init li_inv

  module Tuple2 = struct
    include Ap.Tuple2
    module Tuple2 = BatTuple.Tuple2

    let join (ma, mb) =
      ma >>= fun a ->
      mb >>= fun b -> return (Tuple2.make a b)

    let bind1 m f = m >>= fun (a, b) -> make1 (f a) b
    let bind2 m f = m >>= fun (a, b) -> make2 a (f b)
  end
end
