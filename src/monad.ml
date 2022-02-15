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

  module Tuple3 = struct
    include Ap.Tuple3
    module Tuple3 = BatTuple.Tuple3

    let join (ma, mb, mc) =
      ma >>= fun a ->
      mb >>= fun b ->
      mc >>= fun c -> return (Tuple3.make a b c)

    let bind1 m f = m >>= fun (a, b, c) -> make1 (f a) b c
    let bind2 m f = m >>= fun (a, b, c) -> make2 a (f b) c
    let bind3 m f = m >>= fun (a, b, c) -> make3 a b (f c)
  end

  module Tuple4 = struct
    include Ap.Tuple4
    module Tuple4 = BatTuple.Tuple4

    let join (ma, mb, mc, md) =
      ma >>= fun a ->
      mb >>= fun b ->
      mc >>= fun c ->
      md >>= fun d -> return (Tuple4.make a b c d)

    let bind1 m f = m >>= fun (a, b, c, d) -> make1 (f a) b c d
    let bind2 m f = m >>= fun (a, b, c, d) -> make2 a (f b) c d
    let bind3 m f = m >>= fun (a, b, c, d) -> make3 a b (f c) d
    let bind4 m f = m >>= fun (a, b, c, d) -> make4 a b c (f d)
  end

  module Tuple5 = struct
    include Ap.Tuple5
    module Tuple5 = BatTuple.Tuple5

    let join (ma, mb, mc, md, me) =
      ma >>= fun a ->
      mb >>= fun b ->
      mc >>= fun c ->
      md >>= fun d ->
      me >>= fun e -> return (Tuple5.make a b c d e)

    let bind1 m f = m >>= fun (a, b, c, d, e) -> make1 (f a) b c d e
    let bind2 m f = m >>= fun (a, b, c, d, e) -> make2 a (f b) c d e
    let bind3 m f = m >>= fun (a, b, c, d, e) -> make3 a b (f c) d e
    let bind4 m f = m >>= fun (a, b, c, d, e) -> make4 a b c (f d) e
    let bind5 m f = m >>= fun (a, b, c, d, e) -> make5 a b c d (f e)
  end
end
