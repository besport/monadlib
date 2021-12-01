module type T = BatInterfaces.Monad

module type S = sig
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

module List = BatList

module Make (M : BatInterfaces.Monad) = struct
  include M

  let ( >>= ) = bind
  let ( let> ) = bind
  let ( >=> ) g f x = g x >>= f
  let ( <=< ) f g x = g x >>= f
  let map f x = x >>= fun x -> return (f x)
  let map2 f x y = x >>= fun x -> map (f x) y

  module Ap = Applicative.Make (struct
    include M

    let ( <*> ) f x = map2 (fun f x -> f x) f x
  end)

  include (Ap : Applicative.S with type 'a m := 'a m)

  let join m = m >>= fun x -> x

  let filter_map_list p xs =
    let> l = sequence @@ List.map p xs in
    return (List.filter_map BatPervasives.identity l)

  let filter_list p =
    let rec loop m = function
      | [] -> map List.rev m
      | x :: xs ->
          loop
            ( p x >>= fun b ->
              m >>= fun ys -> return (if b then x :: ys else ys) )
            xs
    in
    loop (return [])
end

module type Monoid = sig
  type t

  val zero : unit -> t
  val plus : t -> t -> t
end
