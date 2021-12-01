module type BasePlus = sig
  include BatInterfaces.Monad

  val zero : unit -> 'a m
  val plus : 'a m -> 'a m -> 'a m
  val null : 'a m -> bool
end

module type Monad = sig
  include BatInterfaces.Monad
  include Applicative.S with type 'a m := 'a m

  val ( >>= ) : 'a m -> ('a -> 'b m) -> 'b m
  val ( let> ) : 'a m -> ('a -> 'b m) -> 'b m
  val ( >=> ) : ('a -> 'b m) -> ('b -> 'c m) -> 'a -> 'c m
  val ( <=< ) : ('b -> 'c m) -> ('a -> 'b m) -> 'a -> 'c m
  val join : 'a m m -> 'a m
  val filter_list : ('a -> bool m) -> 'a list -> 'a list m
end

module type MonadPlus = sig
  include BasePlus
  include Monad with type 'a m := 'a m

  val filter : ('a -> bool) -> 'a m -> 'a m
  val of_list : 'a list -> 'a m
  val sum : 'a list m -> 'a m
  val msum : 'a m list -> 'a m
  val guard : bool -> unit m
  val transpose : 'a list m -> 'a m list
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

module MakePlus (M : BasePlus) = struct
  include Make (M)

  let zero () = M.zero ()
  let plus = M.plus
  let null = M.null
  let filter p xs = xs >>= fun x -> if p x then return x else zero ()
  let of_list xs = List.fold_left (fun x y -> plus x (return y)) (zero ()) xs

  let sum xs =
    xs >>= fun xs -> List.fold_right plus (List.map return xs) (zero ())

  let msum xs = List.fold_left plus (zero ()) xs
  let guard b = if b then return () else zero ()

  let rec transpose xs =
    let hds = sum (map (BatList.take 1) xs) in
    if null hds then [] else hds :: transpose (map (BatList.drop 1) xs)
end

module Identity : Monad with type 'a m = 'a = Make (struct
  type 'a m = 'a

  let return x = x
  let bind x f = f x
end)

module LazyM = Make (struct
  type 'a m = 'a Lazy.t

  let return x = lazy x
  let bind x f = lazy (Lazy.force (f (Lazy.force x)))
end)

module LazyT (M : Monad) = struct
  include Make (struct
    type 'a m = 'a Lazy.t M.m

    let return x = M.return (lazy x)
    let bind x f = M.bind x (fun x -> f (Lazy.force x))
  end)

  let lift x = M.map (fun x -> lazy x) x
end

module type Monoid = sig
  type t

  val zero : unit -> t
  val plus : t -> t -> t
end
