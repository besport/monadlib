open Monad
module Ll = LazyList

module type T = sig
  include BatInterfaces.Monad

  val zero : unit -> 'a m
  val lplus : 'a m -> 'a m Lazy.t -> 'a m
  val null : 'a m -> bool
end

module type S = sig
  include T
  include MonadPlus with type 'a m := 'a m

  val of_llist : 'a Ll.t -> 'a m
  val lsum : 'a Ll.t m -> 'a m
  val lmsum : 'a m Ll.t -> 'a m
  val ltranspose : 'a Ll.t m -> 'a m Ll.t
end

module Make (M : T) = struct
  include MakePlus (struct
    include M

    let plus x y = M.lplus x (lazy y)
  end)

  let lplus = M.lplus

  let lsum xs =
    xs >>= fun xs -> Ll.fold_right lplus (zero ()) (Ll.map return xs)

  let of_llist xs = Ll.fold_right lplus (zero ()) (Ll.map return xs)
  let lmsum xs = Ll.fold_right lplus (zero ()) xs
  let filter p xs = xs >>= fun x -> if p x then return x else zero ()

  let rec ltranspose xs =
    lazy
      (let hds = lsum (map (Ll.take 1) xs) in
       if null hds
       then Ll.Nil
       else Ll.Cons (hds, ltranspose (map (Ll.drop 1) xs)))
end

module LazyList = struct
  include Make (struct
    type 'a m = 'a Ll.t

    let return x = Ll.singleton x
    let bind xs f = Ll.concat (Ll.map f xs)
    let zero () = Ll.nil

    let rec lplus xs ys =
      lazy
        (match Ll.next xs with
        | Ll.Nil -> Ll.next (Lazy.force ys)
        | Ll.Cons (x, xs) -> Ll.Cons (x, lplus xs ys))

    let null = Ll.is_empty
  end)
end
