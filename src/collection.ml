open Monad

module type T = sig
  include LazyPlus.T

  val difference : ('a -> 'a -> bool) -> 'a m -> 'a m -> 'a m
  val unique : ?cmp:('a -> 'a -> bool) -> 'a m -> 'a m
  val maxima : ('a -> 'a -> bool) -> 'a m -> 'a m
  val nub : ('a -> 'a -> bool) -> 'a m -> 'a m
end

module MakeOpt (C : T) = struct
  module O = Make (Option.M)

  let liftp2 p x y =
    match x, y with
    | None, _ -> true
    | Some _, None -> false
    | Some x, Some y -> p x y

  include Option.Trans (C)

  let zero () = C.zero ()
  let lplus xs ys = C.lplus xs ys
  let null xs = C.null xs
  let cmp_on p = liftp2 p
  let difference p = C.difference (cmp_on p)
  let unique ?(cmp = ( = )) = C.unique ~cmp:(cmp_on cmp)
  let nub p = C.nub (cmp_on p)
  let maxima p = C.maxima (cmp_on p)
end
