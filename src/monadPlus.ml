module type T = sig
  include BatInterfaces.Monad

  val zero : unit -> 'a m
  val plus : 'a m -> 'a m -> 'a m
  val null : 'a m -> bool
end

module type S = sig
  include T
  include Monad.S with type 'a m := 'a m

  module Infix : sig
    val ( ++ ) : 'a m -> 'a m -> 'a m
    val ( +? ) : 'a option -> 'a m -> 'a m

    include module type of Infix
  end

  include module type of Infix

  val catch : ('a -> 'a m) -> 'a m -> 'a m
  val filter : ('a -> bool) -> 'a m -> 'a m
  val of_option : 'a option -> 'a m
  val optionally : ('a -> 'b m) -> 'a option -> 'b m
  val of_list : 'a list -> 'a m
  val sum : 'a list m -> 'a m
  val msum : 'a m list -> 'a m
  val guard : bool -> unit m
  val only_if : bool -> (unit -> 'a) -> 'a m
  val conditional : bool -> (unit -> 'a m) -> 'a m
  val transpose : 'a list m -> 'a m list
end

module Make (M : T) : S with type 'a m = 'a M.m = struct
  include Monad.Make (M)

  let zero () = M.zero ()
  let plus = M.plus

  module Infix = struct
    let ( ++ ) = M.plus
    let ( +? ) x y = match x with None -> y | Some x -> return x ++ y

    include Infix
  end

  include Infix

  let null = M.null
  let catch f x = if null x then x >>= f else x
  let filter p xs = xs >>= fun x -> if p x then return x else zero ()
  let of_option = function None -> zero () | Some x -> return x
  let optionally f = function None -> zero () | Some x -> f x
  let of_list xs = BatList.fold_left (fun x y -> plus x (return y)) (zero ()) xs

  let sum xs =
    xs >>= fun xs -> BatList.fold_right plus (BatList.map return xs) (zero ())

  let msum xs = BatList.fold_left plus (zero ()) xs
  let guard b = if b then return () else zero ()
  let only_if b f = if b then return @@ f () else zero ()
  let conditional p f = if p then f () else zero ()

  let rec transpose xs =
    let hds = sum (map (BatList.take 1) xs) in
    if null hds then [] else hds :: transpose (map (BatList.drop 1) xs)
end
