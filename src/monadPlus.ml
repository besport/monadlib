module List = BatList

module type T = sig
  include BatInterfaces.Monad

  val zero : unit -> 'a m
  val plus : 'a m -> 'a m -> 'a m
  val null : 'a m -> bool
end

module type S = sig
  include T
  include Monad.S with type 'a m := 'a m

  val ( ++ ) : 'a m -> 'a m -> 'a m
  val ( +? ) : 'a m option -> 'a m -> 'a m
  val filter : ('a -> bool) -> 'a m -> 'a m
  val of_list : 'a list -> 'a m
  val sum : 'a list m -> 'a m
  val msum : 'a m list -> 'a m
  val guard : bool -> unit m
  val only_if : bool -> (unit -> 'a) -> 'a m
  val only_if_value : bool -> 'a -> 'a m
  val transpose : 'a list m -> 'a m list
end

module Make (M : T) = struct
  include Monad.Make (M)

  let zero () = M.zero ()
  let plus = M.plus
  let ( ++ ) = M.plus
  let ( +? ) x y = match x with None -> y | Some x -> x ++ y
  let null = M.null
  let filter p xs = xs >>= fun x -> if p x then return x else zero ()
  let of_list xs = List.fold_left (fun x y -> plus x (return y)) (zero ()) xs

  let sum xs =
    xs >>= fun xs -> List.fold_right plus (List.map return xs) (zero ())

  let msum xs = List.fold_left plus (zero ()) xs
  let guard b = if b then return () else zero ()
  let only_if b v = if b then return @@ v () else zero ()
  let only_if_value b v = if b then return v else zero ()

  let rec transpose xs =
    let hds = sum (map (BatList.take 1) xs) in
    if null hds then [] else hds :: transpose (map (BatList.drop 1) xs)
end
