module type T = sig
  type 'a m

  val return : 'a -> 'a m
  val ( <*> ) : ('a -> 'b) m -> 'a m -> 'b m
end

module type S = sig
  include T

  val map : ('a -> 'b) -> 'a m -> 'b m
  val map2 : ('a -> 'b -> 'c) -> 'a m -> 'b m -> 'c m
  val map3 : ('a -> 'b -> 'c -> 'd) -> 'a m -> 'b m -> 'c m -> 'd m

  val map4
    :  ('a -> 'b -> 'c -> 'd -> 'e)
    -> 'a m
    -> 'b m
    -> 'c m
    -> 'd m
    -> 'e m

  val ( <$> ) : ('a -> 'b) -> 'a m -> 'b m
  val ( let$ ) : 'a m -> ('a -> 'b) -> 'b m
  val sequence : 'a m list -> 'a list m
  val map_a : ('a -> 'b m) -> 'a list -> 'b list m
  val ( <* ) : 'a m -> 'b m -> 'a m
  val ( >* ) : 'a m -> 'b m -> 'b m
  val ignore : 'a m -> unit m
  val onlyif : bool -> unit m -> unit m
  val unless : bool -> unit m -> unit m
end

module Make (A : T) = struct
  include A

  let ( <$> ) f x = return f <*> x
  let map = ( <$> )
  let ( let$ ) x f = f <$> x
  let map2 f x y = f <$> x <*> y
  let map3 f x y z = f <$> x <*> y <*> z
  let map4 f x y z w = f <$> x <*> y <*> z <*> w
  let ( <* ) x y = map2 (fun x _ -> x) x y
  let ( >* ) x y = map2 (fun _ y -> y) x y

  let rec sequence = function
    | [] -> return []
    | m :: ms -> map2 (fun x xs -> x :: xs) m (sequence ms)

  let map_a f xs = sequence (List.map f xs)
  let ignore m = map (fun _ -> ()) m
  let onlyif b m = if b then m else return ()
  let unless b m = if b then return () else m
end

module Transform (A : T) (Inner : T) = struct
  module A = Make (A)

  type 'a m = 'a Inner.m A.m

  let return x = A.return (Inner.return x)
  let ( <*> ) f x = A.map2 Inner.( <*> ) f x
end
