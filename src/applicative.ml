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
  val map_list : ('a -> 'b m) -> 'a list -> 'b list m
  val optional : 'a m option -> 'a option m
  val map_option : ('a -> 'b m) -> 'a option -> 'b option m
  val ( <* ) : 'a m -> 'b m -> 'a m
  val ( >* ) : 'a m -> 'b m -> 'b m
  val ignore : 'a m -> unit m
  val onlyif : bool -> unit m -> unit m
  val unless : bool -> unit m -> unit m

  module Tuple2 : sig
    val map : ('a -> 'b m) -> ('c -> 'd m) -> 'a * 'c -> ('b * 'd) m
    val map1 : ('a -> 'b m) -> 'a * 'c -> ('b * 'c) m
    val map2 : ('a -> 'b m) -> 'c * 'a -> ('c * 'b) m
  end

  module Tuple3 : sig
    val map
      :  ('a -> 'b m)
      -> ('c -> 'd m)
      -> ('e -> 'f m)
      -> 'a * 'c * 'e
      -> ('b * 'd * 'f) m

    val map1 : ('a -> 'b m) -> 'a * 'c * 'd -> ('b * 'c * 'd) m
    val map2 : ('a -> 'b m) -> 'c * 'a * 'd -> ('c * 'b * 'd) m
    val map3 : ('a -> 'b m) -> 'c * 'd * 'a -> ('c * 'd * 'b) m
  end
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

  let map_list f xs = sequence (BatList.map f xs)

  let optional = function
    | None -> return None
    | Some f -> map (fun y -> Some y) f

  let map_option f xs = optional (BatOption.map f xs)
  let ignore m = map (fun _ -> ()) m
  let onlyif b m = if b then m else return ()
  let unless b m = if b then return () else m

  open BatTuple

  module Tuple2 = struct
    let map f g (x, y) = Tuple2.make <$> f x <*> g y
    let map1 f (x, y) = (fun x -> x, y) <$> f x
    let map2 f (x, y) = Tuple2.make x <$> f y
  end

  module Tuple3 = struct
    let map f g h (x, y, z) = Tuple3.make <$> f x <*> g y <*> h z
    let map1 f (x, y, z) = (fun x -> x, y, z) <$> f x
    let map2 f (x, y, z) = (fun y -> x, y, z) <$> f y
    let map3 f (x, y, z) = (fun z -> x, y, z) <$> f z
  end
end

module Transform (A : T) (Inner : T) = struct
  module A = Make (A)

  type 'a m = 'a Inner.m A.m

  let return x = A.return (Inner.return x)
  let ( <*> ) f x = A.map2 Inner.( <*> ) f x
end
