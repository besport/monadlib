module type T = sig
  type 'a m

  val return : 'a -> 'a m
  val ( <*> ) : ('a -> 'b) m -> 'a m -> 'b m
end

module type S = sig
  include T

  val map : ('a -> 'b) -> 'a m -> 'b m
  val fmap : ('a -> 'b) m -> 'a -> 'b m
  val iter : ('a -> unit) -> 'a m -> unit
  val fiter : ('a -> unit) m -> 'a -> unit

  module Infix : sig
    val ( <@> ) : ('a -> 'b) -> 'a m -> 'b m
    val ( <*> ) : ('a -> 'b) m -> 'a m -> 'b m
    val ( <* ) : 'a m -> 'b m -> 'a m
    val ( *> ) : 'a m -> 'b m -> 'b m
  end

  include module type of Infix

  module Bindings : sig
    val ( let+ ) : 'a m -> ('a -> 'b) -> 'b m
    val ( and+ ) : 'a m -> 'b m -> ('a * 'b) m
  end

  include module type of Bindings

  val ignore : 'a m -> unit m
  val map2 : ('a -> 'b -> 'c) -> 'a m -> 'b m -> 'c m
  val map3 : ('a -> 'b -> 'c -> 'd) -> 'a m -> 'b m -> 'c m -> 'd m

  val map4
    :  ('a -> 'b -> 'c -> 'd -> 'e)
    -> 'a m
    -> 'b m
    -> 'c m
    -> 'd m
    -> 'e m

  val sequence : (unit -> 'a m) list -> 'a list m
  val sequence_unit : (unit -> unit m) list -> unit m
  val list_map : ('a -> 'b m) -> 'a list -> 'b list m
  val list_iter : ('a -> unit m) -> 'a list -> unit m
  val list_filter : ('a -> bool m) -> 'a list -> 'a list m
  val list_filter_map : ('a -> 'b option m) -> 'a list -> 'b list m
  val option_map : ('a -> 'b m) -> 'a option -> 'b option m
  val optionally : ('a -> unit m) -> 'a option -> unit m
  val optional : bool -> (unit -> 'a m) -> 'a option m
  val conditional : bool -> (unit -> unit m) -> unit m

  module Tuple2 : sig
    val split : ('a * 'b) m -> 'a m * 'b m
    val make1 : 'a m -> 'b -> ('a * 'b) m
    val make2 : 'a -> 'b m -> ('a * 'b) m
    val map1 : ('a -> 'c) -> ('a * 'b) m -> ('c * 'b) m
    val map2 : ('b -> 'c) -> ('a * 'b) m -> ('a * 'c) m
    val curry1 : ('a * 'b -> 'c) -> 'a m -> 'b -> 'c m
    val curry2 : ('a * 'b -> 'c) -> 'a -> 'b m -> 'c m
    val uncurry : ('a -> 'b -> 'c) -> ('a * 'b) m -> 'c m
  end

  module Tuple3 : sig
    val split : ('a * 'b * 'c) m -> 'a m * 'b m * 'c m
    val make1 : 'a m -> 'b -> 'c -> ('a * 'b * 'c) m
    val make2 : 'a -> 'b m -> 'c -> ('a * 'b * 'c) m
    val make3 : 'a -> 'b -> 'c m -> ('a * 'b * 'c) m
    val map1 : ('a -> 'd) -> ('a * 'b * 'c) m -> ('d * 'b * 'c) m
    val map2 : ('b -> 'd) -> ('a * 'b * 'c) m -> ('a * 'd * 'c) m
    val map3 : ('c -> 'd) -> ('a * 'b * 'c) m -> ('a * 'b * 'd) m
    val curry1 : ('a * 'b * 'c -> 'd) -> 'a m -> 'b -> 'c -> 'd m
    val curry2 : ('a * 'b * 'c -> 'd) -> 'a -> 'b m -> 'c -> 'd m
    val curry3 : ('a * 'b * 'c -> 'd) -> 'a -> 'b -> 'c m -> 'd m
    val uncurry : ('a -> 'b -> 'c -> 'd) -> ('a * 'b * 'c) m -> 'd m
  end

  module Tuple4 : sig
    val split : ('a * 'b * 'c * 'd) m -> 'a m * 'b m * 'c m * 'd m
    val make1 : 'a m -> 'b -> 'c -> 'd -> ('a * 'b * 'c * 'd) m
    val make2 : 'a -> 'b m -> 'c -> 'd -> ('a * 'b * 'c * 'd) m
    val make3 : 'a -> 'b -> 'c m -> 'd -> ('a * 'b * 'c * 'd) m
    val make4 : 'a -> 'b -> 'c -> 'd m -> ('a * 'b * 'c * 'd) m
    val map1 : ('a -> 'e) -> ('a * 'b * 'c * 'd) m -> ('e * 'b * 'c * 'd) m
    val map2 : ('b -> 'e) -> ('a * 'b * 'c * 'd) m -> ('a * 'e * 'c * 'd) m
    val map3 : ('c -> 'e) -> ('a * 'b * 'c * 'd) m -> ('a * 'b * 'e * 'd) m
    val map4 : ('d -> 'e) -> ('a * 'b * 'c * 'd) m -> ('a * 'b * 'c * 'e) m
    val curry1 : ('a * 'b * 'c * 'd -> 'e) -> 'a m -> 'b -> 'c -> 'd -> 'e m
    val curry2 : ('a * 'b * 'c * 'd -> 'e) -> 'a -> 'b m -> 'c -> 'd -> 'e m
    val curry3 : ('a * 'b * 'c * 'd -> 'e) -> 'a -> 'b -> 'c m -> 'd -> 'e m
    val curry4 : ('a * 'b * 'c * 'd -> 'e) -> 'a -> 'b -> 'c -> 'd m -> 'e m
    val uncurry : ('a -> 'b -> 'c -> 'd -> 'e) -> ('a * 'b * 'c * 'd) m -> 'e m
  end

  module Tuple5 : sig
    val split : ('a * 'b * 'c * 'd * 'e) m -> 'a m * 'b m * 'c m * 'd m * 'e m
    val make1 : 'a m -> 'b -> 'c -> 'd -> 'e -> ('a * 'b * 'c * 'd * 'e) m
    val make2 : 'a -> 'b m -> 'c -> 'd -> 'e -> ('a * 'b * 'c * 'd * 'e) m
    val make3 : 'a -> 'b -> 'c m -> 'd -> 'e -> ('a * 'b * 'c * 'd * 'e) m
    val make4 : 'a -> 'b -> 'c -> 'd m -> 'e -> ('a * 'b * 'c * 'd * 'e) m
    val make5 : 'a -> 'b -> 'c -> 'd -> 'e m -> ('a * 'b * 'c * 'd * 'e) m

    val map1
      :  ('a -> 'f)
      -> ('a * 'b * 'c * 'd * 'e) m
      -> ('f * 'b * 'c * 'd * 'e) m

    val map2
      :  ('b -> 'f)
      -> ('a * 'b * 'c * 'd * 'e) m
      -> ('a * 'f * 'c * 'd * 'e) m

    val map3
      :  ('c -> 'f)
      -> ('a * 'b * 'c * 'd * 'e) m
      -> ('a * 'b * 'f * 'd * 'e) m

    val map4
      :  ('d -> 'f)
      -> ('a * 'b * 'c * 'd * 'e) m
      -> ('a * 'b * 'c * 'f * 'e) m

    val map5
      :  ('e -> 'f)
      -> ('a * 'b * 'c * 'd * 'e) m
      -> ('a * 'b * 'c * 'd * 'f) m

    val curry1
      :  ('a * 'b * 'c * 'd * 'e -> 'f)
      -> 'a m
      -> 'b
      -> 'c
      -> 'd
      -> 'e
      -> 'f m

    val curry2
      :  ('a * 'b * 'c * 'd * 'e -> 'f)
      -> 'a
      -> 'b m
      -> 'c
      -> 'd
      -> 'e
      -> 'f m

    val curry3
      :  ('a * 'b * 'c * 'd * 'e -> 'f)
      -> 'a
      -> 'b
      -> 'c m
      -> 'd
      -> 'e
      -> 'f m

    val curry4
      :  ('a * 'b * 'c * 'd * 'e -> 'f)
      -> 'a
      -> 'b
      -> 'c
      -> 'd m
      -> 'e
      -> 'f m

    val curry5
      :  ('a * 'b * 'c * 'd * 'e -> 'f)
      -> 'a
      -> 'b
      -> 'c
      -> 'd
      -> 'e m
      -> 'f m

    val uncurry
      :  ('a -> 'b -> 'c -> 'd -> 'e -> 'f)
      -> ('a * 'b * 'c * 'd * 'e) m
      -> 'f m
  end
end

module Make (A : T) : S with type 'a m = 'a A.m = struct
  include A

  let map f x = return f <*> x

  module Infix = struct
    let ( <@> ) = map
    let ( <*> ) = A.( <*> )
    let ( <* ) x y = BatPervasives.const <@> x <*> y
    let ( *> ) x y = map (fun _ y -> y) x <*> y
  end

  include Infix

  let iter f x = ignore @@ map f x
  let fmap f x = map (fun f -> f x) f
  let fiter f x = ignore @@ fmap f x

  module Bindings = struct
    let ( let+ ) x f = f <@> x
    let ( and+ ) x y = BatTuple.Tuple2.make <@> x <*> y
  end

  include Bindings

  let map2 f x y = f <@> x <*> y
  let map3 f x y z = f <@> x <*> y <*> z
  let map4 f x y z w = f <@> x <*> y <*> z <*> w
  let ignore m = map (fun _ -> ()) m

  let rec sequence = function
    | [] -> return []
    | h :: t -> BatList.cons <@> h () <*> sequence t

  let rec sequence_unit = function
    | [] -> return ()
    | m :: ms -> m () *> sequence_unit ms

  let rec list_map f = function
    | [] -> return []
    | h :: t -> BatList.cons <@> f h <*> list_map f t

  let rec list_iter f = function
    | [] -> return ()
    | h :: t -> f h *> list_iter f t

  let rec list_filter p = function
    | [] -> return []
    | h :: t ->
        let cons = function
          | false -> BatPervasives.identity
          | true -> BatList.cons h
        in
        cons <@> p h <*> list_filter p t

  let rec list_filter_map f = function
    | [] -> return []
    | h :: t ->
        let cons = function
          | None -> BatPervasives.identity
          | Some x -> BatList.cons x
        in
        cons <@> f h <*> list_filter_map f t

  let optionally f = function None -> return () | Some x -> f x

  let option_map f = function
    | None -> return None
    | Some x -> map (fun y -> Some y) (f x)

  let conditional b f = if b then f () else return ()
  let optional b f = if b then BatOption.some <@> f () else return None

  module Tuple2 = struct
    module Tuple2 = BatTuple.Tuple2

    let split m = map fst m, map snd m
    let make1 m b = map (fun a -> Tuple2.make a b) m
    let make2 a m = map (fun b -> Tuple2.make a b) m
    let map1 f = map (Tuple2.map1 f)
    let map2 f = map (Tuple2.map2 f)
    let curry1 f m b = map f (make1 m b)
    let curry2 f a m = map f (make2 a m)
    let uncurry f = map (Tuple2.uncurry f)
  end

  module Tuple3 = struct
    module Tuple3 = BatTuple.Tuple3

    let split m = map Tuple3.first m, map Tuple3.second m, map Tuple3.third m
    let make1 m b c = map (fun a -> Tuple3.make a b c) m
    let make2 a m c = map (fun b -> Tuple3.make a b c) m
    let make3 a b m = map (fun c -> Tuple3.make a b c) m
    let map1 f = map (Tuple3.map1 f)
    let map2 f = map (Tuple3.map2 f)
    let map3 f = map (Tuple3.map3 f)
    let curry1 f m b c = map f (make1 m b c)
    let curry2 f a m c = map f (make2 a m c)
    let curry3 f a b m = map f (make3 a b m)
    let uncurry f = map (Tuple3.uncurry f)
  end

  module Tuple4 = struct
    module Tuple4 = BatTuple.Tuple4

    let split m =
      ( map Tuple4.first m
      , map Tuple4.second m
      , map Tuple4.third m
      , map Tuple4.fourth m )

    let make1 m b c d = map (fun a -> Tuple4.make a b c d) m
    let make2 a m c d = map (fun b -> Tuple4.make a b c d) m
    let make3 a b m d = map (fun c -> Tuple4.make a b c d) m
    let make4 a b c m = map (fun d -> Tuple4.make a b c d) m
    let map1 f = map (Tuple4.map1 f)
    let map2 f = map (Tuple4.map2 f)
    let map3 f = map (Tuple4.map3 f)
    let map4 f = map (Tuple4.map4 f)
    let curry1 f m b c d = map f (make1 m b c d)
    let curry2 f a m c d = map f (make2 a m c d)
    let curry3 f a b m d = map f (make3 a b m d)
    let curry4 f a b c m = map f (make4 a b c m)
    let uncurry f = map (Tuple4.uncurry f)
  end

  module Tuple5 = struct
    module Tuple5 = BatTuple.Tuple5

    let split m =
      ( map Tuple5.first m
      , map Tuple5.second m
      , map Tuple5.third m
      , map Tuple5.fourth m
      , map Tuple5.fifth m )

    let make1 m b c d e = map (fun a -> Tuple5.make a b c d e) m
    let make2 a m c d e = map (fun b -> Tuple5.make a b c d e) m
    let make3 a b m d e = map (fun c -> Tuple5.make a b c d e) m
    let make4 a b c m e = map (fun d -> Tuple5.make a b c d e) m
    let make5 a b c d m = map (fun e -> Tuple5.make a b c d e) m
    let map1 f = map (Tuple5.map1 f)
    let map2 f = map (Tuple5.map2 f)
    let map3 f = map (Tuple5.map3 f)
    let map4 f = map (Tuple5.map4 f)
    let map5 f = map (Tuple5.map5 f)
    let curry1 f m b c d e = map f (make1 m b c d e)
    let curry2 f a m c d e = map f (make2 a m c d e)
    let curry3 f a b m d e = map f (make3 a b m d e)
    let curry4 f a b c m e = map f (make4 a b c m e)
    let curry5 f a b c d m = map f (make5 a b c d m)
    let uncurry f = map (Tuple5.uncurry f)
  end
end

module Trans (A : S) (Inner : S) : S with type 'a m = 'a Inner.m A.m =
Make (struct
  module A = Make (A)

  type 'a m = 'a Inner.m A.m

  let return x = A.return (Inner.return x)
  let ( <*> ) f x = A.map2 Inner.( <*> ) f x
end)
