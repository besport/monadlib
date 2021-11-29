module Ll = LazyList

module type BasePlus = sig
  include BatInterfaces.Monad

  val zero : unit -> 'a m
  val plus : 'a m -> 'a m -> 'a m
  val null : 'a m -> bool
end

module type BaseLazyPlus = sig
  include BatInterfaces.Monad

  val zero : unit -> 'a m
  val lplus : 'a m -> 'a m Lazy.t -> 'a m
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
  val filter_m : ('a -> bool m) -> 'a list -> 'a list m
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

module type LazyPlus = sig
  include BaseLazyPlus
  include MonadPlus with type 'a m := 'a m

  val of_llist : 'a LazyList.t -> 'a m
  val lsum : 'a LazyList.t m -> 'a m
  val lmsum : 'a m LazyList.t -> 'a m
  val ltranspose : 'a LazyList.t m -> 'a m LazyList.t
end

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

  let filter_m p =
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

module MakeLazyPlus (M : BaseLazyPlus) = struct
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

module LazyT (M : BatInterfaces.Monad) = struct
  module M = Make (M)

  include Make (struct
    type 'a m = 'a Lazy.t M.m

    let return x = M.return (lazy x)
    let bind x f = M.bind x (fun x -> f (Lazy.force x))
  end)

  let lift x = M.map (fun x -> lazy x) x
end

module List = MakePlus (struct
  type 'a m = 'a list

  let return x = [x]
  let bind xs f = BatList.concat (List.map f xs)
  let zero () = []
  let plus xs ys = xs @ ys
  let null = function [] -> true | _ -> false
end)

module ListT (M : BatInterfaces.Monad) = struct
  module M = Make (M)

  include Make (struct
    type 'a m = 'a list M.m

    let return x = M.return [x]
    let bind xs f = M.bind xs (fun x -> M.map BatList.concat (M.map_a f x))
  end)

  let lift x = M.map (fun x -> [x]) x
end

module LazyListM = struct
  include MakeLazyPlus (struct
    type 'a m = 'a LazyList.t

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

module Option = MakePlus (struct
  type 'a m = 'a option

  let return x = Some x
  let bind x f = match x with None -> None | Some y -> f y
  let zero () = None

  let plus x y =
    match x, y with None, x -> x | x, None -> x | Some x, Some _ -> Some x

  let null = BatOption.is_none
end)

module OptionT (M : BatInterfaces.Monad) = struct
  module M = Make (M)

  include Make (struct
    type 'a m = 'a option M.m

    let return x = M.return (Some x)
    let bind xs f = M.bind xs (function None -> M.return None | Some x -> f x)
  end)

  let lift x = M.map (fun x -> Some x) x
end

module Result (E : sig
  type e

  val defaultError : e
end) =
struct
  type 'a err = ('a, E.e) result

  include MakePlus (struct
    type 'a m = 'a err

    let return x = Ok x
    let bind x f = match x with Error e -> Error e | Ok x -> f x
    let zero () = Error E.defaultError
    let plus x y = match x with Error _ -> y | Ok x -> Ok x
    let null x = match x with Error _ -> true | _ -> false
  end)

  let throw e = Error e
  let catch x handler = match x with Error e -> handler e | Ok x -> return x
  let run_error err = err
end

module ResultT (E : sig
  type e

  val defaultError : e
end)
(M : BatInterfaces.Monad) =
struct
  type 'a err = ('a, E.e) result

  module M = Make (M)

  include MakePlus (struct
    type 'a m = 'a err M.m

    let return x = M.return (Ok x)

    let bind x f =
      M.bind x (function Ok x -> f x | Error e -> M.return (Error e))

    let zero () = M.return (Error E.defaultError)
    let plus x y = M.bind x (function Error _ -> y | x -> M.return x)
    let null _ = false
  end)

  let lift x = M.map (fun x -> Ok x) x
  let throw e = M.return (Error e)

  let catch x handler =
    M.bind x (function Error e -> handler e | x -> M.return x)

  let run_error err = err
end

module type Monoid = sig
  type t

  val zero : unit -> t
  val plus : t -> t -> t
end
