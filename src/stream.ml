module Ll = LazyList

let ( ^:^ ) = Ll.( ^:^ )
let ( ^@^ ) = Ll.( ^@^ )

module type S = sig
  type 'a t

  include LazyPlus.S with type 'a m = 'a t Stdlib.Lazy.t

  val iterate : ('a m -> 'a m) -> 'a m -> 'a m
  val delay : 'a m -> 'a m
  val to_depth : int -> 'a m -> 'a m
end

module type StreamC = sig
  include S
  include Collection.T with type 'a m := 'a m
end

module Make (M : sig
  include Applicative.T
  include LazyPlus.T with type 'a m := 'a m
end) =
struct
  module ML = LazyPlus.Make (M)

  type 'a t = 'a ML.m LazyList.node_t

  let delay xs = M.zero () ^:^ xs

  module Base = struct
    type 'a m = 'a t Stdlib.Lazy.t

    let zero () = Ll.nil

    let rec lplus xss yss =
      let hd_yss =
        lazy
          (match Ll.next (Stdlib.Lazy.force yss) with
          | Ll.Nil -> ML.zero ()
          | Ll.Cons (ys, _) -> ys)
      and tl_yss =
        lazy
          (match Ll.next (Stdlib.Lazy.force yss) with
          | Ll.Nil -> Ll.nil
          | Ll.Cons (_, yss) -> yss)
      in
      lazy
        (match Ll.next xss with
        | Ll.Nil -> Ll.next (Stdlib.Lazy.force yss)
        | Ll.Cons (xs, xss) -> Ll.Cons (ML.lplus xs hd_yss, lplus xss tl_yss))

    let plus xss yss = lplus xss (lazy yss)
    let null = Ll.is_empty
    let return x = Ll.singleton (ML.return x)

    let bind xs f =
      let join xs =
        let rec shift xs =
          lazy
            (match Ll.next xs with
            | Ll.Nil -> Ll.Nil
            | Ll.Cons (x', xs') ->
                Ll.next
                  (plus (Ll.map ML.join (ML.ltranspose x')) (delay (shift xs'))))
        in
        shift xs
      in
      join (Ll.map (ML.map f) xs)

    let ( <*> ) fs xs =
      let rec shift fs =
        lazy
          (match Ll.next fs with
          | Ll.Nil -> Ll.Nil
          | Ll.Cons (f', fs') ->
              Ll.next
                (* Without the null check, we'll have a space leak as
               the empty generation is uselessly applied across xs. *)
                (if ML.null f'
                then delay (shift fs')
                else plus (Ll.map (M.( <*> ) f') xs) (delay (shift fs'))))
      in
      shift fs
  end

  include Applicative.Make (Base)
  include (LazyPlus.Make (Base) : LazyPlus.S with type 'a m := 'a m)

  let to_depth n = LazyList.take n
  let rec iterate f xs = plus xs (delay (lazy (Ll.next (iterate f (f xs)))))
end

module MakeStreamC (M : sig
  include Applicative.T
  include Collection.T with type 'a m := 'a m
end) =
struct
  include Make (M)
  module Mm = LazyPlus.Make (M)

  let nub p xs =
    let nub maxs x =
      let x' = M.nub p (M.difference p x maxs) in
      M.lplus (M.difference p maxs x') (lazy x'), x'
    in
    Ll.map_accum_l nub (M.zero ()) xs

  let maxima p xs =
    let maxima maxs x =
      let x' = M.maxima p (M.difference p x maxs) in
      M.lplus (M.difference p maxs x') (lazy x'), x'
    in
    Ll.map_accum_l maxima (M.zero ()) xs

  let unique ?(cmp = ( = )) xs =
    let unique uniques x =
      let x' = M.difference cmp (M.unique ~cmp x) uniques in
      M.lplus (M.difference cmp uniques x') (lazy x'), x'
    in
    Ll.map_accum_l unique (M.zero ()) xs

  let difference p xss (yss : 'a m) =
    Ll.zip_with (M.difference p) xss
      (Ll.drop 1 (Ll.scan Mm.plus (M.zero ()) (yss ^@^ Ll.repeat (M.zero ()))))
end
