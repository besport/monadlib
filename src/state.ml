open Monad

module Make (T : sig
  type s
end) =
struct
  include Make (struct
    type 'a m = T.s -> T.s * 'a

    let return x s = s, x

    let bind xf f s =
      let s', x = xf s in
      f x s'
  end)

  let read s = s, s
  let write x _ = x, ()
  let run x s = x s
  let eval x s = snd (x s)
  let modify f = bind read (fun s -> write (f s))
end

module Trans (T : sig
  type s
end)
(M : Monad.Monad) =
struct
  include Monad.Make (struct
    type 'a m = T.s -> (T.s * 'a) M.m

    let return x s = M.return (s, x)
    let bind xf f s = M.bind (xf s) (fun (s', x) -> (f x) s')
  end)

  let read s = M.return (s, s)
  let write x _ = M.return (x, ())
  let modify f = bind read (fun s -> write (f s))
  let run x s = x s
  let eval x s = M.map snd (x s)
  let lift x s = M.map (fun x -> s, x) x
end

module CollectionState (T : sig
  type s

  val cmp : s -> s -> bool
end)
(C : Collection.T) =
struct
  include Trans (T) (Monad.Make (C))

  let zero () _ = C.zero ()
  let lplus xs ys s = C.lplus (xs s) (lazy (Lazy.force ys s))
  let null _ = false
  let cmp_on p (s, x) (t, y) = p x y && T.cmp s t
  let difference p xs ys s = C.difference (cmp_on p) (xs s) (ys s)
  let unique ?(cmp = ( = )) xs s = C.unique ~cmp:(cmp_on cmp) (xs s)
  let maxima p xs s = C.maxima (cmp_on p) (xs s)
  let nub p xs s = C.nub (cmp_on p) (xs s)
  let read s = C.return (s, s)
  let write x _ = C.return (x, ())
end
