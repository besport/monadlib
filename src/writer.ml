open Monad

module Make (M : Monoid) = struct
  include Monad.Make (struct
    type 'a m = M.t * 'a

    let return x = M.zero (), x

    let bind (m, x) f =
      let m', y = f x in
      M.plus m m', y
  end)

  let listen (x, y) = x, (x, y)
  let run (x, y) = x, y
  let write x = x, ()
end

module Trans (Mon : Monoid) (M : BatInterfaces.Monad) = struct
  module M = Monad.Make (M)
  module W = Make (Mon)

  include Monad.Make (struct
    module WM = Monad.Make (W)

    type 'a m = 'a W.m M.m

    let return x = M.return (W.return x)

    let bind x f =
      M.bind x (fun x ->
          let v, x = W.run x in
          M.map (WM.map2 (fun () y -> y) (W.write v)) (f x))
  end)

  let listen x = M.map W.listen x
  let write x = M.return (W.write x)

  let run xs =
    M.map
      (fun x ->
        let v, x = W.run x in
        v, x)
      xs

  let lift x = M.map W.return x
end

module CollectionWriter (Mon : sig
  include Monoid

  val cmp : t -> t -> bool
end)
(C : Collection.T) =
struct
  include Trans (Mon) (C)

  let zero () = C.zero ()
  let lplus xs ys = C.lplus xs ys
  let null xs = C.null xs
  let cmp_on p (vx, x) (vy, y) = p x y && Mon.cmp vx vy
  let difference p = C.difference (cmp_on p)
  let unique ?(cmp = ( = )) = C.unique ~cmp:(cmp_on cmp)
  let maxima p = C.maxima (cmp_on p)
  let nub p = C.nub (cmp_on p)
end
