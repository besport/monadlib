open OUnit

module type EquivMonad = sig
  include BatMonad.S

  val equiv : 'a m -> 'a m -> bool
end

module type EquivPlus = sig
  include Monad.BasePlus

  val equiv : 'a m -> 'a m -> bool
end

module BuildMonadTests (M : EquivMonad) = struct
  module B = Monad.Make (M)

  let law1 x = M.equiv (B.map (fun x -> x) x) x
  let law2 f g x = M.equiv (B.map f (B.map g x)) (B.map (fun x -> f (g x)) x)
  let law3 f x = M.equiv (B.map f (B.return x)) (B.return (f x))
  let law4 f x = M.equiv (B.map f (B.join x)) (B.join (B.map (B.map f) x))
  let law5 x = M.equiv (B.join (B.return x)) x
  let law6 x = M.equiv (B.join (B.map B.return x)) x
  let law7 x = M.equiv (B.join (B.join x)) (B.join (B.map B.join x))
end

module BuildPlusTests (M : EquivPlus) = struct
  module B = Monad.MakePlus (M)

  let law1 x = M.equiv (B.plus (B.zero ()) x) x
  let law2 x = M.equiv x (B.plus (B.zero ()) x)
end
