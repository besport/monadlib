open Monad

module Make (T : sig
  type s
end) : sig
  include Monad

  val read : T.s m
  val write : T.s -> unit m
  val modify : (T.s -> T.s) -> unit m
  val run : 'a m -> T.s -> T.s * 'a
  val eval : 'a m -> T.s -> 'a
end

module Trans (T : sig
  type s
end)
(M : Monad.Monad) : sig
  include Monad

  val read : T.s m
  val write : T.s -> unit m
  val modify : (T.s -> T.s) -> unit m
  val run : 'a m -> T.s -> (T.s * 'a) M.m
  val eval : 'a m -> T.s -> 'a M.m
  val lift : 'a M.m -> 'a m
end

module CollectionState (T : sig
  type s

  val cmp : s -> s -> bool
end)
(C : Collection.T) : sig
  include Collection.T with type 'a m = T.s -> (T.s * 'a) C.m
  include Monad with type 'a m := 'a m

  val read : T.s m
  val write : T.s -> unit m

  val cmp_on : ('a -> 'a -> bool) -> T.s * 'a -> T.s * 'a -> bool
  (** [cmp_on p] is the usual product of the ordering on ['a] with the ordering
  on [T.s]. *)

  val lift : 'a C.m -> 'a m
end
