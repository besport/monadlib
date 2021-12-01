module Make (M : Monad.Monoid) : sig
  include Monad.S

  val listen : 'a m -> (M.t * 'a) m
  val run : 'a m -> M.t * 'a
  val write : M.t -> unit m
end

module Trans (Mon : Monad.Monoid) (M : Monad.S) : sig
  include Monad.S

  val listen : 'a m -> (Mon.t * 'a) m
  val write : Mon.t -> unit m
  val run : 'a m -> (Mon.t * 'a) M.m
  val lift : 'a M.m -> 'a m
end

module CollectionWriter (Mon : sig
  include Monad.Monoid

  val cmp : t -> t -> bool
end)
(C : Collection.T) : sig
  include Collection.T with type 'a m = (Mon.t * 'a) C.m
  include Monad.S with type 'a m := 'a m

  val write : Mon.t -> unit m
  val run : 'a m -> (Mon.t * 'a) C.m

  val cmp_on : ('a -> 'a -> bool) -> Mon.t * 'a -> Mon.t * 'a -> bool
  (** [cmp_on p] is the usual product of the ordering on ['a] with the ordering
  on [W.t]. *)

  val lift : 'a C.m -> 'a m
end
