module M : MonadPlus.S with type 'a m = 'a option

module Trans (M : Monad.S) : sig
  include Monad.S with type 'a m = 'a option M.m

  val lift : 'a M.m -> 'a m
end
