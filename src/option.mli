module type MonadS = Monad.S

module Monad : MonadPlus.S with type 'a m = 'a option

module Trans (M : MonadS) : sig
  include MonadS with type 'a m = 'a option M.m

  val lift : 'a M.m -> 'a m
end
