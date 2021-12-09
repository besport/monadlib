module Trans (M : Monad.S) : sig
  include Monad.S with type 'a m = 'a option M.m

  val lift : 'a M.m -> 'a m
end

module TransPlus (M : MonadPlus.S) : sig
  include MonadPlus.S with type 'a m = 'a option M.m

  val lift : 'a M.m -> 'a m
end

module Monad : MonadPlus.S with type 'a m = 'a option
