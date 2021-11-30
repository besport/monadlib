open Monad
module M : MonadPlus with type 'a m = 'a option

module Trans (M : BatInterfaces.Monad) : sig
  include Monad with type 'a m = 'a option M.m

  val lift : 'a M.m -> 'a m
end
