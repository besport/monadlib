module type MonadS = Monad.S

module Monad : MonadS with type 'a m = 'a Stdlib.Lazy.t
(** The lazy monad. Automatically wraps calls lazily and forces as needed. *)

module Trans (M : MonadS) : sig
  include MonadS with type 'a m = 'a Stdlib.Lazy.t M.m

  val lift : 'a M.m -> 'a m
end
