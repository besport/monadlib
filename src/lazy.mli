module M : Monad.S with type 'a m = 'a Stdlib.Lazy.t
(** The lazy monad. Automatically wraps calls lazily and forces as needed. *)

module Trans (M : Monad.S) : sig
  include Monad.S with type 'a m = 'a Stdlib.Lazy.t M.m

  val lift : 'a M.m -> 'a m
end
