module Make (E : sig
  type e

  val defaultError : e
end) : sig
  type 'a err = ('a, E.e) result

  include MonadPlus.S

  val throw : E.e -> 'a m
  val catch : 'a m -> (E.e -> 'a m) -> 'a m
  val run_error : 'a m -> 'a err
end

module Trans (E : sig
  type e

  val defaultError : e
end)
(M : Monad.S) : sig
  type 'a err = ('a, E.e) result

  include Monad.S

  val throw : E.e -> 'a m
  val catch : 'a m -> (E.e -> 'a m) -> 'a m
  val lift : 'a M.m -> 'a m
  val run_error : 'a m -> 'a err M.m
end
