module Make (E : sig
  type e
  type arg
  type tag

  val defaultError : e
end) : sig
  include MonadPlus.S

  type 'a err = Error of (E.tag * (E.arg -> 'a m)) list * E.e | Ok of 'a

  val throw : E.e -> 'a m
  val catch : 'a m -> (E.e -> 'a m) -> 'a m
  val add_retry : E.tag -> (E.arg -> 'a m) -> 'a m -> 'a m
  val run_retry : 'a m -> 'a err
end
