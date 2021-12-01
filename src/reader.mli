module Make (T : sig
  type t
end) : sig
  include Monad.S

  val read : T.t m
  val run : T.t -> 'a m -> 'a
end
