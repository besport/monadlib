open Monad

module Reader (T : sig
  type t
end) : sig
  include Monad

  val read : T.t m
  val run : T.t -> 'a m -> 'a
end
