open Monad

module Continuation (T : sig
  type r
end) =
struct
  include Make (struct
    type 'a m = ('a -> T.r) -> T.r

    let return x k = k x
    let bind c f k = c (fun x -> (f x) k)
  end)

  let callCC kk k = kk (fun x _ -> k x) k
end
