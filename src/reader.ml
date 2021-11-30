open Monad

module Make (M : sig
  type t
end) =
struct
  include Make (struct
    type 'a m = M.t -> 'a

    let return x _ = x
    let bind r f e = f (r e) e
  end)

  let read e = e
  let run e x = x e
end
