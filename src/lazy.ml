module M = Monad.Make (struct
  type 'a m = 'a Stdlib.Lazy.t

  let return x = lazy x
  let bind x f = lazy (Stdlib.Lazy.force (f (Stdlib.Lazy.force x)))
end)

module Trans (M : Monad.S) = struct
  include Monad.Make (struct
    type 'a m = 'a Stdlib.Lazy.t M.m

    let return x = M.return (lazy x)
    let bind x f = M.bind x (fun x -> f (Stdlib.Lazy.force x))
  end)

  let lift x = M.map (fun x -> lazy x) x
end
