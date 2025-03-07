module Trans (M : Monad.S) = struct
  include Monad.Make (struct
    type 'a m = 'a option M.m

    let return x = M.return (Some x)
    let bind xs f = M.bind xs (function None -> M.return None | Some x -> f x)
  end)

  let lift x = M.map (fun x -> Some x) x
end

module TransPlus (M : MonadPlus.S) = struct
  include MonadPlus.Make (struct
    type 'a m = 'a option M.m

    let return x = M.return (Some x)
    let bind xs f = M.bind xs (function None -> M.return None | Some x -> f x)
    let zero = M.zero
    let plus = M.plus
    let null = M.null
  end)

  let lift x = M.map (fun x -> Some x) x
end

module Monad = MonadPlus.Make (struct
  include BatOption.Monad

  let zero () = None

  let plus x y =
    match x, y with None, x -> x | x, None -> x | Some x, Some _ -> Some x

  let null = BatOption.is_none
end)
