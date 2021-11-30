open Monad

module M = MakePlus (struct
  type 'a m = 'a list

  let return x = [x]
  let bind xs f = BatList.concat (BatList.map f xs)
  let zero () = []
  let plus xs ys = xs @ ys
  let null = function [] -> true | _ -> false
end)

module Trans (M : BatInterfaces.Monad) = struct
  module M = Make (M)

  include Make (struct
    type 'a m = 'a list M.m

    let return x = M.return [x]
    let bind xs f = M.bind xs (fun x -> M.map BatList.concat (M.map_a f x))
  end)

  let lift x = M.map (fun x -> [x]) x
end
