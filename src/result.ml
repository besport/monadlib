open Monad

module Make (E : sig
  type e

  val defaultError : e
end) =
struct
  type 'a err = ('a, E.e) result

  include MakePlus (struct
    type 'a m = 'a err

    let return x = Ok x
    let bind x f = match x with Error e -> Error e | Ok x -> f x
    let zero () = Error E.defaultError
    let plus x y = match x with Error _ -> y | Ok x -> Ok x
    let null x = match x with Error _ -> true | _ -> false
  end)

  let throw e = Error e
  let catch x handler = match x with Error e -> handler e | Ok x -> return x
  let run_error err = err
end

module Trans (E : sig
  type e

  val defaultError : e
end)
(M : Monad) =
struct
  type 'a err = ('a, E.e) result

  include MakePlus (struct
    type 'a m = 'a err M.m

    let return x = M.return (Ok x)

    let bind x f =
      M.bind x (function Ok x -> f x | Error e -> M.return (Error e))

    let zero () = M.return (Error E.defaultError)
    let plus x y = M.bind x (function Error _ -> y | x -> M.return x)
    let null _ = false
  end)

  let lift x = M.map (fun x -> Ok x) x
  let throw e = M.return (Error e)

  let catch x handler =
    M.bind x (function Error e -> handler e | x -> M.return x)

  let run_error err = err
end
