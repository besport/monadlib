open Monad

module Retry (E : sig
  type e
  type arg
  type tag

  val defaultError : e
end) =
struct
  type 'a err = Error of (E.tag * (E.arg -> 'a err)) list * E.e | Ok of 'a

  include MakePlus (struct
    type 'a m = 'a err

    let return x = Ok x

    let rec bind x f =
      match x with
      | Ok x -> f x
      | Error (retries, e) ->
          Error
            (BatList.map (fun (t, r) -> t, fun arg -> bind (r arg) f) retries, e)

    let zero () = Error ([], E.defaultError)
    let plus x y = match x with Error _ -> y | Ok x -> Ok x
    let null x = match x with Error _ -> true | _ -> false
  end)

  let add_retry tag retry = function
    | Error (retries, e) -> Error ((tag, retry) :: retries, e)
    | x -> x

  let throw e = Error ([], e)

  let catch x handler =
    match x with Error (_, e) -> handler e | Ok x -> return x

  let run_retry err = err
end
