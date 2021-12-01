module Identity : Monad.S with type 'a m = 'a = Monad.Make (struct
  type 'a m = 'a

  let return x = x
  let bind x f = f x
end)
