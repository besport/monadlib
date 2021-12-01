module type T = sig
  type t

  val zero : unit -> t
  val plus : t -> t -> t
end
