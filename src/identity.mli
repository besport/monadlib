(** A trivial monad which brings into view some useful functions from
    {!Monad.S} for simple types ['a]. *)

module Monad : Monad.S with type 'a m = 'a
