open Monad

(** For the incorruptible programmer:

    A continuation of type 'a has type [('a -> 'r) -> 'r]

    The first argument to this continuation is a final value of type 'r which
    depends on some intermediate value of type 'a. In other words, we can
    understand the continuation as a result of type 'r which depends on the {i
    future} of an intermediate value.

    The function [return] just {i throws} its return value to the future in
    order to produce the final result. The [bind] intercedes in the future,
    inserting a computation.

    Call-with-current-continuation allows one to effectively reflect into one's own
    future. That is, callCC is a computation which depends on another computation
    taking the future as a first-class value. One can store this future, and at any
    time, throw it a return value to reinstate it.

    If you are into the Curry-Howard Isomorphism,
    call-with-current-continuation has a type which corresponds to a law of
    classical logic (Pierce's Law). Writing your code in the continuation
    monad corresponds to embedding classical logic intuitionistically. Allowing
    callCC corresponds to assuming a classical hypothesis.
 *)
module Continuation (T : sig
  type r
end) : sig
  include Monad with type 'a m = ('a -> T.r) -> T.r

  val callCC : (('a -> 'r m) -> 'a m) -> 'a m
end
