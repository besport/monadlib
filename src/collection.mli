(** Monads for collections. Stream is the current use-case for this, since it
    is parameterised on a collection monad (like [list]). *)
module type T = sig
  include LazyPlus.T

  val difference : ('a -> 'a -> bool) -> 'a m -> 'a m -> 'a m
  (** [difference p xs ys] removes all elements from [xs] which are less than or
    equal to some element in [ys], according to the partial order [p]. *)

  val unique : ?cmp:('a -> 'a -> bool) -> 'a m -> 'a m
  (** [unique eq xs] removes duplicates according to the function [cmp]. *)

  val maxima : ('a -> 'a -> bool) -> 'a m -> 'a m
  (** [maxima p xs] leaves only the maximal elements according to the
  partial order [p]. *)

  val nub : ('a -> 'a -> bool) -> 'a m -> 'a m
  (** [nub p xs ys] is the same as maxima, but some values might be treated as
  greater than others depending on their position in the collection.

  For instance, with lists, we treat elements that occur earlier in the list
  as always greater than elements that occur later, otherwise we use [p]. This
  is really just an optimisation: in order to retrieve any value from [maxima p
  xs], the [maxima] function will have had to seen every value. Not so with
  [nub].*)
end

(** {1 Transformers on Collections } *)

(** Sometimes, you might want to transform a collection monad, in such a way
    that functions such as {! BaseCollectionM.unique} behave in a sensible
    way by regarding None as smaller than any Some. Each transformer provides
    a function [cmp_on] with which the collection functions such as
    [BaseCollectionM.difference] are implemented. We also provide the [list]
    function for transformers. *)

module MakeOpt (C : T) : sig
  include T with type 'a m = 'a option C.m
  include Monad.S with type 'a m := 'a m

  val cmp_on : ('a -> 'a -> bool) -> 'a option -> 'a option -> bool
  (** [cmp_on p] is a partial order satisfying:
         [None   = None]
         [None   < Some _]
         [Some x < Some y <=> x < y]
   *)

  val lift : 'a C.m -> 'a m
end
