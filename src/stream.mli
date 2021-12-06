(** Streams supporting fair and unbounded search.

    Spivey has a nice and authoratitive paper on this, understanding streams as
    a breadth-first search. If that description appeals to you, I recommend his
    paper. Personally, I found it helpful to understand streams a bit
    differently, as concurrent processes that go off and discover data.

    First, we have {i generations}, which are collections of zero or more
    data. The data in each generation is discovered simultaneously, and
    therefore, it doesn't make much sense to have any ordering imposed on the
    generations. In fact, the lack of an ordering is essential for getting the
    associativity laws of the monad, and is why Spivey's paper works with bags.

    Now a stream is just a lazy list of generations. We want to think of the
    indices into these streams as temporal indices. So the first generation in
    the lazy list was discovered at time 0. The second was discovered at time
    1. The third was discovered at time 2. And so on.

    Thinking this way helped me figure out [return] and [join]. Here,
    [return x] gives you the process which immediately discovers [x] and then
    terminates.

    With [join xss], we should interpret the [xss] as a process which discovers
    {i other processes}. When we join them, we ask the outer process to fork
    each inner process as soon as it is discovered, and then merge in all the
    values found by the forked processes. For instance, if [xs] discovers a
    process [p] at time 5, and in turn, [p] discovers the string "hello world!"
    at time 13, then [join xs] discovers "hello world!" at time 5 + 13 = 18.

    Now we can understand [bind], realising that it is just the result of doing
    a [map] and then a [join]. The expression [bind xs f] forks processes [f]
    which depend on a discovered value [x], and then merges back all their values.

    Technically, all our streams should be infinite, and the processes should
    run forever, but this gave me sad performance in my theorem proving
    code. So for efficiency, our streams can be truncated, which represents a
    process that terminates.

    Note that it is impossible to define a general {! T.null} predicate
    for streams, because we would have to be able to decide whether an infinite
    stream of values consists entirely of the empty generation. Turing says that's
    impossible, and I believe him. As a crude approximation, then, we have it that {!
    T.null} returns [true] just for the special case that its input is an
    empty lazy list. *)
module type S = sig
  type 'a t

  include LazyPlus.S with type 'a m = 'a t Stdlib.Lazy.t

  val iterate : ('a m -> 'a m) -> 'a m -> 'a m
  (** The sum of the stream [\[f x, f (f x), f (f (f x)),...\]] *)

  val delay : 'a m -> 'a m
  (** Delay a stream by one time step. This is needed when you write recursive
  streams and you have to avoid deadlock. The nice thing about Ocaml here is that
  it will generally detect deadlock for you, announcing to you that you're
  writing viciously circular lists! *)

  val to_depth : int -> 'a m -> 'a m
  (** Terminate discovery at some depth. *)
end

(** The union of streams and collections. *)
module type StreamC = sig
  include S
  include Collection.T with type 'a m := 'a m
end

(** Create a stream monad from an arbitrary inner monad, which computes the
    values discovered in each generation. This is pretty abstract, since we're
    not requiring that the inner monad is a {i collection}. There is a
    constraint here, since we're strictly supposed to disallow a monad
    collection where order of elements matters. I think we can characterise
    this abstractly by saying that the plus operation on the inner monad must
    be commutative, but don't take my word for it!
 *)
module Make (M : sig
  include Applicative.T
  include LazyPlus.T with type 'a m := 'a m
end) : sig
  include
    S with type 'a t = 'a M.m LazyList.node_t and type 'a m = 'a M.m LazyList.t

  include Monad.S with type 'a m := 'a m
end

(** Here, we create a stream monad from a definite collection monad
    {! Collection.T}. The inner monad will be used to represent the
    generations in the stream. The order of elements in each generation should
    not matter, so you might want to use a set or a bag. If you want to live
    life on the edge, just remember that your code should not depend on the
    order of elements within generations (you can, of course, depend on the
    order that generations appear in the stream). You can enforce this
    constraint by performing, say, a sort on each generation.  *)
module MakeStreamC (M : sig
  include Applicative.T
  include Collection.T with type 'a m := 'a m
end) : sig
  include
    StreamC
      with type 'a t = 'a M.m LazyList.node_t
       and type 'a m = 'a M.m LazyList.t
end
