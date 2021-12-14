monadlib
========

Description
-----------

A starter monad library in Ocaml. Monad library functions are generated from implementations of BatInterfaces.Monad. Generated documentation can be found at: https://besport.github.io/monadlib/monadlib/index.html

We support monads with the additional "plus" structure, and provide a lazy variant useful for monads such as LazyList.
Applicative functors (« functor » not in the OCaml-sense of parametrised module) are provided.

The lazy lists here are lazier than those of BatLazyList. If you're writing really recursive lazy lists, you'll probably find you need these versions of the functions. Simple example: as of writing, concat in BatLazyList is implemented as a left fold with append. This makes it strict in the outer list, which isn't always what you want.

There are some usual suspect monads in the library. Basic monads are provided, as well as transformers for many of those monads. Warning: transformer code is verbose in Ocaml compared with Haskell.

Included is an implementation of Spivey's breadth-first search monad based on an abstract inner "bag" collection. Use lazy lists for a simple implementation. For a more complex example useful in, say, theorem proving, use TagTree. (See http://link.springer.com/chapter/10.1007%2F978-3-642-31374-5_25)

Installation
------------

    $ dune build @all
    $ dune install

For documentation:

    $ dune build @doc
