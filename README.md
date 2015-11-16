[![build-status]](https://travis-ci.org/abhinav/pinch)

`pinch` aims to provide an alternative implementation of Apache Thrift for
Haskell. The `pinch` library itself acts only as a serialization library. Types
specify their Thrift encoding by defining instances of the `Pinchable`
typeclass, which may be done by hand or automatically with the use of Generics.
Check the documentation and examples for more information.

Haddock documentation for this package is avilable on [Hackage] and [here].

  [build-status]: https://travis-ci.org/abhinav/pinch.svg?branch=master
  [Hackage]: http://hackage.haskell.org/package/pinch
  [here]: http://abhinavg.net/pinch/
