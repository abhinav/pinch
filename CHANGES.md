0.4.1.1 (2021-12-04)
=====================

- Allow semigroups 0.20

0.4.1.0 (2021-11-14)
====================

-   Support GHC 9.2. (#45)

0.4.0.0 (2021-01-14)
====================

Breaking changes:

-   The protocol parsers now use the cereal library and the Pinch.Protocol.Parser module has been removed.
    Custom protocol implementations need to be ported to the cereal parsers. (#28)
-   Use new header format for binary protocol. This fixes some compatibility issues with thrift libraries
    for other languages. (#33)

Other changes:

-   Add implementations of the framed/unframed transports (`Pinch.Transport`, #29).
-   Add Thrift RPC client/server functionality (`Pinch.Server`/`Pinch.Client`).
-   A new code generator, `pinch-gen`, is available. It can generate code from Thrift files for use
    with the pinch library. For more details, see https://github.com/phile314/pinch-gen/.

0.3.5.1 (2020-12-29)
====================

-   Compatibility with GHC 8.10

0.3.5.0 (2019-09-25)
====================

-   Introduce decodeWithLeftovers for receiving unparsed portions
    of the provided bytestring.

0.3.4.1 (2019-02-20)
====================

-   Bounds bumps

0.3.4.0 (2018-11-11)
====================

-   Support GHC 8.6 (#20).

0.3.3.0 (2018-06-15)
====================

-   Add Semigroup instances for some internal types. This improves GHC 8.4
    compatibility.


0.3.2.0 (2017-06-03)
====================

-   Compact: Fixed bug which caused incorrect encoding of doubles.


0.3.1.0 (2017-05-13)
====================

-   Support GHC 8.2 (#14).


0.3.0.2 (2017-01-12)
====================

-   Bump upper bound for vector (#11).


0.3.0.1 (2016-07-12)
====================

-   Compile on 32-bit systems.

0.3.0.0 (2016-06-02)
====================

-   Add support for the Thrift Compact Protocol (#2).
-   Add support for returning the leftover ByteString when parsing Thrift
    payloads (#3).

0.2.0.2 (2016-07-12)
====================

-   Compile on 32-bit systems.

0.2.0.1 (2016-05-23)
====================

-   Build with GHC 8.

0.2.0.0 (2015-12-27)
====================

Breaking changes:

-   `unpinch` no longer returns `Either String a`. Instead it returns a
    `Parser a`.
-   `Protocol.serialize*` methods no longer produce a `ByteString.Builder` and
    the serialized length. Instead, they produce a custom `Builder` type.

Other changes:

-   Improve deserialization performance significantly by getting rid of
    unnecessary calls to `Data.Typeable.{eqT, cast}`.
-   Improve serialization performance by allocating the output buffer in one go
    rather than using `ByteString.Builder`.
-   Improve serialization and deserialization performance further by changing
    the intermediate representation of lists, sets, and maps.

0.1.0.2 (2015-12-27)
====================

-   Loosen `vector` version constraint.

0.1.0.1 (2015-11-15)
====================

-   Fixed recursion in C pre-processor expansion. This can break the build on
    some systems.

0.1.0.0 (2015-11-15)
====================

-   Initial release.
