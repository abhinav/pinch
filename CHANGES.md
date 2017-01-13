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

