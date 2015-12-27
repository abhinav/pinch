0.2.0.0
=======

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

0.1.0.2
=======

-   Loosen `vector` version constraint.

0.1.0.1
=======

-   Fixed recursion in C pre-processor expansion. This can break the build on
    some systems.

0.1.0.0
=======

-   Initial release.

