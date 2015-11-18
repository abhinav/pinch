0.2.0.0
=======

-   Breaking: `unpinch` no longer returns `Either String a`. Instead it returns
    a `Parser a`.
-   Improve deserialization performance significantly by getting rid of calls
    to `Data.Typeable.{eqT, cast}`.

0.1.0.1
=======

-   Fixed recursion in C pre-processor expansion. This can break the build on
    some systems.

0.1.0.0
=======

-   Initial release.

