
[![build-status]](https://travis-ci.org/abhinav/pinch)

`pinch` aims to provide an alternative implementation of Apache Thrift for
Haskell. The `pinch` library itself acts only as a serialization library. Types
specify their Thrift encoding by defining instances of the `Pinchable`
typeclass, which may be done by hand or automatically with the use of Generics.

  [build-status]: https://travis-ci.org/abhinav/pinch.svg?branch=master

Haddock documentation for this package is avilable on [Hackage] and [here].

  [Hackage]: http://hackage.haskell.org/package/pinch
  [here]: http://abhinavg.net/pinch/

Overview
--------

Types which can be encoded into Thrift payloads implement the `Pinchable`
typeclass.

Given the Thrift struct,

```thrift
struct Person {
    1: required string name
    2: optional i64 dateOfBirth
}
```

You can write a `Pinchable` instance like so,

```haskell
data Person = Person { name :: Text, dateOfBirth :: Maybe Int64 }
    deriving (Eq)

instance Pinchable Person where
    type Tag Person = TStruct
    -- The Tag tells the system that this represents a struct.

    pinch (Person name dateOfBirth) =
        struct [1 .= name, 2 ?= dateOfBirth]

    unpinch value =
        Person <$> value .:  1
               <*> value .:? 2
```

Better yet, you can drive an instance automatically.

```haskell
{-# LANGUAGE DeriveGeneric, DataKinds #-}
import GHC.Generics (Generic)

data Person = Person
    { name        :: Field 1 Text
    , dateOfBirth :: Field 2 (Maybe Int64)
    } deriving (Eq, Generic)

instance Pinchable Person
```

Objects can be serialized and deserialized using the `encode` and `decode`
methods. These methods accept a `Protocol` as an argument.

```haskell
decode binaryProtocol (encode binaryProtocol person) == person
```

For more information, check the documentation and the examples.

Caveats
-------

-   Only the Thrift Binary Protocol is supported right now. (Pull
    requests welcome.)
-   There is no code generation or template haskell support yet so types from
    the Thrift file will have to be translated by hand.

