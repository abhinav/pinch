-- |
-- Module      :  Pinch
-- Copyright   :  (c) Abhinav Gupta 2015
-- License     :  BSD3
--
-- Maintainer  :  Abhinav Gupta <mail@abhinavg.net>
-- Stability   :  experimental
--
-- TODO
module Pinch
    (
    -- * Pinchable

      Pinchable(..)

    -- ** Writing instances

    -- | Instances of 'Pinchable' will usually be constructed by composing
    -- together existing instances and using the '.=', '.:', etc. helpers.

    -- *** Structs and exceptions
    -- $struct

    -- *** Unions
    -- $union

    -- *** Enums
    -- $enum

    -- ** Helpers

    -- *** @pinch@

    , (.=)
    , (?=)
    , struct
    , FieldPair

    -- *** @unpinch@

    , (.:)
    , (.:?)

    -- * Value

    -- | 'Value' is an intermediate representation of Thrift payloads tagged
    -- with TType tags. Types that want to be serialized into/deserialized
    -- from Thrift payloads need only define a way to convert themselves to
    -- and from 'Value' objects via 'Pinchable'.

    , Value

    -- * Protocols

    -- | Protocols define a specific way to convert values into binary and
    -- back.

    , Protocol
    , binaryProtocol


    -- * TType

    -- | TType is used to refer to the Thrift protocol-level type of a value.
    --
    -- For most basic types, it's just that type: 'TBool', 'TByte', etc. For
    -- @string@ and @binary@, it's always 'TBinary'; Thrift doesn't
    -- differentiate between text and binary at the protocol level. Enums use
    -- 'TInt32', and all structs, exceptions, and unions use 'TStruct'.

    , TType
    , IsTType(..)

    -- ** Tags

    -- | TType tags allow writing code that depends on knowing the @TType@ of
    -- values, or asserting conditions on it, at compile time.
    --
    -- For example, values in a map, list, or set must all have the same TType.
    -- This is enforced at the type level by parameterizing 'Value' over these
    -- tags.

    , TBool
    , TByte
    , TDouble
    , TInt16
    , TInt32
    , TInt64
    , TBinary
    , TStruct
    , TMap
    , TSet
    , TList
    ) where

import Pinch.Internal.TType
import Pinch.Internal.Value
import Pinch.Pinchable
import Pinch.Protocol
import Pinch.Protocol.Binary


-- $struct
--
-- Given a Thrift struct,
--
-- > struct Post {
-- >   1: optional string subject
-- >   2: required string body
-- > }
--
-- And a corresponding Haskell data type, the 'Pinchable' instance for it will
-- be,
--
-- @
-- instance 'Pinchable' Post where
--     type 'Tag' Post = 'TStruct'
--
--     pinch (Post subject body) =
--         'struct' [ 1 '?=' subject
--                , 2 '.=' body
--                ]
--
--     unpinch value =
--         Post \<$\> value '.:?' 1
--              \<*\> value '.:'  2
-- @
--

-- $union
--
-- Given a Thrift union,
--
-- > union PostBody {
-- >   1: string markdown
-- >   2: binary rtf
-- > }
--
-- And a corresponding Haskell data type, the 'Pinchable' instance for it will
-- be,
--
-- > instance Pinchable PostBody where
-- >     type Tag PostBody = TStruct
-- >
-- >     pinch (PostBodyMarkdown markdownBody) = struct [1 .= markdownBody]
-- >     pinch (PostBodyRtf rtfBody) = struct [2 .= rtfBody]
-- >
-- >     unpinch v = PostBodyMarkdown <$> v .: 1
-- >             <|> PostBodyRtf      <$> v .: 2

-- $enum
--
-- For an enum,
--
-- > enum Role {
-- >   DISABLED = 0,
-- >   USER,
-- >   ADMIN,
-- > }
--
-- And a corresponding Haskell data type, the 'Pinchable' instance for it will
-- be,
--
-- > instance Pinchable Role where
-- >     type Tag Role = TInt32
-- >
-- >     pinch RoleDisabled = pinch 0
-- >     pinch RoleUser     = pinch 1
-- >     pinch RoleAdmin    = pinch 2
-- >
-- >     unpinch v = do
-- >        value <- unpinch v
-- >        case (value :: Int32) of
-- >            0 -> Right RoleDisabled
-- >            1 -> Right RoleUser
-- >            2 -> Right RoleAdmin
-- >            _ -> Left $ "Unknown role: " ++ show value
--
