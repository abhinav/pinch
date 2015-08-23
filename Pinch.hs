-- |
-- Module      :  Pinch
-- Copyright   :  (c) Abhinav Gupta 2015
-- License     :  BSD3
--
-- Maintainer  :  Abhinav Gupta <mail@abhinavg.net>
-- Stability   :  experimental
--
-- Pinch defines the machinery to specify how types can be encoded into or
-- decoded from Thrift values. Types that can be serialized and deserialized
-- into/from Thrift values implement the 'Pinchable' typeclass. The
-- 'Pinchable' typeclass converts objects into and from 'Value' objects, which
-- map directly to the over-the-wire representation of Thrift values.
-- A 'Protocol' is responsible for converting 'Value' objects to and from
-- raw bytestrings.
--
-- > +------------+   Pinchable   +------------+   Protocol    +------------+
-- > |            |               |            |               |            |
-- > |            +----pinch------>            +---serialize--->            |
-- > | Your Type  |               |  Value a   |               | ByteString |
-- > |            <---unpinch-----+            <--deserialize--+            |
-- > |            |               |            |               |            |
-- > +------------+               +------------+               +------------+
module Pinch
    (

      encode
    , decode

    -- * Automatically derived instances

    -- | Pinch supports deriving instances of 'Pinchable' automatically for
    -- types that implement the @Generic@ typeclass provided that they follow
    -- the outlined patterns in their constructors.

    -- ** Structs and exceptions
    -- $genericStruct

    -- ** Unions
    -- $genericUnion

    , Field(..)
    , getField
    , putField
    , field

    -- ** Enums
    -- $genericEnum

    , Enumeration(..)
    , enum

    -- * Manually writing instances

    -- | Instances of 'Pinchable' can be constructed by composing together
    -- existing instances and using the '.=', '.:', etc. helpers.

    -- ** Structs and exceptions
    -- $struct

    -- ** Unions
    -- $union

    -- ** Enums
    -- $enum

    -- * Pinchable

    , Pinchable(..)

    -- ** Helpers

    -- *** @pinch@

    , (.=)
    , (?=)
    , struct
    , union
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
    , SomeValue(..)

    -- * Message

    , Message(..)
    , MessageType(..)

    -- * Protocols

    -- | Protocols define a specific way to convert values into binary and
    -- back.

    , Protocol(..)
    , binaryProtocol


    -- * TType

    -- | TType is used to refer to the Thrift protocol-level type of a value.

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
    , TEnum
    , TInt16
    , TInt32
    , TInt64
    , TBinary
    , TStruct
    , TUnion
    , TException
    , TMap
    , TSet
    , TList
    ) where

import Control.Monad
import Data.ByteString      (ByteString)
import Data.ByteString.Lazy (toStrict)

import qualified Data.ByteString.Builder as BB

import Pinch.Generic
import Pinch.Internal.Message
import Pinch.Internal.Pinchable
import Pinch.Internal.TType
import Pinch.Internal.Value
import Pinch.Protocol
import Pinch.Protocol.Binary

-- | Encode the given 'Pinchable' value using the given 'Protocol'.
--
-- >>> unpack $ encode binaryProtocol ["a" :: ByteString, "b"]
-- [11,0,0,0,2,0,0,0,1,97,0,0,0,1,98]
--
encode :: Pinchable a => Protocol -> a -> ByteString
encode p = toStrict . BB.toLazyByteString . serializeValue p . pinch

-- | Decode a 'Pinchable' value from the using the given 'Protocol'.
--
-- >>> let s = pack [11,0,0,0,2,0,0,0,1,97,0,0,0,1,98]
-- >>> decode binaryProtocol s :: Either String [ByteString]
-- Right ["a","b"]
--
decode :: Pinchable a => Protocol -> ByteString -> Either String a
decode p = deserializeValue p >=> unpinch

-- $struct
--
-- Given a Thrift struct,
--
-- > struct Post {
-- >   1: optional string subject
-- >   2: required string body
-- > }
--
-- The 'Pinchable' instance for it will be,
--
-- @
-- data Post = Post
--     { postSubject :: Maybe Text
--     , postBody    :: Text
--     }
--
-- instance 'Pinchable' Post where
--     type Tag Post = TStruct
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
-- The 'Pinchable' instance for it will be,
--
-- > data PostBody
-- >     = PostBodyMarkdown Text
-- >     | PostBodyRtf ByteString
-- >
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
-- Given an enum,
--
-- > enum Role {
-- >   DISABLED = 0,
-- >   USER,
-- >   ADMIN,
-- > }
--
-- The 'Pinchable' instance for it will be,
--
-- > data Role = RoleDisabled | RoleUser | RoleAdmin
-- >
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

-- $genericStruct
--
-- Given the struct,
--
-- > struct User {
-- >   1: required string name
-- >   2: optional string emailAddress
-- > }
--
-- A @Pinchable@ instance for it can be automatically derived by wrapping
-- fields of the data type with the 'Field' type and specifying the field
-- identifier as a type-level number. Fields which hold a @Maybe@ value are
-- considered optional.
--
-- @
-- data User = User
--     { userName         :: 'Field' 1 Text
--     , userEmailAddress :: Field 2 (Maybe Text)
--     }
--   deriving (Generic)
--
-- instance Pinchable User
-- @
--
-- (The @DeriveGeneric@ extension is required to automatically derive
-- instances of the @Generic@ typeclass.)

-- $genericUnion
--
-- As with structs and exceptions, fields of the data type representing a
-- union must be tagged with 'Field', but to satisfy the property of a union
-- that only one value is set at a time, they must be on separate
-- constructors.
--
-- For example, given the union,
--
-- > union Item {
-- >   1: binary bin
-- >   2: string str
-- >   3: i32    int
-- > }
--
-- A @Pinchable@ instance can be derived like so,
--
-- > data Item
-- >     = ItemBin (Field 1 ByteString)
-- >     | ItemStr (Field 2 Text)
-- >     | ItemInt (Field 3 Int32)
-- >   deriving (Generic)
-- >
-- > instance Pinchable Item

-- $genericEnum
--
-- Given the enum,
--
-- > enum Op {
-- >   Add, Sub, Mul, Div
-- > }
--
-- A @Pinchable@ instance can be derived for it by creating one constructor
-- for each of the enum values and providing it a single 'Enumeration'
-- argument tagged with the enum value.
--
-- @
-- data Op
--     = OpAdd ('Enumeration' 0)
--     | OpSub (Enumeration 1)
--     | OpMul (Enumeration 2)
--     | OpDiv (Enumeration 3)
--   deriving (Generic)
--
-- instance Pinchable Op
-- @
