{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      :  Pinch
-- Copyright   :  (c) Abhinav Gupta 2015
-- License     :  BSD3
--
-- Maintainer  :  Abhinav Gupta <mail@abhinavg.net>
-- Stability   :  experimental
--
-- Pinch defines machinery to specify how types can be encoded into or decoded
-- from Thrift payloads.
--
module Pinch
    (

    -- * Serializing and deserializing

    -- $encodeDecodeValues

      encode
    , decode
    , decodeWithLeftovers

    -- * RPC

    -- $rpc

    , encodeMessage
    , decodeMessage

    -- * Pinchable

    , Pinchable(..)
    , Parser
    , runParser

    -- ** Automatically deriving instances

    -- | Pinch supports deriving instances of 'Pinchable' automatically for
    -- types that implement the @Generic@ typeclass provided that they follow
    -- the outlined patterns in their constructors.

    -- *** Structs and exceptions
    -- $genericStruct

    -- *** Unions
    -- $genericUnion

    , Field(..)
    , getField
    , putField
    , field

    , Void(..)

    -- *** Enums
    -- $genericEnum

    , Enumeration(..)
    , enum

    -- ** Manually writing instances

    -- | Instances of 'Pinchable' can be constructed by composing together
    -- existing instances and using the '.=', '.:', etc. helpers.

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
    , union
    , FieldPair

    -- *** @unpinch@

    , (.:)
    , (.:?)

    -- * Value

    -- | 'Value' is an intermediate representation of Thrift payloads tagged
    -- with TType tags. Types that want to be serialized into\/deserialized
    -- from Thrift payloads need only define a way to convert themselves to
    -- and from 'Value' objects via 'Pinchable'.

    , Value
    , SomeValue(..)

    -- * Messages

    , Message
    , mkMessage
    , messageName
    , messageType
    , messageId
    , getMessageBody

    , MessageType(..)

    -- * Protocols

    , Protocol
    , binaryProtocol
    , compactProtocol

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
import Data.ByteString (ByteString)
import Data.Int        (Int32)
import Data.Text       (Text)

import Pinch.Internal.Builder   (runBuilder)
import Pinch.Internal.Generic
import Pinch.Internal.Message
import Pinch.Internal.Pinchable
import Pinch.Internal.TType
import Pinch.Internal.Value
import Pinch.Protocol
import Pinch.Protocol.Binary
import Pinch.Protocol.Compact

------------------------------------------------------------------------------

-- $encodeDecodeValues
--
-- Types that can be serialized and deserialized into\/from Thrift values
-- implement the 'Pinchable' typeclass. Instances may be derived automatically
-- using generics, or written out by hand.
--
-- The 'Pinchable' typeclass converts objects into and from 'Value' objects,
-- which act as a direct mapping to the Thrift wire representation.  A
-- 'Protocol' is responsible for converting 'Value' objects to and from
-- bytestrings.
--
-- The 'encode' and 'decode' methods may be used on objects that implement the
-- 'Pinchable' typeclass to get the wire representation directly.
--
-- > +------------+   Pinchable                    Protocol    +------------+
-- > |            |               +------------+               |            |
-- > |            +----pinch------>            +---serialize--->            |
-- > | Your Type  |               |  Value a   |               | ByteString |
-- > |            <---unpinch-----+            <--deserialize--+            |
-- > |            |               +------------+               |            |
-- > |            |                                            |            |
-- > |            +-------------------encode------------------->            |
-- > |            |                                            |            |
-- > |            <-------------------decode-------------------+            |
-- > +------------+                                            +------------+

-- | Encode the given 'Pinchable' value using the given 'Protocol'.
--
-- >>> unpack $ encode binaryProtocol ["a" :: ByteString, "b"]
-- [11,0,0,0,2,0,0,0,1,97,0,0,0,1,98]
--
encode :: Pinchable a => Protocol -> a -> ByteString
encode p = runBuilder . serializeValue p . pinch
{-# INLINE encode #-}

-- | Decode a 'Pinchable' value from the using the given 'Protocol'.
--
-- >>> let s = pack [11,0,0,0,2,0,0,0,1,97,0,0,0,1,98]
-- >>> decode binaryProtocol s :: Either String [ByteString]
-- Right ["a","b"]
--
decode :: Pinchable a => Protocol -> ByteString -> Either String a
decode p = deserializeValue p >=> runParser . unpinch
{-# INLINE decode #-}

-- | Decode a 'Pinchable' value from the using the given 'Protocol'.
--
-- >>> let s = pack [11,0,0,0,2,0,0,0,1,97,0,0,0,1,98,0,0,0]
-- >>> decodeWithLeftovers binaryProtocol s :: Either String [ByteString]
-- Right ("\NUL\NUL\NUL",["a","b"])
--
decodeWithLeftovers :: Pinchable a => Protocol -> ByteString -> Either String (ByteString, a)
decodeWithLeftovers p = deserializeValue' p >=> traverse (runParser . unpinch)
{-# INLINE decodeWithLeftovers #-}

------------------------------------------------------------------------------

-- $rpc
--
-- Thrift requests implicitly form a struct and responses implicitly form a
-- union. To send\/receive the request\/response, it must be wrapped inside a
-- 'Message'. The 'Message' contains information like the method name, the
-- message ID (to match out of order responses with requests), and whether
-- it contains a request or a response.
--
-- Requests and responses may be wrapped into @Message@ objects using the
-- 'mkMessage' function. The message body can be retrieved back using the
-- 'getMessageBody' function. The 'encodeMessage' and 'decodeMessage'
-- functions may be used to encode and decode messages into\/from bytestrings.
--
-- Consider the service method,
--
-- > User getUser(1: string userName, 2: list<Attribute> attributes)
-- >   throws (1: UserDoesNotExist doesNotExist,
-- >           2: InternalError internalError)
--
-- The request and response for this method implictly take the form:
--
-- > struct getUserRequest {
-- >   1: string userName
-- >   2: list<Attribute> attributes
-- > }
--
-- > union getUserResponse {
-- >   0: User success
-- >   1: UserDoesNotExist doesNotExist
-- >   2: InternalError InternalError
-- > }
--
-- (Note that the field ID 0 is reserved for the return value of the method.)
--
-- Given corresponding data types @GetUserRequest@ and @GetUserResponse@, the
-- client can do something similar to,
--
-- @
-- let req = GetUserRequest "jsmith" []
--     msg = 'mkMessage' "getUser" 'Call' 0 req
-- response <- sendToServer ('encodeMessage' msg)
-- case 'decodeMessage' response of
--     Left err -> handleError err
--     Right msg -> case 'getMessageBody' msg of
--         Left err -> handleError err
--         Right (res :: GetUserResponse) -> handleResponse res
-- @
--
-- Similarly, on the server side,
--
-- @
-- case decodeMessage request of
--     Left err -> handleError err
--     Right msg -> case 'messageName' msg of
--         "getUser" -> case getMessageBody msg of
--             Left err -> handleError err
--             Right (req :: GetUserRequest) -> do
--                 let mid = 'messageId' msg
--                 res <- handleGetUser req
--                 return (mkMessage "getUser" 'Reply' mid res)
--                 -- Note that the response MUST contain the same
--                 -- message ID as its request.
--         _ -> handleUnknownMethod
-- @

-- | Encode the 'Message' using the given 'Protocol'.
--
-- @
-- let request = GetUserRequest (putField "jsmith") (putField [])
--     message = 'mkMessage' "getUser" Call 42 request
-- in encodeMessage binaryProtocol message
-- @
--
encodeMessage :: Protocol -> Message -> ByteString
encodeMessage p = runBuilder . serializeMessage p
{-# INLINE encodeMessage #-}

-- | Decode a 'Message' using the given 'Protocol'.
--
-- >>> decodeMessage binaryProtocol bs >>= getMessageBody :: Either String GetUserRequest
-- Right (GetUserRequest {userName = Field "jsmith", userAttributes = Field []})
--
decodeMessage :: Protocol -> ByteString -> Either String Message
decodeMessage = deserializeMessage
{-# INLINE decodeMessage #-}

-- | Build a @Message@.
mkMessage
    :: (Pinchable a, Tag a ~ TStruct)
    => Text
    -- ^ Name of the target method.
    -> MessageType
    -- ^ Type of the message.
    -> Int32
    -- ^ Message ID.
    -> a
    -- ^ Message payload. This must be an object which serializes into a
    -- struct.
    -> Message
mkMessage name typ mid body = Message name typ mid (pinch body)
{-# INLINE mkMessage #-}

-- | Read the message contents.
--
-- This returns a @Left@ result if the message contents do not match the
-- requested type.
getMessageBody
    :: (Pinchable a, Tag a ~ TStruct) => Message -> Either String a
getMessageBody = runParser . unpinch . messagePayload
{-# INLINE getMessageBody #-}

------------------------------------------------------------------------------

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
-- identifier as a type-level numeral. Fields which hold a @Maybe@ value are
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
-- The @DeriveGeneric@ extension is required to automatically derive instances
-- of the @Generic@ typeclass and the @DataKinds@ extension is required to use
-- type-level numerals.

------------------------------------------------------------------------------

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
--
-- The @DeriveGeneric@ extension is required to automatically derive instances
-- of the @Generic@ typeclass and the @DataKinds@ extension is required to use
-- type-level numerals.
--
-- If the union represents the response of a service method which returns a
-- @void@ result, the type 'Void' may be used.
--
-- @
-- data GetFooResponse
--   = GetFooDoesNotExist  (Field 1 FooDoesNotExist)
--   | GetFooInternalError (Field 2 InternalError)
--   | GetFooSuccess 'Void'
-- @

------------------------------------------------------------------------------

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
--
-- Note that you need to know the values assigned to the enums. If not
-- specified, Thrift automatically assigns incrementing values to the items in
-- the order they appear starting at 0.
--
-- The @DeriveGeneric@ extension is required to automatically derive instances
-- of the @Generic@ typeclass and the @DataKinds@ extension is required to use
-- type-level numerals.

------------------------------------------------------------------------------

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

------------------------------------------------------------------------------

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
-- @
-- data PostBody
--     = PostBodyMarkdown Text
--     | PostBodyRtf ByteString
--
-- instance Pinchable PostBody where
--     type Tag PostBody = 'TUnion'
--
--     pinch (PostBodyMarkdown markdownBody) =
--         'union' 1 markdownBody
--     pinch (PostBodyRtf rtfBody) =
--         union 2 rtfBody
--
--     unpinch v = PostBodyMarkdown \<$\> v .: 1
--             \<|\> PostBodyRtf      \<$\> v .: 2
-- @

------------------------------------------------------------------------------

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
-- >     type Tag Role = TEnum
-- >
-- >     pinch RoleDisabled = pinch (0 :: Int32)
-- >     pinch RoleUser     = pinch (1 :: Int32)
-- >     pinch RoleAdmin    = pinch (2 :: Int32)
-- >
-- >     unpinch v = do
-- >        value <- unpinch v
-- >        case (value :: Int32) of
-- >            0 -> Right RoleDisabled
-- >            1 -> Right RoleUser
-- >            2 -> Right RoleAdmin
-- >            _ -> Left $ "Unknown role: " ++ show value
