module Pinch.Internal.Util
    ( vbin
    , vbool
    , vbyt
    , vdub
    , vi16
    , vi32
    , vi64
    , vlist
    , vmap
    , vsome
    , vstruct
    , vset

    , vstruct_
    , vset_
    , vlist_
    , vmap_
    , vdub_
    , vbyt_
    , vi16_
    , vi32_
    , vi64_
    , vbool_
    , vbin_
    ) where

import Data.ByteString (ByteString)
import Data.Int

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import qualified Data.Vector         as V

import Pinch.Internal.TType
import Pinch.Internal.Value

vsome :: IsTType a => Value a -> SomeValue
vsome = SomeValue

vstruct_ :: [(Int16, SomeValue)] -> SomeValue
vstruct_ = vsome . vstruct

vset_ :: IsTType a => [Value a] -> SomeValue
vset_ = vsome . vset

vlist_ :: IsTType a => [Value a] -> SomeValue
vlist_ = vsome . vlist

vmap_ :: (IsTType k, IsTType v) => [(Value k, Value v)] -> SomeValue
vmap_ = vsome . vmap

vdub_ :: Double -> SomeValue
vdub_ = vsome . vdub

vbyt_ :: Int8 -> SomeValue
vbyt_ = vsome . vbyt

vi16_ :: Int16 -> SomeValue
vi16_ = vsome . vi16

vi32_ :: Int32 -> SomeValue
vi32_ = vsome . vi32

vi64_ :: Int64 -> SomeValue
vi64_ = vsome . vi64

vbool_ :: Bool -> SomeValue
vbool_ = vsome . vbool

vbin_ :: ByteString -> SomeValue
vbin_ = vsome . vbin

vstruct :: [(Int16, SomeValue)] -> Value TStruct
vstruct = VStruct . HM.fromList

vset :: IsTType a => [Value a] -> Value TSet
vset = VSet . HS.fromList

vlist :: IsTType a => [Value a] -> Value TList
vlist = VList . V.fromList

vmap :: (IsTType k, IsTType v) => [(Value k, Value v)] -> Value TMap
vmap = VMap . HM.fromList

vdub :: Double -> Value TDouble
vdub = VDouble

vbyt :: Int8 -> Value TByte
vbyt = VByte

vi16 :: Int16 -> Value TInt16
vi16 = VInt16

vi32 :: Int32 -> Value TInt32
vi32 = VInt32

vi64 :: Int64 -> Value TInt64
vi64 = VInt64

vbool :: Bool -> Value TBool
vbool = VBool

vbin :: ByteString -> Value TBinary
vbin = VBinary
