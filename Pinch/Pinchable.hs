{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Pinch.Pinchable
    ( Pinchable(..)
    ) where


#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif

import Data.ByteString     (ByteString)
import Data.Hashable       (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.Int            (Int16, Int32, Int64)
import Data.Proxy          (Proxy (..))
import Data.Text           (Text)
import Data.Typeable       ((:~:) (..), Typeable, eqT)
import Data.Vector         (Vector)
import Data.Word           (Word8)

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import qualified Data.Map.Strict     as M
import qualified Data.Set            as S
import qualified Data.Text.Encoding  as TE
import qualified Data.Vector         as V

import Pinch.Internal.TType
import Pinch.Internal.Value


-- | Typeclass implemented by types that can be sent over the wire by Thrift.
class IsTType (PType a) => Pinchable a where
    -- | 'TType' tag for the type.
    type PType a :: *

    -- | Convert @a@ into a 'Value'.
    pinch :: a -> Value (PType a)

    -- | Read a 'Value' back from @a@.
    unpinch :: Value (PType a) -> Either String a

    -- TODO: Use a parser type instead of Either?


-- | Helper to 'unpinch' values by matching TTypes.
checkedUnpinch
    :: forall a b. (Pinchable a, Typeable b)
    => Value b -> Either String a
checkedUnpinch = case eqT of
    Nothing -> const $ Left "Type mismatch"
    Just (Refl :: PType a :~: b) -> unpinch


-- | Helper to 'pinch' maps.
pinchMap
    :: forall k v ktype vtype kval vval m.
        ( Pinchable k
        , Pinchable v
        , ktype ~ PType k
        , vtype ~ PType v
        , kval ~ Value ktype
        , vval ~ Value vtype
        )
    => ((k -> v -> HashMap kval vval -> HashMap kval vval)
           -> HashMap kval vval -> m -> HashMap kval vval)
          -- ^ @foldrWithKey@
    -> m  -- ^ map that implements @foldrWithKey@
    -> Value TMap
pinchMap folder = VMap ktype vtype . folder go HM.empty
  where
    ktype = ttype (Proxy :: Proxy ktype)
    vtype = ttype (Proxy :: Proxy vtype)
    go k v = HM.insert (pinch k) (pinch v)


instance Pinchable ByteString where
    type PType ByteString = TBinary
    pinch = VBinary
    unpinch = Right . vbinary

instance Pinchable Text where
    type PType Text = TBinary
    pinch = VBinary . TE.encodeUtf8
    unpinch = Right . TE.decodeUtf8 . vbinary

instance Pinchable Bool where
    type PType Bool = TBool
    pinch = VBool
    unpinch = Right . vbool

instance Pinchable Word8 where
    type PType Word8 = TByte
    pinch = VByte
    unpinch = Right . vbyte

instance Pinchable Double where
    type PType Double = TDouble
    pinch = VDouble
    unpinch = Right . vdouble

instance Pinchable Int16 where
    type PType Int16 = TInt16
    pinch = VInt16
    unpinch = Right . vint16

instance Pinchable Int32 where
    type PType Int32 = TInt32
    pinch = VInt32
    unpinch = Right . vint32

instance Pinchable Int64 where
    type PType Int64 = TInt64
    pinch = VInt64
    unpinch = Right . vint64

instance Pinchable a => Pinchable (Vector a) where
    type PType (Vector a) = TList

    pinch = VList vtype . V.map pinch
      where vtype = ttype (Proxy :: Proxy (PType a))

    unpinch (VList _ xs) = V.mapM checkedUnpinch xs
    unpinch x = Left $ "Failed to read list. Got " ++ show x

instance Pinchable a => Pinchable [a] where
    type PType [a] = TList

    pinch = VList vtype . V.fromList . map pinch
      where vtype = ttype (Proxy :: Proxy (PType a))

    unpinch (VList _ xs) = mapM checkedUnpinch $ V.toList xs
    unpinch x = Left $ "Failed to read list. Got " ++ show x

instance
  ( Eq k
  , Hashable k
  , Pinchable k
  , Pinchable v
  ) => Pinchable (HM.HashMap k v) where
    type PType (HM.HashMap k v) = TMap

    pinch = pinchMap HM.foldrWithKey

    unpinch (VMap _ _ xs) =
        fmap HM.fromList . mapM go $ HM.toList xs
      where go (k, v) = (,) <$> checkedUnpinch k <*> checkedUnpinch v
    unpinch x = Left $ "Failed to read map. Got " ++ show x

instance (Ord k, Pinchable k, Pinchable v) => Pinchable (M.Map k v) where
    type PType (M.Map k v) = TMap

    pinch = pinchMap M.foldrWithKey

    unpinch (VMap _ _ xs) =
        fmap M.fromList . mapM go $ HM.toList xs
      where go (k, v) = (,) <$> checkedUnpinch k <*> checkedUnpinch v
    unpinch x = Left $ "Failed to read map. Got " ++ show x

instance (Eq a, Hashable a, Pinchable a) => Pinchable (HS.HashSet a) where
    type PType (HS.HashSet a) = TSet

    pinch = VSet vtype . HS.map pinch
      where vtype = ttype (Proxy :: Proxy (PType a))

    unpinch (VSet _ xs) =
        fmap HS.fromList . mapM checkedUnpinch $ HS.toList xs
    unpinch x = Left $ "Failed to read set. Got " ++ show x

instance (Ord a, Pinchable a) => Pinchable (S.Set a) where
    type PType (S.Set a) = TSet

    pinch = VSet vtype . S.foldr (HS.insert . pinch) HS.empty
      where vtype = ttype (Proxy :: Proxy (PType a))

    unpinch (VSet _ xs) =
        fmap S.fromList . mapM checkedUnpinch $ HS.toList xs
    unpinch x = Left $ "Failed to read set. Got " ++ show x
