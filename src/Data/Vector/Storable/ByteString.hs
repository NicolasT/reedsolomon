module Data.Vector.Storable.ByteString (
      fromByteString
    , toByteString
    ) where

import Data.Word (Word8)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as I

import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as S

fromByteString :: ByteString -> Vector Word8
fromByteString = unsafeFromForeignPtr . I.toForeignPtr
  where
    unsafeFromForeignPtr (p, o, l) = S.unsafeFromForeignPtr p o l
{-# INLINE fromByteString #-}

toByteString :: Vector Word8 -> ByteString
toByteString = fromForeignPtr . S.unsafeToForeignPtr
  where
    fromForeignPtr (p, o, l) = I.fromForeignPtr p o l
{-# INLINE toByteString #-}
