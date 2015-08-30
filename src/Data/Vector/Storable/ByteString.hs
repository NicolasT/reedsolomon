{-# LANGUAGE RankNTypes #-}

module Data.Vector.Storable.ByteString (
      fromByteString
    , toByteString
    , iso
    ) where

import Data.Word (Word8)

import Data.Profunctor (Profunctor, dimap)

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

type Iso s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)
type Iso' s a = Iso s s a a

iso :: Iso' ByteString (Vector Word8)
iso = dimap fromByteString (fmap toByteString)
