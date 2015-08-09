> module Data.ReedSolomon.Galois.Amd64 (
>       galMulSlice
>     , galMulSliceXor
>     ) where
>
> import Control.Monad (when)
> import Data.Bits (shiftL, shiftR, xor)
> import Data.Word (Word8)
> import Foreign.C (CSize(..))
> import Foreign.Ptr (Ptr)
>
> import Control.Monad.Primitive (PrimMonad, PrimState, unsafePrimToPrim)
>
> import Control.Loop (numLoop)
>
> import Data.Vector.Generic ((!))
> import qualified Data.Vector.Storable as V
> import qualified Data.Vector.Storable.Mutable as MV
>
> import qualified Data.ReedSolomon.Galois.GenTables as GenTables

//+build !noasm
//+build !appengine

// Copyright 2015, Klaus Post, see LICENSE for details.

package reedsolomon

import (
	"github.com/klauspost/cpuid"
)

func galMulSSSE3(low, high, in, out []byte)
func galMulSSSE3Xor(low, high, in, out []byte)

// This is what the assembler rountes does in blocks of 16 bytes:
/*
func galMulSSSE3(low, high, in, out []byte) {
	for n, input := range in {
		l := input & 0xf
		h := input >> 4
		out[n] = low[l] ^ high[h]
	}
}

func galMulSSSE3Xor(low, high, in, out []byte) {
	for n, input := range in {
		l := input & 0xf
		h := input >> 4
		out[n] ^= low[l] ^ high[h]
	}
}
*/

func galMulSlice(c byte, in, out []byte) {
	var done int
	if cpuid.CPU.SSSE3() {
		galMulSSSE3(mulTableLow[c][:], mulTableHigh[c][:], in, out)
		done = (len(in) >> 4) << 4
	}
	remain := len(in) - done
	if remain > 0 {
		mt := mulTable[c]
		for i := done; i < len(in); i++ {
			out[i] = mt[in[i]]
		}
	}
}

> galMulSlice :: PrimMonad m => Word8 -> V.Vector Word8 -> V.MVector (PrimState m) Word8 -> m ()
> galMulSlice c in_ out = do
>     let c' = fromIntegral c
>         mtlc = GenTables.mulTableLow ! c'
>         mthc = GenTables.mulTableHigh ! c'
>         len = V.length in_
>     galMulSSSE3 mtlc mthc in_ out
>     let done = (len `shiftR` 4) `shiftL` 4
>     when (len - done > 0) $ do
>         let mt = GenTables.mulTable ! c'
>         numLoop done (len - 1) $ \i ->
>             MV.write out i (mt ! (fromIntegral $ in_ ! i))
>   where
>     galMulSSSE3 = cProtoToST c_galMulSSSE3
> {-# INLINE galMulSlice #-}

func galMulSliceXor(c byte, in, out []byte) {
	var done int
	if cpuid.CPU.SSSE3() {
		galMulSSSE3Xor(mulTableLow[c][:], mulTableHigh[c][:], in, out)
		done = (len(in) >> 4) << 4
	}
	remain := len(in) - done
	if remain > 0 {
		mt := mulTable[c]
		for i := done; i < len(in); i++ {
			out[i] ^= mt[in[i]]
		}
	}
}

> galMulSliceXor :: PrimMonad m => Word8 -> V.Vector Word8 -> V.MVector (PrimState m) Word8 -> m ()
> galMulSliceXor c in_ out = do
>     let c' = fromIntegral c
>         mtlc = GenTables.mulTableLow ! c'
>         mthc = GenTables.mulTableHigh ! c'
>         len = min (V.length in_) (MV.length out)
>     galMulSSSE3Xor mtlc mthc in_ out
>     let done = (len `shiftR` 4) `shiftL` 4
>     when (len - done > 0) $ do
>         let mt = GenTables.mulTable ! c'
>         numLoop done (len - 1) $ \i -> do
>             r <- MV.read out i
>             let m = mt ! (fromIntegral $ in_ ! i)
>                 r' = r `xor` m
>             MV.write out i r'
>   where
>     galMulSSSE3Xor = cProtoToST c_galMulSSSE3Xor
> {-# INLINE galMulSliceXor #-}

> type CProto = Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> CSize -> IO ()
>
> cProtoToST :: PrimMonad m
>            => CProto
>            -> V.Vector Word8
>            -> V.Vector Word8
>            -> V.Vector Word8
>            -> V.MVector (PrimState m) Word8
>            -> m ()
> cProtoToST inner low high in_ out@(MV.MVector ol op) = do
>     let len = fromIntegral $ min (V.length in_) (MV.length out)
>         thawToPtr :: V.Storable a => V.Vector a -> (Ptr a -> IO b) -> IO b
>         thawToPtr v f = V.unsafeThaw v >>= flip MV.unsafeWith f
>     unsafePrimToPrim $ -- TODO Safe?
>         thawToPtr low $ \low' ->
>         thawToPtr high $ \high' ->
>         thawToPtr in_ $ \in_' ->
>         MV.unsafeWith (MV.MVector ol op) $ \out' ->
>         inner low' high' in_' out' len
> {-# INLINE cProtoToST #-}

> foreign import ccall unsafe "galMulSSSE3" c_galMulSSSE3 :: CProto
> foreign import ccall unsafe "galMulSSSE3Xor" c_galMulSSSE3Xor :: CProto