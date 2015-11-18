{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
module Main (main) where

import Control.Monad.ST (ST)
import Data.Word (Word8)

#ifdef SIMD
import Foreign.C (CSize(..))
#endif

import Criterion.Main

import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Storable as SV

#ifdef SIMD
import qualified Data.Vector.Generic.Sized as S
#endif
import qualified Data.ReedSolomon.Galois.NoAsm as NoAsm
#ifdef SIMD
import qualified Data.ReedSolomon.Galois.Amd64 as Amd64
#endif

main :: IO ()
main = defaultMain [
      bgroup "Galois/galMulSlice/1048576" [
          bench "NoAsm" $ whnf (benchGalMulSlice NoAsm.galMulSlice 177) v1048576
#ifdef SIMD
        , bench "Native" $ whnf (benchGalMulSlice Amd64.galMulSlice 177) v1048576
#endif
        ]
#ifdef SIMD
    , bgroup "reedsolomon_gal_mul" [
          bench "Native" $ whnf (benchRGM c_reedsolomon_gal_mul) v1048576
        , bench "AVX-Optimized" $ whnf (benchRGM c_reedsolomon_gal_mul_avx_opt) v1048576
        , bench "AVX" $ whnf (benchRGM c_reedsolomon_gal_mul_avx) v1048576
        , bench "SSE4.1" $ whnf (benchRGM c_reedsolomon_gal_mul_sse_4_1) v1048576
        , bench "Generic" $ whnf (benchRGM c_reedsolomon_gal_mul_generic) v1048576
        ]
#endif
    ]
  where
    v1048576 = V.fromListN 1048576 $ cycle [minBound .. maxBound]
    benchGalMulSlice :: (forall s. Word8 -> SV.Vector Word8 -> SV.MVector s Word8 -> ST s ())
                     -> Word8
                     -> SV.Vector Word8
                     -> SV.Vector Word8
    benchGalMulSlice f c in_ = V.create $ do
        out <- MV.new (V.length in_)
        f c in_ out
        return out
#ifdef SIMD
    benchRGM :: Amd64.CProto
             -> SV.Vector Word8
             -> SV.Vector Word8
    benchRGM f in_ = V.create $ do
        out <- MV.new (V.length in_)
        _ <- f' v16 v16 in_ out
        return out
      where
        f' :: forall s.
              S.SVector 16 Word8
           -> S.SVector 16 Word8
           -> SV.Vector Word8
           -> SV.MVector s Word8
           -> ST s CSize
        f' = Amd64.cProtoToPrim f
        v16 = [0 .. 15]
#endif

#ifdef SIMD
type CProto = Amd64.CProto
foreign import ccall unsafe "reedsolomon_gal_mul" c_reedsolomon_gal_mul :: CProto
foreign import ccall unsafe "reedsolomon_gal_mul_avx_opt" c_reedsolomon_gal_mul_avx_opt :: CProto
foreign import ccall unsafe "reedsolomon_gal_mul_avx" c_reedsolomon_gal_mul_avx :: CProto
foreign import ccall unsafe "reedsolomon_gal_mul_sse_4_1" c_reedsolomon_gal_mul_sse_4_1 :: CProto
foreign import ccall unsafe "reedsolomon_gal_mul_generic" c_reedsolomon_gal_mul_generic :: CProto
#endif
