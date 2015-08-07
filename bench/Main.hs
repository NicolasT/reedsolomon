{-# LANGUAGE RankNTypes #-}
module Main (main) where

import Control.Monad.ST (ST)
import Data.Word (Word8)

import Criterion.Main

import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Storable as SV

import qualified Data.ReedSolomon.Galois.NoAsm as NoAsm
import qualified Data.ReedSolomon.Galois.Amd64 as Amd64

main :: IO ()
main = defaultMain [
      bgroup "Galois/galMulSlice/1048576" [
          bench "NoAsm" $ whnf (benchGalMulSlice NoAsm.galMulSlice 177) v1048576
        , bench "Amd64" $ whnf (benchGalMulSlice Amd64.galMulSlice 177) v1048576
        ]
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
