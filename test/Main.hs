module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Galois
import qualified Matrix
import qualified ReedSolomon
import qualified Vector

tests :: TestTree
tests = testGroup "Tests" [ Galois.tests
                          , Matrix.tests
                          , ReedSolomon.tests
                          , Vector.tests
                          ]

main :: IO ()
main = defaultMain tests
