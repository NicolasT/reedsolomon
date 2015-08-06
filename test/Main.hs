module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Galois
import qualified Matrix
import qualified ReedSolomon

tests :: TestTree
tests = testGroup "Tests" [ Galois.tests
                          , Matrix.tests
                          , ReedSolomon.tests
                          ]

main :: IO ()
main = defaultMain tests
