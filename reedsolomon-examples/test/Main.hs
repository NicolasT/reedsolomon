module Main (main) where

import Control.Monad (forM_, void)
import Data.Char (chr)
import System.Exit (ExitCode(ExitFailure))
import System.IO (Handle, SeekMode(AbsoluteSeek), hFlush, hGetContents, hPutStr, hSeek)

import System.Directory (getDirectoryContents, removeFile)

import System.FilePath ((</>), takeDirectory, takeFileName)

import System.Process (readProcess, readProcessWithExitCode)

import System.Random (getStdGen, randomRIO, randomRs)

import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)

import Test.Tasty (defaultMain)
import Test.Tasty.Hspec

main :: IO ()
main = defaultMain =<< testSpec "reedsolomon-examples" spec

spec :: Spec
spec = do
    encoderSpec
    decoderSpec

encoderSpec :: Spec
encoderSpec = around (withDataAndOut fileSize) $
    describe "reedsolomon-simple-encoder" $ do
        context "when provided with no options" $
            it "outputs 6 (4 + 2) parts" $ \(dat, out) -> do
                exec "reedsolomon-simple-encoder" ["--out", out, dat]
                parts <- listDirectory out
                length parts `shouldBe` 6
        context "when provided with options" $ do
            let testOptions n k =
                    let msg = unwords ["outputs", show (n + k)
                                      , "(" ++ show n ++ " + " ++ show k ++ ")"
                                      , "parts for"
                                      , "'--data", show n, "--par", show k ++ "'"
                                      ]
                    in
                    it msg $ \(dat, out) -> do
                        exec "reedsolomon-simple-encoder" [ "--out", out
                                                          , "--data", show n
                                                          , "--par", show k
                                                          , dat
                                                          ]
                        parts <- listDirectory out
                        length parts `shouldBe` n + k

            testOptions 4 2
            testOptions 9 3
            testOptions 50 10

  where
    fileSize = 4 * 1024 + 1


decoderSpec :: Spec
decoderSpec = around (withEncodedData 9 3 fileSize) $
    describe "reedsolomon-simple-decoder" $ do
        context "when no data parts are missing" $
            it "decodes the data correctly" $ runTest (const (return ()))

        context "when K random parts are missing" $
            it "decodes the data correctly" $
                let pre (_, k, parts, _, _) =
                        forM_ [0 .. k - 1] $ \_ -> do
                            parts' <- listDirectory (takeDirectory parts)
                            idx <- randomRIO (0, length parts' - 1)
                            let part = takeDirectory parts </> (parts' !! idx)
                            removeFile part
                in
                runTest pre

        context "when (K + 1) data parts are missing" $
            it "fails to decode" $ \(n, k, parts, out, _) -> do
                parts' <- listDirectory (takeDirectory parts)
                forM_ (take (k + 1) parts') $ \name ->
                    removeFile (takeDirectory parts </> name)

                let cmd = "reedsolomon-simple-decoder"
                    args = [ "--out", out </> "result"
                           , "--data", show n
                           , "--par", show k
                           , parts
                           ]
                (exitCode, _, err) <- readProcessWithExitCode cmd args ""

                exitCode `shouldSatisfy` isExitFailure
                err `shouldContain` "InvalidNumberOfShards"
  where
    fileSize = 3 * 1024 + 1
    isExitFailure e = case e of
        ExitFailure _ -> True
        _ -> False
    runTest :: ((Int, Int, FilePath, FilePath, Handle) -> IO ())
            -> (Int, Int, FilePath, FilePath, Handle)
            -> IO ()
    runTest pre args@(n, k, parts, out, original) = do
        pre args

        let result = out </> "result"
        exec "reedsolomon-simple-decoder" [ "--out", result
                                          , "--data", show n
                                          , "--par", show k
                                          , parts
                                          ]

        result' <- take fileSize `fmap` readFile result
        original' <- hGetContents original

        result' `shouldBe` original'


exec :: String -> [String] -> IO ()
exec cmd args = void $ readProcess cmd args ""

fillHandle :: Int -> Handle -> IO ()
fillHandle size hnd = do
    gen <- getStdGen
    hPutStr hnd $ map chr $ take size $ randomRs (0, 127) gen
    hFlush hnd

withDataAndOut :: Int -> ((FilePath, FilePath) -> IO ()) -> IO ()
withDataAndOut size action =
    withSystemTempFile "reedsolomon-examples.dat" $ \dat hnd -> do
        fillHandle size hnd
        withSystemTempDirectory "reedsolomon-examples.out" $ \out ->
            action (dat, out)

withEncodedData :: Int -> Int -> Int -> ((Int, Int, FilePath, FilePath, Handle) -> IO ()) -> IO ()
withEncodedData n k size action =
    withSystemTempFile "reedsolomon-examples.dat" $ \dat hnd -> do
        fillHandle size hnd
        hSeek hnd AbsoluteSeek 0

        withSystemTempDirectory "reedsolomon-examples.out" $ \out -> do
            exec "reedsolomon-simple-encoder" [ "--out", out
                                              , "--data", show n
                                              , "--par", show k
                                              , dat
                                              ]

            let out' = out </> takeFileName dat
            withSystemTempDirectory "reedsolomon-examples.out" $ \out2 ->
                action (n, k, out', out2, hnd)

listDirectory :: FilePath -> IO [FilePath]
listDirectory = fmap (filter $ flip notElem [".", ".."]) . getDirectoryContents
