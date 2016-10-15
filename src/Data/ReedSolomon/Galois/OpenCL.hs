{-# LANGUAGE QuasiQuotes #-}

module Data.ReedSolomon.Galois.OpenCL where

import Control.Monad (void)
import Data.Word (Word8)

import Control.Parallel.OpenCL

import Foreign (castPtr, nullPtr)

import Text.Heredoc

import qualified Data.Vector.Storable as V hiding (unsafeIndex)
import qualified Data.Vector.Storable.Mutable as MV

import qualified Data.Vector.Generic.Sized as S
import qualified Data.ReedSolomon.Galois.GenTables as GenTables

import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.ReedSolomon.Galois.SIMD (galMulSlice)

programSource :: String
programSource = [here|
    constant uchar16 low_mask_unpacked = { 0x0f, 0x0f, 0x0f, 0x0f, 0x0f, 0x0f, 0x0f, 0x0f
                                         , 0x0f, 0x0f, 0x0f, 0x0f, 0x0f, 0x0f, 0x0f, 0x0f
                                         },
                     zeros = { 0, 0, 0, 0, 0, 0, 0, 0
                             , 0, 0, 0, 0, 0, 0, 0, 0
                             };

    __kernel void gal_mul(__global const uchar16 * const low_vectorp,
                          __global const uchar16 * const high_vectorp,
                          __global const uchar16 * const inp,
                          __global uchar16 * const outp) {
        const uchar16 low_vector = *low_vectorp,
                      high_vector = *high_vectorp;

        int id = get_global_id(0);

        uchar16 in = inp[id],
                low_input = in & low_mask_unpacked,
                in_shifted = in >> 4,
                high_input = in_shifted & low_mask_unpacked,
                mul_low_part = shuffle2(low_vector, zeros, low_input),
                mul_high_part = shuffle2(high_vector, zeros, high_input),
                new = mul_low_part ^ mul_high_part,
                result = new;

        outp[id] = result;
    }
|]

buildProgram :: (MonadIO m1, MonadIO m2)
             => CLDeviceID
             -> CLContext
             -> CLCommandQueue
             -> m1 (Word8 -> V.Vector Word8 -> MV.IOVector Word8 -> m2 ())
buildProgram dev context q = do
    program <- liftIO $ do
        p <- clCreateProgramWithSource context programSource
        clBuildProgram p [dev] ""
        return p

    return $ \c in_ out -> do
        let mtlc = S.index GenTables.mulTableLow (c :: Word8)
            mthc = S.index GenTables.mulTableHigh c
            len = min (V.length in_) (MV.length out)

        liftIO $ do
            kernel <- clCreateKernel program "gal_mul"

            S.unsafeWith mtlc $ \lowp ->
                S.unsafeWith mthc $ \highp ->
                    V.unsafeWith in_ $ \inp ->
                        MV.unsafeWith out $ \outp -> do
                mem_low_vector <- clCreateBuffer context [CL_MEM_READ_ONLY, CL_MEM_COPY_HOST_PTR] (16 :: Int, castPtr lowp)
                mem_high_vector <- clCreateBuffer context [CL_MEM_READ_ONLY, CL_MEM_COPY_HOST_PTR] (16 :: Int, castPtr highp)
                mem_in <- clCreateBuffer context [CL_MEM_READ_ONLY, CL_MEM_COPY_HOST_PTR] (len, castPtr inp)
                mem_out <- clCreateBuffer context [CL_MEM_WRITE_ONLY] (len, nullPtr)

                clSetKernelArgSto kernel 0 mem_low_vector
                clSetKernelArgSto kernel 1 mem_high_vector
                clSetKernelArgSto kernel 2 mem_in
                clSetKernelArgSto kernel 3 mem_out

                eventExec <- clEnqueueNDRangeKernel q kernel [len `div` 16] [] []
                void $ clEnqueueReadBuffer q mem_out True 0 len (castPtr outp) [eventExec]

demo :: IO ()
demo = do
    (platform:_) <- clGetPlatformIDs
    (dev:_) <- clGetDeviceIDs platform CL_DEVICE_TYPE_ALL
    context <- clCreateContext [CL_CONTEXT_PLATFORM platform] [dev] print
    q <- clCreateCommandQueue context dev []

    prog <- buildProgram dev context q

    let in_ = V.fromList [0 .. 3 * 16 - 1]
        c = 8
    out <- MV.new (V.length in_)

    prog c in_ out

    print =<< V.unsafeFreeze out

    out' <- MV.new (V.length in_)
    galMulSlice c in_ out'
    print =<< V.unsafeFreeze out'

{-
demo :: IO ()
demo = do
    let in_ = V.fromList [0 .. 3 * 16 - 1 :: Word8]
        c = 2 :: Word8
        mtlc = S.index GenTables.mulTableLow c
        mthc = S.index GenTables.mulTableHigh c
        len = V.length in_

    (platform:_) <- clGetPlatformIDs
    (dev:_) <- clGetDeviceIDs platform CL_DEVICE_TYPE_ALL
    context <- clCreateContext [CL_CONTEXT_PLATFORM platform] [dev] print
    q <- clCreateCommandQueue context dev []

    program <- clCreateProgramWithSource context programSource
    catch (clBuildProgram program [dev] "")
          (\CL_BUILD_PROGRAM_FAILURE -> do
              putStrLn =<< clGetProgramBuildLog program dev
              throw CL_BUILD_PROGRAM_FAILURE)

    kernel <- clCreateKernel program "dup"

    {-let original = [0 .. 20] :: [CFloat]
        elemSize = sizeOf (0 :: CFloat)
        vecSize = elemSize * length original
    input <- newArray original-}

    out <- MV.new len

    S.unsafeWith mtlc $ \mtlcPtr -> do
    S.unsafeWith mthc $ \mthcPtr -> do
    V.unsafeWith in_ $ \inPtr -> do
    MV.unsafeWith out $ \outPtr -> do
        mem_low_vector <- clCreateBuffer context [CL_MEM_READ_ONLY, CL_MEM_COPY_HOST_PTR] (16, castPtr mtlcPtr)
        mem_high_vector <- clCreateBuffer context [CL_MEM_READ_ONLY, CL_MEM_COPY_HOST_PTR] (16, castPtr mthcPtr)
        mem_in <- clCreateBuffer context [CL_MEM_READ_ONLY, CL_MEM_COPY_HOST_PTR] (len, castPtr inPtr)
        mem_out <- clCreateBuffer context [CL_MEM_WRITE_ONLY] (len, nullPtr)

        clSetKernelArgSto kernel 0 mem_low_vector
        clSetKernelArgSto kernel 1 mem_high_vector
        clSetKernelArgSto kernel 2 mem_in
        clSetKernelArgSto kernel 3 mem_out

        eventExec <- clEnqueueNDRangeKernel q kernel [len `div` 16] [] []
        eventRead <- clEnqueueReadBuffer q mem_out True 0 len (castPtr outPtr) [eventExec]

        return ()

    putStrLn "OpenCL"
    print =<< (V.unsafeFreeze out :: IO (V.Vector Word8))


    out' <- MV.new len
    galMulSlice c in_ out'
    putStrLn "SIMD"
    print =<< (V.unsafeFreeze out' :: IO (V.Vector Word8))
-}
