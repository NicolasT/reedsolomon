> {-# LANGUAGE ScopedTypeVariables #-}
> module Galois (tests) where
>
> import Control.Monad (foldM, void, when)
> import Data.Word (Word8)
>
> import Control.Loop (numLoop)
>
> import qualified Data.Vector.Generic as V
> import qualified Data.Vector.Generic.Mutable as MV
> import qualified Data.Vector.Storable as SV
>
> import Test.Tasty (TestTree, testGroup)
> import Test.Tasty.HUnit (Assertion, (@?=), testCase)
> import Test.Tasty.QuickCheck (Property, (==>), testProperty)
>
> import Data.ReedSolomon.Galois
> import qualified Data.ReedSolomon.Galois.NoAsm as NoAsm
> import qualified Data.ReedSolomon.Galois.Amd64 as Amd64

/**
 * Unit tests for Galois
 *
 * Copyright 2015, Klaus Post
 * Copyright 2015, Backblaze, Inc.
 */

package reedsolomon

import (
	"bytes"
	"testing"
)

func TestAssociativity(t *testing.T) {
	for i := 0; i < 256; i++ {
		a := byte(i)
		for j := 0; j < 256; j++ {
			b := byte(j)
			for k := 0; k < 256; k++ {
				c := byte(k)
				x := galAdd(a, galAdd(b, c))
				y := galAdd(galAdd(a, b), c)
				if x != y {
					t.Fatal("add does not match:", x, "!=", y)
				}
				x = galMultiply(a, galMultiply(b, c))
				y = galMultiply(galMultiply(a, b), c)
				if x != y {
					t.Fatal("multiply does not match:", x, "!=", y)
				}
			}
		}
	}
}

> testAssociativity :: Assertion
> testAssociativity =
>     numLoop (0 :: Int) 255 $ \i -> do
>         let a = fromIntegral i
>         numLoop (0 :: Int) 255 $ \j -> do
>             let b = fromIntegral j
>             numLoop (0 :: Int) 255 $ \k -> do
>                 let c = fromIntegral k
>                     x = galAdd a (galAdd b c)
>                     y = galAdd (galAdd a b) c
>                 x @?= y
>
>                 let x' = galMultiply a (galMultiply b c)
>                     y' = galMultiply (galMultiply a b) c
>                 x' @?= y'

func TestIdentity(t *testing.T) {
	for i := 0; i < 256; i++ {
		a := byte(i)
		b := galAdd(a, 0)
		if a != b {
			t.Fatal("Add zero should yield same result", a, "!=", b)
		}
		b = galMultiply(a, 1)
		if a != b {
			t.Fatal("Mul by one should yield same result", a, "!=", b)
		}
	}
}

> testIdentity :: Assertion
> testIdentity =
>     numLoop (0 :: Int) 255 $ \i -> do
>         let a = fromIntegral i
>             b = galAdd a 0
>         b @?= a
>         let b' = galMultiply a 1
>         b' @?= a

func TestInverse(t *testing.T) {
	for i := 0; i < 256; i++ {
		a := byte(i)
		b := galSub(0, a)
		c := galAdd(a, b)
		if c != 0 {
			t.Fatal("inverse sub/add", c, "!=", 0)
		}
		if a != 0 {
			b = galDivide(1, a)
			c = galMultiply(a, b)
			if c != 1 {
				t.Fatal("inverse div/mul", c, "!=", 1)
			}
		}
	}
}

> testInverse :: Assertion
> testInverse =
>     numLoop (0 :: Int) 255 $ \i -> do
>         let a = fromIntegral i
>             b = galSub 0 a
>             c = galAdd a b
>         c @?= 0
>         when (a /= 0) $ do
>             let b' = galDivide 1 a
>                 c' = galMultiply a b'
>             c' @?= 1

func TestCommutativity(t *testing.T) {
	for i := 0; i < 256; i++ {
		a := byte(i)
		for j := 0; j < 256; j++ {
			b := byte(j)
			x := galAdd(a, b)
			y := galAdd(b, a)
			if x != y {
				t.Fatal(x, "!= ", y)
			}
			x = galMultiply(a, b)
			y = galMultiply(b, a)
			if x != y {
				t.Fatal(x, "!= ", y)
			}
		}
	}
}

> testCommutativity :: Assertion
> testCommutativity =
>     numLoop (0 :: Int) 255 $ \i -> do
>         let a = fromIntegral i
>         numLoop (0 :: Int) 255 $ \j -> do
>             let b = fromIntegral j
>                 x = galAdd a b
>                 y = galAdd b a
>             x @?= y
>             let x' = galMultiply a b
>                 y' = galMultiply b a
>             x' @?= y'

func TestDistributivity(t *testing.T) {
	for i := 0; i < 256; i++ {
		a := byte(i)
		for j := 0; j < 256; j++ {
			b := byte(j)
			for k := 0; k < 256; k++ {
				c := byte(k)
				x := galMultiply(a, galAdd(b, c))
				y := galAdd(galMultiply(a, b), galMultiply(a, c))
				if x != y {
					t.Fatal(x, "!= ", y)
				}
			}
		}
	}
}

> testDistributivity :: Assertion
> testDistributivity =
>     numLoop (0 :: Int) 255 $ \i -> do
>         let a = fromIntegral i
>         numLoop (0 :: Int) 255 $ \j -> do
>             let b = fromIntegral j
>             numLoop (0 :: Int) 255 $ \k -> do
>                 let c = fromIntegral k
>                     x = galMultiply a (galAdd b c)
>                     y = galAdd (galMultiply a b) (galMultiply a c)
>                 x @?= y

func TestExp(t *testing.T) {
	for i := 0; i < 256; i++ {
		a := byte(i)
		power := byte(1)
		for j := 0; j < 256; j++ {
			x := galExp(a, j)
			if x != power {
				t.Fatal(x, "!=", power)
			}
			power = galMultiply(power, a)
		}
	}
}

> testExp :: Assertion
> testExp =
>     numLoop (0 :: Int) 255 $ \i -> do
>         let a = fromIntegral i
>         void $ foldM (\power j -> do
>             let x = galExp a j
>             x @?= power
>             return (galMultiply power a))
>             1 [0 .. 255]

func TestGalois(t *testing.T) {
	// These values were copied output of the Python code.
	if galMultiply(3, 4) != 12 {
		t.Fatal("galMultiply(3, 4) != 12")
	}
	if galMultiply(7, 7) != 21 {
		t.Fatal("galMultiply(7, 7) != 21")
	}
	if galMultiply(23, 45) != 41 {
		t.Fatal("galMultiply(23, 45) != 41")
	}

	// Test slices (>16 entries to test assembler)
	in := []byte{0, 1, 2, 3, 4, 5, 6, 10, 50, 100, 150, 174, 201, 255, 99, 32, 67, 85}
	out := make([]byte, len(in))
	galMulSlice(25, in, out)
	expect := []byte{0x0, 0x19, 0x32, 0x2b, 0x64, 0x7d, 0x56, 0xfa, 0xb8, 0x6d, 0xc7, 0x85, 0xc3, 0x1f, 0x22, 0x7, 0x25, 0xfe}
	if 0 != bytes.Compare(out, expect) {
		t.Errorf("got %#v, expected %#v", out, expect)
	}

	galMulSlice(177, in, out)
	expect = []byte{0x0, 0xb1, 0x7f, 0xce, 0xfe, 0x4f, 0x81, 0x9e, 0x3, 0x6, 0xe8, 0x75, 0xbd, 0x40, 0x36, 0xa3, 0x95, 0xcb}
	if 0 != bytes.Compare(out, expect) {
		t.Errorf("got %#v, expected %#v", out, expect)
	}

	if galExp(2, 2) != 4 {
		t.Fatal("galExp(2, 2) != 4")
	}
	if galExp(5, 20) != 235 {
		t.Fatal("galExp(5, 20) != 235")
	}
	if galExp(13, 7) != 43 {
		t.Fatal("galExp(13, 7) != 43")
	}
}

> testGalois :: Assertion
> testGalois = do
>     galMultiply 3 4 @?= 12
>     galMultiply 7 7 @?= 21
>     galMultiply 23 45 @?= 41
>
>     let in_ = V.fromList [0, 1, 2, 3, 4, 5, 6, 10, 50, 100, 150, 174, 201, 255, 99, 32, 67, 85]
>     out <- MV.new (V.length in_)
>     out2 <- MV.new (V.length in_)
>     NoAsm.galMulSlice 25 in_ out
>     Amd64.galMulSlice 25 in_ out2
>     let expect = V.fromList [0x0, 0x19, 0x32, 0x2b, 0x64, 0x7d, 0x56, 0xfa, 0xb8, 0x6d, 0xc7, 0x85, 0xc3, 0x1f, 0x22, 0x7, 0x25, 0xfe]
>     out' :: SV.Vector Word8 <- V.freeze out
>     out2' :: SV.Vector Word8 <- V.freeze out
>     out' @?= expect
>     out2' @?= expect
>
>     NoAsm.galMulSlice 177 in_ out
>     Amd64.galMulSlice 177 in_ out
>     let expect' = V.fromList [0x0, 0xb1, 0x7f, 0xce, 0xfe, 0x4f, 0x81, 0x9e, 0x3, 0x6, 0xe8, 0x75, 0xbd, 0x40, 0x36, 0xa3, 0x95, 0xcb]
>     out'' :: SV.Vector Word8 <- V.freeze out
>     out2'' :: SV.Vector Word8 <- V.freeze out
>     out'' @?= expect'
>     out2'' @?= expect'
>
>     galExp 2 2 @?= 4
>     galExp 5 20 @?= 235
>     galExp 13 7 @?= 43

> galMulSliceProperty :: Word8 -> [Word8] -> Property
> galMulSliceProperty c in_ = length in_ /= 0 ==>
>     let in' = V.fromList in_ in
>     let noasm :: SV.Vector Word8 = V.create $ do
>             out <- MV.new (V.length in')
>             NoAsm.galMulSlice c in' out
>             return out in
>     let amd64 = V.create $ do
>             out <- MV.new (V.length in')
>             Amd64.galMulSlice c in' out
>             return out in
>     noasm == amd64

> galMulSliceXorProperty :: Word8 -> [Word8] -> Property
> galMulSliceXorProperty c in_ = length in_ /= 0 ==>
>     let in' = V.fromList in_ in
>     let out = V.map (\i -> i `mod` 123) in' in -- 'Random' initial 'out'
>     let noasm :: SV.Vector Word8 = V.create $ do
>             out' <- V.thaw out
>             NoAsm.galMulSliceXor c in' out'
>             return out' in
>     let amd64 = V.create $ do
>             out' <- V.thaw out
>             Amd64.galMulSliceXor c in' out'
>             return out' in
>     noasm == amd64

> tests :: TestTree
> tests = testGroup "Galois" [
>       testGroup "Unit" [
>             testCase "associativity" testAssociativity
>           , testCase "identity" testIdentity
>           , testCase "inverse" testInverse
>           , testCase "commutativity" testCommutativity
>           , testCase "distributivity" testDistributivity
>           , testCase "exp" testExp
>           , testCase "galois" testGalois
>           ]
>     , testGroup "Properties" [
>             testGroup "NoAsm/Amd64" [
>                   testProperty "galMulSlice" galMulSliceProperty
>                 , testProperty "galMulSliceXor" galMulSliceXorProperty
>                 ]
>           ]
>     ]