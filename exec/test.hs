-- | Main entry point for test executable.

module Main where

import Data.Array.Repa ((:.)(..), Array, DIM1, DIM2, DIM3, Shape, Z(..), toList)
import Data.Array.Repa.Eval (fromList)
import Data.Array.Repa.Repr.ForeignPtr (F)
import Data.Complex (Complex(..), magnitude)

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import Test.Tasty.QuickCheck (Property, choose, forAll, testProperty, vector)

import Data.Array.Repa.FFTW (fft, ifft, fft2d, ifft2d, fft3d, ifft3d)


main :: IO ()
main = defaultMain $
    testGroup "repa-fftw"
    [ testCase "magsum1d-hunit" case_magsum1d
    , testCase "magsum2d-hunit" case_magsum2d
    , testCase "magsum3d-hunit" case_magsum3d
    , testProperty "magsum1d-qc" prop_magsum1d
    , testProperty "magsum2d-qc" prop_magsum2d
    , testProperty "magsum3d-qc" prop_magsum3d
    ]

case_magsum1d :: Assertion
case_magsum1d = do
    let a = fromList (Z :. 4) [i :+ 0 | i <- [0..3]]
    assertBool "diff is greater than epsilon" $ compareMagnitudeSum1d a

case_magsum2d :: Assertion
case_magsum2d = do
    let a = fromList (Z :. 4 :. 4) [i :+ 0 | i <- [0..15]]
    assertBool "diff is greater than epsilon" $ compareMagnitudeSum2d a

case_magsum3d :: Assertion
case_magsum3d = do
    let a = fromList (Z :. 4 :. 4 :. 4) [i :+ 0 | i <- [0..63]]
    assertBool "diff is greater than epsilon" $ compareMagnitudeSum3d a

prop_magsum1d :: Property
prop_magsum1d =
    forAll (choose (1, 16::Int)) $ \pow ->
    let size = (2 ^ pow)
    in forAll (vector size) $ \vals ->
       compareMagnitudeSum1d $ fromList (Z :. size) vals

prop_magsum2d :: Property
prop_magsum2d =
    forAll (choose (1, 4::Int)) $ \pow ->
    let extnt = 2 ^ pow
        size  = extnt ^ (2 :: Int)
    in  forAll (vector size) $ \vals ->
        compareMagnitudeSum2d $ fromList (Z :. extnt :. extnt) vals

prop_magsum3d :: Property
prop_magsum3d =
    forAll (choose (1, 4::Int)) $ \pow ->
    let extnt = 2 ^ pow
        size  = extnt ^ (3 :: Int)
    in  forAll (vector size) $ \vals ->
        compareMagnitudeSum3d $ fromList (Z :. extnt :. extnt :. extnt) vals

-- | Performs fft and ifft, then compareing magnitude of sum with the
-- original.
compareMagnitudeSum1d :: Array F DIM1 (Complex Double) -> Bool
compareMagnitudeSum1d = compareMagnitudeSum fft ifft

-- | Like 'compareMagnitudeSum', for 2-dimensional array.
compareMagnitudeSum2d :: Array F DIM2 (Complex Double) -> Bool
compareMagnitudeSum2d = compareMagnitudeSum fft2d ifft2d

-- | Like 'compareMagnitudeSum', for 3-dimensional array.
compareMagnitudeSum3d :: Array F DIM3 (Complex Double) -> Bool
compareMagnitudeSum3d = compareMagnitudeSum fft3d ifft3d

-- | Apply given functions, then compare the result with original.
compareMagnitudeSum ::
    (Shape sh)
    => (Array F sh (Complex Double) -> a)
    -- ^ Forward FFT function.
    -> (a -> Array F sh (Complex Double))
    -- ^ Inverse FFT function.
    -> Array F sh (Complex Double)
    -- ^ Original input.
    -> Bool
compareMagnitudeSum fn ifn arr = diff < epsilon
  where
    diff = abs (mag arr - mag arr')
    mag = magnitude . sum . toList
    arr' = ifn $ fn arr
    epsilon = 1e-2
