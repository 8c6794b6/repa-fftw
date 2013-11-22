-- | Main entry point for test executable.

module Main where

import Data.Array.Repa ((:.)(..), Array, DIM1, Z(..), toList)
import Data.Array.Repa.Eval (fromList)
import Data.Array.Repa.Repr.ForeignPtr (F)
import Data.Complex (Complex(..), magnitude)

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import Test.Tasty.QuickCheck (Property, choose, forAll, testProperty, vector)

import Data.Array.Repa.FFTW (fft, ifft)


main :: IO ()
main = defaultMain $
    testGroup "repa-fftw"
    [ testCase "magsum1d-hunit" case_magsum1d
    , testProperty "magsum1d-qc" prop_magsum1d
    ]

case_magsum1d :: Assertion
case_magsum1d = do
    let a = fromList (Z :. 5) [i :+ 0 | i <- [0..4]]
    assertBool "diff is greater than epsilon" $ compareMagnitudeSum a

prop_magsum1d :: Property
prop_magsum1d =
    forAll (choose (1, 16::Int)) $ \pow ->
    let size = (2 ^ pow) + 1 in
    forAll (vector size) $ \vals ->
    compareMagnitudeSum $ fromList (Z :. size) vals

-- | Performs fft and ifft, then compareing magnitude of sum with the
-- original.
compareMagnitudeSum :: Array F DIM1 (Complex Double) -> Bool
compareMagnitudeSum arr = diff < epsilon
  where
    arr' = ifft $ fft arr
    mag = magnitude . sum . toList
    epsilon = 1e-2
    diff = abs (mag arr - mag arr')
