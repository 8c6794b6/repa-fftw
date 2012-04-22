{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6, 2011, 2012
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : portable

Benchmark for comparing FFT with repa-fftw to repa-algorithms.
-}
module Main where

import Control.DeepSeq (NFData(..))
import Data.Complex
import System.Random

import Criterion.Main

import Data.Array.Repa ((:.)(..), Z(..))
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Eval as RE
import qualified Data.Array.Repa.Algorithms.FFT as RA
import qualified Data.Array.Repa.FFTW as F

type RFFTArr = R.Array R.U R.DIM1 (Double, Double)

main :: IO ()
main = do
  rs <- randomRs (0,1) `fmap` newStdGen
  is <- randomRs (0,1) `fmap` newStdGen
  let bench_fft n =
        let mkarr ks = RE.fromList (Z:.n) $ take n ks
            ts = R.fromListUnboxed (Z:.n) $ take n $ zip rs is
            cs = mkarr $ zipWith (:+) rs is
            ra = RA.fft1dP RA.Forward :: RFFTArr -> IO RFFTArr
        in  ts `seq` cs `seq`
            bgroup ("n="++show n)
              [ bench "repa-algorithms" (nfIO (ra ts))
              , bench "repa-fftw" (whnf F.fft cs) ]
  defaultMain $ map (\k -> bench_fft (2^k)) [9..13::Int]

instance NFData (R.Array R.U R.DIM1 (Double, Double)) where
  rnf arr = arr `R.deepSeqArray` ()
