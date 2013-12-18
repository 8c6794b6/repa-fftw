{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6, 2011, 2012
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : portable

Performs fft of repa array data via FFTW.

Currently supporting ('Complex' Double) arrays for dimensions 'DIM1', 'DIM2',
and 'DIM3' only.

-}

module Data.Array.Repa.FFTW
  ( -- * Examples
    -- $examples

    -- * Multi dimension functions
    -- $multi

    -- * References
    -- $references

    -- * FFT functions (1 dimension)
    fft
  , ifft
    -- * FFT functions (2 dimension)
  , fft2d
  , ifft2d
    -- * FFT functions (3 dimension)
  , fft3d
  , ifft3d
  ) where

import Data.Complex (Complex(..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (Storable(..))
import System.IO.Unsafe (unsafePerformIO)

import Data.Array.CArray (CArray)
import Data.Array.Repa ((:.)(..), Array, DIM1, DIM2, DIM3, Z(..))
import Data.Array.Repa.Repr.ForeignPtr (F)
import Foreign.Storable.Complex ()

import qualified Data.Array.CArray as C
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.ForeignPtr as RF
import qualified Math.FFT as FFT

{-$examples

Sample module:

> import Data.Complex
> import Data.Array.Repa
> import Data.Array.Repa.Eval
> import Data.Array.Repa.Repr.ForeignPtr
> import Data.Array.Repa.FFTW
>
> a :: Array F DIM1 (Complex Double)
> a = fromList (Z :. 4) [i :+ 0 | i <- [0..3]]

Loading above in ghci:

>>> toList a
[0.0 :+ 0.0,1.0 :+ 0.0,2.0 :+ 0.0,3.0 :+ 0.0]
>>> toList $ fft a
[6.0 :+ 0.0,(-2.0) :+ 2.0,(-2.0) :+ 0.0,(-2.0) :+ (-2.0)]
>>> toList $ ifft $ fft a
[0.0 :+ 0.0,1.0 :+ 0.0,2.0 :+ 0.0,3.0 :+ 0.0]

-}

{-$multi

Although FFTW library has more flexiblity, choice of dimensions in
multi-dimension FFT functions ('fft2d', 'ifft2d' ... etc) were using fixed
dimensions to perform FFTs. Those choices were made after @fft2dP@, @fft3dP@
functions from @repa-algorithms@ package.

-}

{-$references

* fftw : <http://www.fftw.org>

* fftw haskell binding : <http://hackage.haskell.org/package/fft>

* repa-algorithms: <http://hackage.haskell.org/package/repa-algorithms>

-}

-- --------------------------------------------------------------------------
-- Exposed functions

-- | Performs 1 dimension forward fft.
fft :: Array F DIM1 (Complex Double) -> Array F DIM1 (Complex Double)
fft = c2r . FFT.dft . r2c
{-# INLINE fft #-}

-- | Performs 1 dimension inverse fft.
ifft :: Array F DIM1 (Complex Double) -> Array F DIM1 (Complex Double)
ifft = c2r . FFT.idft . r2c
{-# INLINE ifft #-}

-- | Performs 2 dimension forward fft.
fft2d :: Array F DIM2 (Complex Double) -> Array F DIM2 (Complex Double)
fft2d = c2r2d . FFT.dftN [0,1] . r2c2d
{-# INLINE fft2d #-}

-- | Performs 2 dimension inverse fft.
ifft2d :: Array F DIM2 (Complex Double) -> Array F DIM2 (Complex Double)
ifft2d = c2r2d . FFT.idftN [0,1] . r2c2d
{-# INLINE ifft2d #-}

-- | Performs 3 dimension forward fft.
fft3d :: Array F DIM3 (Complex Double) -> Array F DIM3 (Complex Double)
fft3d = c2r3d . FFT.dftN [0,1,2] . r2c3d
{-# INLINE fft3d #-}

-- | Performs 3 dimension inverse fft.
ifft3d :: Array F DIM3 (Complex Double) -> Array F DIM3 (Complex Double)
ifft3d = c2r3d . FFT.idftN [0,1,2] . r2c3d
{-# INLINE ifft3d #-}

-- --------------------------------------------------------------------------
-- Guts

r2c :: Array F DIM1 (Complex Double) -> CArray Int (Complex Double)
r2c rarr = unsafePerformIO $ do
  let _:.nelem = R.extent rarr
      fptr = RF.toForeignPtr rarr
  C.unsafeForeignPtrToCArray fptr (0,nelem-1)
{-# INLINE r2c #-}

c2r :: CArray Int (Complex Double) -> Array F DIM1 (Complex Double)
c2r carr = case C.toForeignPtr carr of
  (n, fptr) -> let sh = Z:.n in
    R.computeS $ R.fromFunction sh $ \ix ->
    unsafePerformIO $ withForeignPtr fptr $ \ptr ->
    peekElemOff ptr $ R.toIndex sh ix
{-# INLINE c2r #-}

r2c2d :: Array F DIM2 (Complex Double) -> CArray (Int, Int) (Complex Double)
r2c2d rarr = unsafePerformIO $ do
    let _:.n1:.n2 = R.extent rarr
        fptr = RF.toForeignPtr rarr
    C.unsafeForeignPtrToCArray fptr ((0,0), (n1-1, n2-1))
{-# INLINE r2c2d #-}

c2r2d :: CArray (Int, Int) (Complex Double) -> Array F DIM2 (Complex Double)
c2r2d carr = case C.toForeignPtr carr of
    (n, fptr) ->
        let sh = Z:.n':.n'
            n' = ceiling $ (sqrt $ fromIntegral n :: Double)
        in  R.computeS $ R.fromFunction sh $ \ix ->
            unsafePerformIO $ withForeignPtr fptr $ \ptr ->
            peekElemOff ptr $ R.toIndex sh ix
{-# INLINE c2r2d #-}

r2c3d :: Array F DIM3 (Complex Double)
      -> CArray (Int, Int, Int) (Complex Double)
r2c3d rarr = unsafePerformIO $ do
    let _:.n1:.n2:.n3 = R.extent rarr
        fptr = RF.toForeignPtr rarr
    C.unsafeForeignPtrToCArray fptr ((0,0,0), (n1-1, n2-1, n3-1))
{-# INLINE r2c3d #-}

c2r3d :: CArray (Int, Int, Int) (Complex Double)
      -> Array F DIM3 (Complex Double)
c2r3d carr = case C.toForeignPtr carr of
    (n, fptr) ->
        let sh = Z:.n':.n':.n'
            n' = ceiling $ fromIntegral n ** (1/3 :: Double)
        in  R.computeS $ R.fromFunction sh $ \ix ->
            unsafePerformIO $ withForeignPtr fptr $ \ptr ->
            peekElemOff ptr $ R.toIndex sh ix
{-# INLINE c2r3d #-}
