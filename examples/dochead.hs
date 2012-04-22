-- | Example written in haddock header.

import Data.Complex
import Data.Array.Repa
import Data.Array.Repa.Eval
import Data.Array.Repa.Repr.ForeignPtr
import Data.Array.Repa.FFTW

a :: Array F DIM1 (Complex Double)
a = fromList (Z :. 5) [i :+ 0 | i <- [0..4]]

-- XXX: Cannot view 'a' when loaded in ghci.
--
-- ghci> a
-- <interactive>:118:1:
--     No instance for (Show (Array F DIM1 (Complex Double)))
--       arising from a use of `print'
--     Possible fix:
--       add an instance declaration for
--       (Show (Array F DIM1 (Complex Double)))
--     In a stmt of an interactive GHCi command: print it
