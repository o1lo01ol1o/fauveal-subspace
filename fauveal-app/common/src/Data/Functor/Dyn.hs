module Data.Functor.Dyn where

import Data.Functor.Poly ()

-- type family StateFn a
--    StateFn () = Arena () ()

-- newtype Dyn state a bpos bdir = Dyn {pheno :: Lens (state, a -> state) (Arena bpos bdir)}