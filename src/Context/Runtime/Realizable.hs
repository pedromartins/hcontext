{-# LANGUAGE TypeFamilies, TypeOperators #-}
module Context.Runtime.Realizable where

import Prelude hiding ((>>=), (>>))
import Control.Applicative
import Context.Prelude

import Data.HList hiding ((>>=), (>>))

class Realizable c where
  realize :: ContextRuntimeT IO HNil c ()
  realizeCF :: a :↓ c -> ContextRuntimeT IO HNil c a
  fetch :: IO c

  realize = liftCRT fetch >>= pushCT
  realizeCF x = liftCRT fetch >>= pushCT >> cfToCrT x

instance Realizable HNil where
  fetch = Prelude.return hNil

instance (HList cs, Realizable c, Realizable cs)
          => Realizable (HCons c cs) where
  fetch = hCons <$> fetch <*> fetch

-- class RealizableWith c where
--   type Cfg c :: *
--   realizeWith :: Cfg c -> a :↓ c -> IO a
--   fetchWith :: Cfg c -> IO c
--
--   realizeWith cfg x = fmap (runContextF x) (fetchWith cfg)
