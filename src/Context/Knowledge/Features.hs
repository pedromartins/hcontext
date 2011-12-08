{-# LANGUAGE NoMonomorphismRestriction, GADTs, TypeFamilies,
             FlexibleInstances, TypeOperators, QuasiQuotes, RebindableSyntax #-}
module Context.Knowledge.Features where

import Data.Maybe
import Data.List
import Data.Function

import Control.Applicative

import Context.Utils
import Context.Implicit

type family FeatureType a :: *

data Feat a = a := (FeatureType a)
infixr 2 :=

data individual :> feature = individual :> (Feat feature)

π :: a -> f -> FeatureType f :↓ [h| a :> f |]
π _ _ = mkC1 $ \(_ :> (_ := v)) -> v

extractFeature (_ :> _ := v) = v

