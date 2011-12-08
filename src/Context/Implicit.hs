{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts,
       UndecidableInstances, OverlappingInstances, TypeOperators,
       QuasiQuotes, NoMonomorphismRestriction, GeneralizedNewtypeDeriving #-}
module Context.Implicit where

import Control.Arrow
import Control.Applicative
import Data.List
import Data.HList
import Data.Monoid

import Context.Types
import Context.Utils

newtype ContextF c a = ContextF {runContextF :: c -> a}
  deriving (Functor, Applicative, Monad)

type a :↓ c = ContextF c a

instance BifunctorMixed (->) where
  mbimap f g = \x -> f . x . g

instance BifunctorMixed ContextF where
  mbimap f g (ContextF x) = ContextF (mbimap f g x)

(<^*>) :: ( BifunctorMixed f , Applicative (f cr) , HUnion c1 c2 cr
          , cr :▷ c1 , cr :▷ c2)
      => f c1 (a -> b) -> f c2 a -> f cr b
af <^*> ax = mbimap id q af <*> mbimap id q ax

infixl 4 <^*>

liftAP2 :: (HUnion c1 c2 cr, cr :▷ c1, cr :▷ c2)
        => (a -> b -> c) -> a :↓ c1 -> b :↓ c2 -> c :↓ cr
liftAP2 f a b = f <$> a <^*> b
liftAP3 f a b c = f <$> a <^*> b <^*> c

comp :: (HList c1, HList c2 , HUnion c1 c2 cr , cr :▷ c1, cr :▷ c2)
     => (b -> c) :↓ c2 -> (a -> b) :↓ c1
     -> (a -> c) :↓ cr
comp g f = ContextF $
  \cr -> evalC g cr . evalC f cr

evalC :: (c2 :▷ c1) => a :↓ c1 -> c2 -> a
evalC ca k = ca `runContextF` q k
(⋆) = evalC

mkC0 :: a -> a :↓ HNil
mkC0 = ContextF . const

mkC1 :: (c -> a) -> a :↓ [h| c |]
mkC1 f = ContextF (f . hHead)
ι1 = mkC1

mkC :: ( HUnion cs [h| c |] cr , cr :▷ cs , cr :▷ [h| c |] )
    => (c -> a :↓ cs) -> a :↓ cr
mkC = comb . mkC1
  where
    comb :: ( HUnion c1 c2 cr , cr :▷ c1 , cr :▷ c2)
         => (a :↓ c1 :↓ c2) -> (a :↓ cr)
    comb cca = ContextF $ \k -> (cca `evalC` k) `evalC` k
ι = mkC
