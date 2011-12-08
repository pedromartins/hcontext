{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, OverlappingInstances,
      UndecidableInstances, FunctionalDependencies, TypeOperators,
      TypeSynonymInstances #-}
module Context.Runtime.Runtime where

import Prelude hiding (return, (>>=), (>>))
import qualified Control.Monad as M

import Data.HList (HCons(..), HNil(..), hNil, (:*:), (.*.), Fail, TypeNotFound)

import Context.Utils
import Context.Types
import Context.Implicit
import Context.Runtime.PMonad
import Control.Arrow (first)

m1 >> m2 = m1 >>= \_ -> m2

newtype ContextRuntime c1 c2 a =
  CR { runContextRuntime :: c1 -> (a, c2) }

instance PMonad ContextRuntime where
  return x = CR $ \c -> (x, c)
  m >>= k = CR $ \c ->
    let (a, c') = runContextRuntime m c
    in  runContextRuntime (k a) c'

newtype ContextRuntimeT m c1 c2 a =
  CRT { runContextRuntimeT :: c1 -> m (a, c2) }

instance (Functor m) => Functor (ContextRuntimeT m c1 c2) where
  fmap f (CRT cf) = CRT $ fmap (first f) . cf

instance (M.Monad m) => PMonad (ContextRuntimeT m) where
  return x = CRT $ \c -> M.return (x, c)
  m >>= k = CRT $ \c -> do
    (a, c') <- runContextRuntimeT m c
    runContextRuntimeT (k a) c'

liftCRT :: Monad m => m a -> ContextRuntimeT m c c a
liftCRT m = CRT $ \c -> m M.>>= (\a -> M.return (a, c))

cfToCr :: (k :▷ cs)
       => ContextF cs a -> ContextRuntime k k a
cfToCr cf = CR $ \k -> (evalC cf k, k)

cfToCrT :: (Monad m, k :▷ cs)
        => ContextF cs a -> ContextRuntimeT m k k a
cfToCrT cf = CRT $ \k -> M.return (evalC cf k, k)
inContextT :: (Monad m, k :▷ cs)
        => ContextF cs a -> ContextRuntimeT m k k a
inContextT = cfToCrT

cfToCrT' :: (Monad m) => ContextF cs a -> ContextRuntimeT m cs cs a
cfToCrT' cf = CRT $ \k -> M.return (evalC cf k, k)

updateC :: HUpdateAtTypeOrAppend e l c2
        => e -> ContextRuntime l c2 ()
updateC c = CR $ \c' -> ((), hUpdateAtTypeOrAppend c c')

updateCT :: (HUpdateAtTypeOrAppend e l c2, Monad m)
         => e -> ContextRuntimeT m l c2 ()
updateCT c = CRT $ \c' -> M.return ((), hUpdateAtTypeOrAppend c c')

emptyC :: ContextRuntime HNil HNil ()
emptyC = CR $ \c -> ((), c)

evalCR :: ContextRuntime HNil k a -> a
evalCR ca = fst . runContextRuntime ca $ hNil

evalCRT :: (Monad m)
        => ContextRuntimeT m HNil k a -> m a
evalCRT ca = runContextRuntimeT ca hNil M.>>= M.return . fst

execCR :: ContextRuntime HNil k a -> k
execCR ca = snd . runContextRuntime ca $ hNil

runCR :: ContextRuntime HNil k a -> (a, k)
runCR ca = runContextRuntime ca hNil

runCRT :: ContextRuntimeT m HNil k a -> m (a, k)
runCRT ca = runContextRuntimeT ca hNil

pushCT :: (Monad m) => c -> ContextRuntimeT m HNil c ()
pushCT c = CRT . const . M.return $ ((), c)

joinCRT :: (Monad m) => ContextRuntimeT m c c (ContextRuntimeT m c c a)
        -> ContextRuntimeT m c c a
joinCRT mma = mma >>= id

mcfToCrT :: (Functor m, Monad m) => (m a) :↓ c -> ContextRuntimeT m c c a
mcfToCrT = joinCRT . fmap liftCRT . cfToCrT

inContextM :: (Functor m, Monad m) => (m a) :↓ c -> ContextRuntimeT m c c a
inContextM = mcfToCrT


