module Context.Runtime.PMonad where

class PMonad m where
  return :: a -> m c c a
  (>>=) :: m c1 c2 a -> (a -> m c2 c3 b) -> m c1 c3 b
