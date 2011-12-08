{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
module Context.Types where

class BifunctorMixed f where
  mbimap :: (a -> b) -> (d -> c) -> f c a -> f d b

class k :▷ c where
  q :: k -> c

instance (:▷) c c where
  q = id
