{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Context.Knowledge.HList where

import Data.HList

import Context.Types

instance (HList c, HProject k c) => (:▷) k c where
  q = hProject
