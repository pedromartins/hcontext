{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
module Context.Knowledge.Relevance where

import Data.List

import Context.Prelude

class Relevant a where
  type RelevantK a :: *
  relevance :: a -> RelevantK a -> Double

sortC :: (Relevant c)
      => (a -> c) -> [a] -> [a] :↓ [h| RelevantK c |]
sortC contextfn xs =
  let sortfn c x y = compare (relevance (contextfn x) c)
                             (relevance (contextfn y) c)
  in  ContextF (\c -> sortBy (sortfn . hOccurs $ c) xs)
