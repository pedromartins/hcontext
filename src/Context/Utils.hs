{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances,
             OverlappingInstances, UndecidableInstances, FunctionalDependencies,
             TypeSynonymInstances, TypeOperators, TemplateHaskell #-}
module Context.Utils where

import Data.HList
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.Exts.Parser
    (parseTypeWithMode, ParseResult(..), ParseMode(..), defaultParseMode)
import qualified Language.Haskell.Exts.Syntax as H
import Language.Haskell.Exts.Extension
import Language.Haskell.Meta.Syntax.Translate

class HUnion l1 l2 lr | l1 l2 -> lr where
    hUnion :: l1 -> l2 -> lr

instance (HDiff' l2 l1 l1 lr, HAppend l1 lr lr') => HUnion l1 l2 lr' where
    hUnion l1 l2 = hAppend l1 (hDiff' l2 l1 l1)

class HDiff' xs ys oys rs | xs ys oys -> rs where
    hDiff' :: xs -> ys -> oys -> rs

instance HDiff' HNil ys oys HNil where
    hDiff' _ ys oys = hNil

instance (HDiff' xs oys oys rs) => HDiff' (x :*: xs) HNil oys (x :*: rs) where
    hDiff' (HCons x xs) _ oys = (HCons x (hDiff' xs oys oys))

instance (HDiff' xs oys oys rs) => HDiff' (x :*: xs) (x :*: ys) oys rs where
    hDiff' (HCons x xs) (HCons x' ys) oys = hDiff' xs oys oys

instance (HDiff' (x :*: xs) ys oys rs) => HDiff' (x :*: xs) (y :*: ys) oys rs where
    hDiff' (HCons x xs) (HCons y ys) oys = hDiff' (HCons x xs) ys oys

class HUpdateAtTypeOrAppend e l r | e l -> r where
  hUpdateAtTypeOrAppend :: e -> l -> r

instance HUpdateAtTypeOrAppend e (e :*: l) (e :*: l) where
  hUpdateAtTypeOrAppend e (HCons e' l) = HCons e l

instance HUpdateAtTypeOrAppend e l lr =>
              HUpdateAtTypeOrAppend e (e' :*: l) (e' :*: lr) where
  hUpdateAtTypeOrAppend e (HCons e' l) = HCons e' (hUpdateAtTypeOrAppend e l)

instance HUpdateAtTypeOrAppend e HNil (e :*: HNil) where
  hUpdateAtTypeOrAppend e HNil = e .*. hNil

--  Quasiquoters.

h :: QuasiQuoter
h
  = QuasiQuoter { quoteExp  = undefined
                , quotePat  = undefined
                , quoteType = quoteHType
                , quoteDec  = undefined
                }

quoteHType :: String -> TypeQ
quoteHType s
  = do
      let parseMode = defaultParseMode { extensions = glasgowExts }
          parseResult = parseTypeWithMode parseMode $ "(" ++ s ++ ")"

      case parseResult of
        ParseFailed loc e -> error $ "Parse error: " ++ show (loc, e)
        ParseOk ty        -> toHListTy ty

toHListTy :: H.Type -> TypeQ
toHListTy (H.TyParen ty)
  = [t| $(return . toType $ ty) :*: HNil |]

toHListTy (H.TyTuple _ tys)
  = foldr (appT . appT (conT ''(:*:))) [t| HNil |] . map (return . toType) $ tys

toHListTy t = fail $ "Type malformed: " ++ (show t)

