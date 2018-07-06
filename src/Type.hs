module Type where

import Id
import PPrint
import Token

data Type
  = TyVar Id
  | TyConst Id
  | TyApp Type Type
  deriving (Eq)

data TypePred
  = TyPred Id [Type]
  deriving (Eq)

data QType
  = QType TypePreds Type

data TypeSch
  = TySch [Id] QType

type TypePreds
  = [TypePred]

mkTyApp t ts =
  foldl TyApp t ts

mkTyFn ts t =
  foldr (\a b -> mkTyApp (TyConst IFn) [a,b]) t ts

isFactor (TyVar _) = True
isFactor (TyConst _) = True
isFactor _ = False

isType1 t
  | isFactor t = True
  | (TyConst IFn `TyApp` _) `TyApp` _ <- t = False
  | _ `TyApp` _ <- t = True
  | otherwise = False

pprintParen p t =
  if p t
  then pprint TkLParen <> pprint t <> pprint TkRParen
  else pprint t

instance PPrint Type where
  pprint (TyVar i) =
    pprint i
  pprint (TyConst i) =
    pprint i
  pprint ((TyConst IFn `TyApp` a) `TyApp` b) =
    pprintParen (not . isType1) a <+> pprint TkArrow <+> pprint b    
  pprint (TyApp t t') =
    hsep [pprint t, pprintParen (not . isFactor) t']

instance PPrint TypePred where
  pprint (TyPred p ts) =
    pprint p <+> hsep (map (pprintParen (not . isFactor)) ts)

instance PPrint TypePreds where
  pprint ps =
    pprint TkLCurly <> sepBy (pprint TkComma) (map pprint ps) <> pprint TkRCurly

instance PPrint QType where
  pprint (QType ps t) =
    pprint ps <+> pprint TkImply <+> pprint t

instance PPrint TypeSch where
  pprint (TySch as t) =
    text "\\" <> hsep (map pprint as) <> text "." <> pprint t
