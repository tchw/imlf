module Ast where

import Records
import PPrint
import Token
import Id
import Type

data AExpr
  = AInt Int
  | AVar Id
  | AApp AExpr [AExpr]
  | AFn [Id] AExpr
  | ALet Bool [ABind] AExpr
  | ACase AExpr [ABranch]

data ABranch
  = ABranch Id [Id] AExpr

data ABind
  = ABind Id AExpr

data AFunDep
  = AFunDep [Id] Id

data AOver
  = AOver Id [Id] [AFunDep] Type

data AInst
  = AInst [TypePred] Id [Id] AExpr

data AConstr
  = AConstr Id [Type]

data AData
  = AData Id [Id] [AConstr]

data AProg
  = AProg
  { _overs :: [AOver]
  , _insts :: [AInst]
  , _binds :: [ABind]
  , _datas :: [AData]
  , _main :: AExpr }

overloadedRecord def ''AProg

sepByComma xs =
  sepBy (pprint TkComma) (map pprint xs)

pprintDefs xs =
  vcat (map (\x -> pprint x <> pprint TkSemicolon) xs)

instance PPrint AExpr where
  pprint (AInt x) =
    text (show x)
  pprint (AVar i) =
    pprint i
  pprint (AApp e []) =
    pprint e
  pprint (AApp e as) =
    pprint e <> pprint TkLParen <> sepByComma as <> pprint TkRParen
  pprint (AFn as e) =
    pprint TkLParen <> sepByComma as <> pprint TkRParen <+> pprint TkArrow $$ nest 4 (pprint e)
  pprint (ALet r [] e) =
    if r then text "letrec" else pprint TkLet $$
    pprint TkIn <+> pprint e
  pprint (ALet r bs e) =
    if r then text "letrec" else pprint TkLet <+>
    nest 4 (pprintDefs (init bs) $$ pprint (last bs)) $$
    pprint TkIn <+> pprint e
  pprint (ACase e (b:bs)) =
    pprint TkCase <+> pprint e <+> pprint TkOf $$
    nest 2 (vcat (text "  " <> pprint b : map (\b -> pprint TkVBar <+> pprint b) bs))

instance PPrint ABranch where
  pprint (ABranch c [] e) =
    pprint c <+> pprint TkArrow <+> pprint e    
  pprint (ABranch c vs e) =
    pprint c <> pprint TkLParen <> sepByComma vs <> pprint TkRParen <+> pprint TkArrow <+>
    pprint e

instance PPrint ABind where
  pprint (ABind i e) =
    pprint i <+> pprint TkAssign <+> pprint e

instance PPrint AFunDep where
  pprint (AFunDep is i) =
    hsep (map pprint is) <+> pprint TkArrow <+> pprint i

instance PPrint AOver where
  pprint (AOver i vs fds t) =
    let pprintFunDeps [] =
          empty
        pprintFunDeps fds =
          pprint TkVBar <+> sepByComma fds
    in pprint TkOver <+> pprint i <+> hsep (map pprint vs) <+> 
       pprintFunDeps fds <+>
       pprint TkDoubleColon <+> pprint t

instance PPrint AInst where
  pprint (AInst ps i is e) =
    let pprintTypePreds [] =
          empty
        pprintTypePreds [p] =
          pprint p <+> pprint TkImply
        pprintTypePreds ps =
          pprint ps <+> pprint TkImply
    in pprint TkInst <+> pprintTypePreds ps <+>
    pprint i <+> hsep (map pprint is) <+>
    pprint TkAssign $$ nest 4 (pprint e)

instance PPrint AConstr where
  pprint (AConstr i []) =
    pprint i
  pprint (AConstr i ts) =
    pprint i <> pprint TkLParen <> sepByComma ts <> pprint TkRParen

instance PPrint AData where
  pprint (AData i vs (c:cs)) =
    pprint TkData <+> pprint i <+> hsep (map pprint vs) $$
    nest 4 (pprint TkAssign <+> pprint c $$ vcat (map (\c -> pprint TkVBar <+> pprint c) cs))

instance PPrint AProg where
  pprint p =
    pprintDefs (p^. #overs) $$
    pprintDefs (p^. #insts) $$
    pprintDefs (p^. #datas) $$
    pprintDefs (p^. #binds) $$
    pprint TkMain <+> pprint TkAssign $$ nest 4 (pprint (p^. #main))
