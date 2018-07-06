module Lambda where

import Prelude hiding (showParen)
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.List
import Id
import Ast

data LExpr
  = LInt Int
  | LVar Id
  | LApp Bool LExpr LExpr
  | LAbs Id LExpr
  | LCase [Id] LExpr [LExpr]
  | LFix Int Int [LExpr]
  | LPrint LExpr
  deriving (Eq)

mkLApp =
  foldl (LApp False)

toLambda :: AExpr -> LExpr
toLambda (AInt x) =
  LInt x
toLambda (AVar i) =
  LVar i
toLambda (AApp e []) =
  toLambda e
toLambda (AApp (AVar (ICase is)) (e:es)) =
  LCase is (toLambda e) (map toLambda es)
toLambda (AApp (AVar (IFix n k)) es) =
  LFix n k (map toLambda es)
toLambda (AApp e es) =
  mkLApp (toLambda e) (map toLambda es)
toLambda (AFn [] e) =
  toLambda e
toLambda (AFn is e) =
  foldr LAbs (toLambda e) is
toLambda (ALet _ [] e) =
  toLambda e
toLambda (ALet False ((ABind i e1):bs) e2) =
  mkLApp (LAbs i (toLambda (ALet False bs e2))) [toLambda e1]
toLambda (ACase e bs) =
  LCase
  [i | ABranch i _ _ <- bs] (toLambda e)
  [toLambda (AFn is e) | ABranch _ is e <- bs]

addDefs :: LExpr -> LExpr
addDefs e =
  let mkVar s = LVar (Id s)
      mkAbs :: [String] -> LExpr -> LExpr
      mkAbs as e = foldr (LAbs . Id) e as
      mkApp = LApp False
      mkStrictApp = LApp True
      appUnpP e = LVar IUnpP `mkApp` e
      appUnpE e = LVar IUnpE `mkApp` e
      vf = mkVar "f"
      vx = mkVar "x"
      vo = mkVar "o"
      vP = LVar IP
      vE = LVar IE
      apI is =
        IMth IAp is
      packI i =
        IMth IPack [i]
      ap = mkAbs ["o","f","x"] $
        -- (o f) x
        (vo `mkApp` vf) `mkApp` vx
      pack = mkAbs ["o","x"] $
        -- o x
        vo `mkApp` vx
      packP = mkAbs ["x"] $
        -- P x
        vP `mkApp` vx
      packE = mkAbs ["x"] $
        -- E x
        vE `mkApp` vx
      unpP = mkAbs ["x"] $
        -- case x of P(x) -> x
        LCase [IP] vx [LAbs (Id "x") vx]
      unpE = mkAbs ["x"] $
        -- case x of E(x) -> x
        LCase [IE] vx [LAbs (Id "x") vx]        
      apPPPP = mkAbs ["f","x"] $
        -- (appUnpP f) (appUnpP x)
        appUnpP vf `mkApp` appUnpP vx
      apPPPE = mkAbs ["f","x"] $
        -- E (appUnpP ((appUnpP f) (appUnpP x)))
        vE `mkApp` (appUnpP (appUnpP vf `mkApp` appUnpP vx))
      apPPEE = mkAbs ["f","x"] $
        -- E (appUnpP ((appUnpP f) ! (appUnpE x)))
        vE `mkApp` (appUnpP (appUnpP vf `mkStrictApp` appUnpE vx))
      apPEPE = mkAbs ["f","x"] $
        -- (appUnpP f) (appUnpP x)
        appUnpP vf `mkApp` appUnpP vx
      apPEEE = mkAbs ["f","x"] $
        -- (appUnpP f) ! (appUnpE x)
        appUnpP vf `mkStrictApp` appUnpE vx
      apEPPE = mkAbs ["f","x"] $
        -- E (appUnpP (((\x.x) ! (appUnpE f)) (appUnpP x)))
        vE `mkApp` (appUnpP ((mkAbs ["x"] vx `mkStrictApp` appUnpE vf) `mkApp` appUnpP vx))
      apEPEE = mkAbs ["f","x"] $
        -- E (appUnpP (((\x.x) ! (appUnpE f)) ! (appUnpE x)))
        vE `mkApp` (appUnpP ((mkAbs ["x"] vx `mkStrictApp` appUnpE vf) `mkStrictApp` appUnpE vx))
      apEEPE = mkAbs ["f","x"] $
        -- ((\x.x) ! (appUnpE f)) (appUnpP x)
        (mkAbs ["x"] vx `mkStrictApp` appUnpE vf) `mkApp` appUnpP vx
      apEEEE = mkAbs ["f","x"] $
        -- ((\x.x) ! (appUnpE f)) ! (appUnpP x)
        (mkAbs ["x"] vx `mkStrictApp` appUnpE vf) `mkStrictApp` appUnpP vx
      print_ = mkAbs ["pack-t"] $
        -- pack (\x -> E (print# x))
        (LVar IPack `mkApp` mkVar "pack-t") `mkApp` mkAbs ["x"] (vE `mkApp` LPrint vx)
  in foldr (\(mthI,mthE) e -> LApp False (LAbs mthI e) mthE) e
     [ (IAp, ap)
     , (IPack, pack)
     , (packI IP, packP)
     , (packI IE, packE)
     , (IUnpP, unpP)
     , (IUnpE, unpE)
     , (apI [IP,IP,IP,IP], apPPPP)
     , (apI [IP,IP,IP,IE], apPPPE)
     , (apI [IP,IP,IE,IE], apPPEE)
     , (apI [IP,IE,IP,IE], apPEPE)
     , (apI [IP,IE,IE,IE], apPEEE)
     , (apI [IE,IP,IP,IE], apEPPE)
     , (apI [IE,IP,IE,IE], apEPEE)
     , (apI [IE,IE,IP,IE], apEEPE)
     , (apI [IE,IE,IE,IE], apEEEE)
     , (Id "print", print_) ]

isFactor (LInt _) =
  True
isFactor (LVar _) =
  True
isFactor _ =
  False

showParen x =
  if isFactor x
  then show x
  else "(" ++ show x ++ ")"

instance Show LExpr where
  show (LInt x) =
    show x
  show (LVar i) =
    show i    
  show (LApp _ e1@(LApp _ _ _) e2) =
    show e1 ++ " " ++ showParen e2
  show (LApp strict e1 e2) =
    showParen e1 ++ if strict then " !" else " " ++ showParen e2
  show (LAbs i e) =
    "\\" ++ show i ++ "." ++ show e
  show (LCase is e es) =
    show $ mkLApp (LVar (ICase is)) (e:es)
  show (LFix n k es) =
    show $ mkLApp (LVar (IFix n k)) es
  show (LPrint e) =
    "print# " ++ showParen e

type Eval
  = StateT Int IO

newId :: Eval Id
newId =
  do i <- get
     put (i + 1)
     return $ Id ("i" ++ show i)

occur :: Id -> LExpr -> Bool
occur _ (LInt _) =
  False
occur v (LVar w) =
  v == w
occur v (LApp _ e1 e2) =
  occur v e1 || occur v e2
occur v (LAbs w e) =
  if v == w
  then False
  else occur v e
occur v (LCase _ e es) =
  any (occur v) (e:es)
occur v (LFix _ _ es) =
  any (occur v) es
occur v (LPrint e) =
  occur v e

subs :: Id -> LExpr -> LExpr -> Eval LExpr
subs _ _ (LInt x) =
  return $ LInt x
subs x t (LVar v) =
  if v == x
  then return t
  else return $ LVar v
subs x t (LApp strict e1 e2) =
  do e1' <- subs x t e1
     e2' <- subs x t e2
     return $ LApp strict e1' e2'
subs x t (LAbs v e)
  | x == v    = do return $ LAbs v e
  | occur v t = do w <- newId
                   t' <- subs v (LVar w) t
                   e' <- subs x t' e
                   return $ LAbs v e'
  | otherwise = do e' <- subs x t e
                   return $ LAbs v e'
subs x t (LCase is e es) =
  do e' <- subs x t e
     es' <- mapM (subs x t) es
     return $ LCase is e' es'
subs x t (LFix n k es) =
  do es' <- mapM (subs x t) es
     return $ LFix n k es'
subs x t (LPrint e) =
  do e' <- subs x t e
     return $ LPrint e'

infinity = 1000000000

reduce1 :: LExpr -> Eval LExpr
reduce1 (LApp strict (LAbs x e1) e2) =
  if strict
  then do e2' <- reduceNf infinity e2
          subs x e2' e1
  else do subs x e2 e1
reduce1 (LApp _ (LApp _ (LVar IEq) e1) e2) =
  do LInt a <- reduceNf infinity e1
     LInt b <- reduceNf infinity e2
     return $ LVar $ Id (if a == b then "True" else "False")
reduce1 (LApp _ (LApp _ (LVar IAdd) e1) e2) =
  do LInt a <- reduceNf infinity e1
     LInt b <- reduceNf infinity e2
     return $ LInt (a + b)
reduce1 (LApp _ (LApp _ (LVar ISub) e1) e2) =
  do LInt a <- reduceNf infinity e1
     LInt b <- reduceNf infinity e2
     return $ LInt (a - b)
reduce1 (LApp _ (LApp _ (LVar IMul) e1) e2) =
  do LInt a <- reduceNf infinity e1
     LInt b <- reduceNf infinity e2
     return $ LInt (a * b)
reduce1 (LApp _ (LApp _ (LVar IDiv) e1) e2) =
  do LInt a <- reduceNf infinity e1
     LInt b <- reduceNf infinity e2
     return $ LInt (a `div` b)
reduce1 (LApp strict e1 e2) =
  do e1' <- reduce1 e1
     return $ LApp strict e1' e2
reduce1 (LCase is e cs) =
  do e' <- reduce1 e
     let asData (LVar i) es =
           if elem i is
           then Just (i,es)
           else Nothing
         asData (LApp _ e1 e2) es =
           asData e1 (e2:es)
         asData _ _ =
           Nothing
     case asData e' [] of
       Just (i,es) ->
         case findIndex (==i) is of
           Just i ->
             do return $ mkLApp (cs!!i) es
           Nothing ->
             do undefined
       Nothing ->
         do return $ LCase is e' cs
reduce1 (LFix n k es) =
  do return $ mkLApp (es!!(k - 1)) [LFix n i es | i <- [1..n]]
reduce1 (LPrint e) =
  do e' <- reduceNf infinity e
     lift $ putStrLn (show e')
     return $ LVar (Id "Unit")
reduce1 e =
  do return e

reduceNf 0 e =
  do return e
reduceNf n e =
  let asData (LVar i) es =
        if elem i [IAdd, ISub, IMul, IDiv, IEq]
        then Nothing
        else Just (i,es)
      asData (LApp _ e1 e2) es =
        asData e1 (e2:es)
      asData _ _ =
        Nothing
  in case asData e [] of
       Just (i,es) ->
         do es' <- mapM (reduceNf n) es
            return (mkLApp (LVar i) es')
       Nothing ->
         case e of
           LInt _ ->
             do return e
           LVar _ ->
             do return e
           LAbs _ _ ->
             do return e
           _ ->
             do e' <- reduce1 e
                reduceNf (n - 1) e'

reduce n e cnt =
  evalStateT (reduceNf n e) cnt

-- reduceN 0 e =
--   return (e,0)
-- reduceN n e =
--   case asData e [] of
--     Just (i,es) ->
--       do let reduceNEs n [] =
--                do return ([],0)
--              reduceNEs n (e:es) =
--                do (e',n') <- reduceN n e
--                   (es',ns') <- reduceNEs (n - n') es
--                   return (e':es',n' + ns')
--          (es',n') <- reduceNEs n es
--          return (mkLApp (LVar i) es',n')
--     Nothing ->
--       case e of
--         LInt _ ->
--           do return (e,0)
--         LVar _ ->
--           do return (e,0)
--         LAbs _ _ ->
--           do return (e,0)
--         _ ->
--           do e' <- reduce1 e
--              (e'',n'') <- reduceN (n - 1) e'
--              return (e'',n'' + 1)

-- reduce n e cnt =
--   let (e',n') = evalState (reduceN n e) cnt
--   in e'
