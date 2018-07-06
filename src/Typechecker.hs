{-# LANGUAGE MultiWayIf #-}

module Typechecker where

import Data.Char (isUpper)

import Prelude hiding (lookup)
import Control.Monad.State
import Data.Graph
import Data.Map hiding (map, foldr, (\\), valid, filter)
import Data.List hiding (lookup, delete, union, insert, sort)
import Records hiding (get)
import PPrint hiding (empty)
import Token
import Ast
import Type
import Id

type Subs
  = Map Id Type

type Asms
  = Map Id TypeSch

data PHol
  = PHol Id TypePred

instance PPrint Subs where
  pprint s =
    pprint TkLCurly <>
    sepBy
    (pprint TkComma)
    (map (\(i,t) -> pprint i <+> pprint TkArrow <+> pprint t) (toList s)) <>
    pprint TkRCurly

instance PPrint Asms where
  pprint a =
    pprint TkLCurly <>
    sepBy
    (pprint TkComma)
    (map (\(i,t) -> pprint i <+> pprint TkColon <+> pprint t) (toList a)) <>
    pprint TkRCurly  

class Types a where
  apply :: Subs -> a -> a
  fvs :: a -> [Id]

instance (Foldable f, Functor f, Types a) => Types (f a) where
  apply s xs =
    fmap (apply s) xs
  fvs xs =
    nub $ concat $ fmap fvs xs

instance Types Type where
  apply s t@(TyVar x) =
    case lookup x s of
      Just t  -> t
      Nothing -> t
  apply s t@(TyConst _) =
    t
  apply s (TyApp t t') =
    TyApp (apply s t) (apply s t')
  fvs (TyVar x) =
    [x]
  fvs (TyConst _) =
    []
  fvs (TyApp t t') =
    nub (fvs t ++ fvs t')

instance Types TypePred where
  apply s (TyPred i ts) =
    TyPred i (apply s ts)
  fvs (TyPred i ts) =
    fvs ts

instance Types QType where
  apply s (QType ps t) =
    QType (apply s ps) (apply s t)
  fvs (QType ps t) =
    nub (fvs ps ++ fvs t)

-- instance Types TypeSch where
--   apply s (TySch as t) =
--     TySch as (apply (foldr delete s as) t)
--   fvs (TySch as t) =
--     fvs t \\ as

applyAsms :: Subs -> Asms -> Infer Asms
applyAsms s a =
  do let applyTySch s (TySch as p) =
           do (bs,bts) <- mkTyVars (length as)
              return $ TySch bs (apply (s `compose` fromList (zip as bts)) p)
     mapM (applyTySch s) a

fvsAsms a =
  let fvsTySch (TySch as t) =
        fvs t \\ as
  in nub $ concat $ fmap fvsTySch a

instance Types PHol where
  apply s (PHol i p) =
    PHol i (apply s p)
  fvs (PHol _ p) =
    fvs p

compose s2 s1 =
  apply s2 s1 `union` s2

mgu (TyVar x) t'@(TyVar y) =
  if x == y
  then empty
  else fromList [(x, t')]
mgu (TyConst c) (TyConst c') =
  if c == c'
  then empty
  else undefined
mgu (TyVar x) t' =
  if elem x (fvs t')
  then undefined
  else fromList [(x, t')]
mgu t t'@(TyVar _) =
  mgu t' t
mgu (TyApp t1 t2) (TyApp t1' t2') =
  let u1 = mgu t1 t1'
      u2 = mgu (apply u1 t2) (apply u1 t2')
  in u2 `compose` u1
mgu _ _ =
  undefined

type Infer
  = State (Int,[AInst])

newId :: String -> Infer Id
newId p =
  do (n,is) <- get
     put (n + 1,is)
     return $ Id (p ++ show n)

mkVar f p =
  do i <- newId p
     return (i, f i)

mkVars f p n =
  do xs <- replicateM n (mkVar f p)
     return $ unzip xs

mkAVars =
  mkVars AVar "i"

mkTyVar =
  mkVar TyVar "t"

mkTyVars =
  mkVars TyVar "t"

mkTySch vs t =
  TySch vs $ QType [] t

spreadE :: TypePreds -> Subs
spreadE ps =
  let hasE (TyPred IAp [t1,t2,t3,TyVar _]) =
        let isE (TyConst IE) = True
            isE _ = False
        in isE t1 || isE t2 || isE t3
      hasE _ =
        False
  in case find hasE ps of
       Just (TyPred _ [_,_,_,TyVar v]) ->
         let s = fromList [(v,TyConst IE)]
         in spreadE (apply s ps) `compose` s
       Nothing ->
         empty

closure :: TypePreds -> [Id] -> [Id]
closure ps is =
  let closurePred is (TyPred IAp [t1,t2,t3,t4]) =
        if any (\t -> (is `intersect` fvs t) /= []) [t1,t2,t3]
        then fvs t4
        else []
      closurePred _ _ =
        []
      is' = nub (is ++ concat (map (closurePred is) ps))
  in if is' == is
     then is
     else closure ps is'

defaultToP :: Bool -> TypePreds -> Type -> Subs
defaultToP isMain ps t =
  let isApOrPack (TyPred IAp _) = True
      isApOrPack (TyPred IPack _) = True
      isApOrPack _ = False
      is =
        if isMain
        then []
        else closure ps (fvs t)
  in fromList $ zip
     (fvs (filter isApOrPack ps) \\ is)
     (repeat (TyConst IP))

impSubs :: Bool -> TypePreds -> Type -> Subs
impSubs isMain ps t =
  let s1 = spreadE ps
  in defaultToP isMain (apply s1 ps) (apply s1 t) `compose` s1

infer :: Asms -> AExpr -> Infer ([PHol], Subs, Type, AExpr)
infer _ e@(AInt x) =
  do return ([], empty, TyConst $ Id "Int", e)
infer a e@(AVar x) =
  case lookup x a of
    Just (TySch as (QType ps t)) ->
      do (_,bts) <- mkTyVars (length as)
         let s = fromList (zip as bts)
         (is,ies) <- mkAVars (length ps)
         return ( zipWith PHol is (apply s ps)
                , empty
                , apply s t
                , AApp e ies )
    Nothing ->
      do undefined
infer a (AApp e []) =
  do infer a e
infer a (AApp e es) =
  do (p1,s1,t1,e1') <- infer a (AApp e (init es))
     a1 <- applyAsms s1 a
     (p2,s2,t2,e2') <- infer a1 (last es)
     (_,bt) <- mkTyVar
     let v = mgu (apply s2 t1) (mkTyFn [t2] bt)
     return ( apply v (apply s2 p1 ++ p2)
            , v `compose` s2 `compose` s1
            , apply v bt
            , case e1' of
                AApp e' es' -> AApp e' (es' ++ [e2'])
                _ -> AApp e1' [e2'] )
infer a (AFn [] e) =
  do infer a e
infer a (AFn as e) =
  do (_,bt) <- mkTyVar
     (p1,s1,t1,e') <- infer (insert (head as) (mkTySch [] bt) a) (AFn (tail as) e)
     return ( p1
            , s1
            , mkTyFn [apply s1 bt] t1
            , case e' of
                AFn as' e' -> AFn (head as:as') e'
                _ -> AFn [head as] e' )
infer a (ALet _ [] e) =
  do infer a e
infer a (ALet False ((ABind x e1):bs) e2) =
  do let reduce ps =
           let isTyVar (TyVar _) = True
               isTyVar _ = False
               isPredVar (TyPred i ts) = any isTyVar ts
           in filter isPredVar (nub ps)
         mkMthArg (TyPred i ts) =
           let extract (TyApp t _) = extract t
               extract (TyConst i) = i
               extract (TyVar a) = a
           in IMth i (map extract ts)
         mkMth p =
           do -- let Just _ = find (\(AInst _ (TyPred i' (TyApp c' _:_)) _) ->
              --                       i == i' && c == c') is
              return $ AVar $ mkMthArg p
     (phs1,s1,t1,e1') <- infer a e1
     let ps1 = [p | PHol _ p <- phs1]
         is1 = [i | PHol i _ <- phs1]
         sImp = impSubs (x == Id "main") (reduce ps1) t1
         ps1_ = apply sImp ps1
         ps1r_ = reduce ps1_
         s1_ = sImp `compose` s1
         t1_ = apply sImp t1
     a1_ <- applyAsms s1_ a
     let q1_ = (QType ps1r_ t1_)
         o1_ = TySch (fvs q1_ \\ fvsAsms a1_) q1_
     (phs2,s2,t2,e2') <- infer
       (insert x o1_ a1_)
       (ALet False bs e2)
     ms <- mapM mkMth ps1_
     return ( phs2
            , s2 `compose` s1_
            , t2
            , let b' =
                    let addFn =
                          case map mkMthArg ps1r_ of
                            [] -> id
                            mas -> AFn mas
                        addLet =
                          case is1 of
                            [] -> id
                            _ -> ALet False (zipWith ABind is1 ms)
                    in ABind x . addFn . addLet $ e1'
              in case e2' of
                   ALet _ bs' e2' -> ALet False (b':bs') e2'
                   _ -> ALet False [b'] e2' )
infer a (ALet True bs e) =
  do let n = length bs
     (as,ats) <- mkTyVars n
     let fixts = [mkTySch as (mkTyFn [mkTyFn ats r | r <- ats] r) | r <- ats]
         fixis = [IFix n k | k <- [1..n]]
         is = [i | ABind i _ <- bs]
         es = [e | ABind _ e <- bs]
         fixEs = [AApp (AVar fixi) [AFn is e | e <- es] | fixi <- fixis]
     infer (fromList (zip fixis fixts) `union` a) (ALet False (zipWith ABind is fixEs) e)
infer a (ACase e bs) =
  do let mkCaseFn (ABranch c vs e') =
           AFn vs e'
     infer a (AApp (AVar (ICase [c | ABranch c _ _ <- bs])) (e : map mkCaseFn bs))

sort :: AExpr -> AExpr
sort (ALet True bs e) =
  let fvs (AInt _) =
        []
      fvs (AVar i) =
        [i]
      fvs (AApp e es) =
        nub (concat (map fvs (e:es)))
      fvs (AFn as e) =
        fvs e \\ as
      fvs (ALet r bs e) =
        let is = [i | ABind i _ <- bs]
            es = [e | ABind _ e <- bs]
        in if r
           then nub (concat $ map fvs (e:es)) \\ is
           else nub (concat (map fvs es) ++ (fvs e \\ is))
      fvs (ACase e bs) =
        fvs e ++ nub (concat (map (\(ABranch _ is e) -> fvs e \\ is) bs))
      mkNode (ABind i e) =
        let e' = sort e
        in (ABind i e', i, fvs e' `intersect` [i | ABind i _ <- bs])
      mkLet (AcyclicSCC b) e =
        ALet False [b] e
      mkLet (CyclicSCC bs) e =
        ALet True bs e
  in foldr mkLet e $ stronglyConnComp (map mkNode bs)
sort e =
  e

typecheck :: Int -> AProg -> ([PHol],Subs,Type,AExpr,Int)
typecheck g prog =
  let overToAsm (AOver i vs _ t) =
        do return (i, TySch (vs ++ fvs t) $ QType [TyPred i (map TyVar vs)] t)
      dataToAsms (AData i vs cs) =
        do let t =
                 mkTyApp (TyConst i) (map TyVar vs)
               mkConstrAsm (AConstr c ts) =
                 do return ( c
                           , mkTySch vs (mkTyFn ts t) )
               mkCaseAsm =
                 do (b,bt) <- mkTyVar
                    return ( ICase [c | AConstr c _ <- cs]
                           , mkTySch (b:vs) $
                             mkTyFn (t : map (\(AConstr _ ts) -> mkTyFn ts bt) cs) bt)

           cas <- mapM mkConstrAsm cs
           ca <- mkCaseAsm
           return (cas ++ [ca])
      mkWriteAsm =
        do (f,ft) <- mkTyVar
           (a,at) <- mkTyVar
           return ( Id "print"
                  , TySch [f,a] $
                    QType [TyPred IPack [ft]]
                    (ft `TyApp` mkTyFn [at] (TyConst IE `TyApp` TyConst (Id "Unit"))) )
      e =
        sort $ ALet True
        ((prog^. #binds) ++ [ABind (Id "main") (prog^. #main)])
        (AVar (Id "main"))
      m =
        do oas <- mapM overToAsm (prog^. #overs)
           dass <- mapM dataToAsms (prog^. #datas)
           printAs <- mkWriteAsm
           let intT = TyConst (Id "Int")
               boolT = TyConst (Id "Bool")
               arithT = mkTySch [] $ mkTyFn [intT,intT] intT
               relT = mkTySch [] $ mkTyFn [intT,intT] boolT
               arithAs =
                 [ (IAdd, arithT)
                 , (ISub, arithT)
                 , (IMul, arithT)
                 , (IDiv, arithT)
                 , (IEq, relT) ]
           infer (fromList (oas ++ concat dass ++ arithAs ++ [printAs])) e
      ((phs,s,t,e'),(g',_)) = runState m (g,prog^. #insts)
  in (phs,s,t,e',g')
