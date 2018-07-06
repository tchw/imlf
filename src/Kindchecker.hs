module Kindchecker where

import Prelude hiding (lookup)
import Control.Monad.State
import Data.Map hiding (map, foldr, (\\))
import Data.List hiding (lookup, delete, union, insert)
import PPrint hiding (empty)
import Token
import Type
import Kind
import Id

type Subs
  = Map Id Kind

type Asms
  = Subs

instance PPrint Subs where
  pprint s =
    pprint TkLCurly <>
    sepBy
    (pprint TkComma)
    (map (\(i,t) -> pprint t <+> text "/" <+> pprint i) (toList s)) <>
    pprint TkRCurly

class Kinds a where
  apply :: Subs -> a -> a
  fvs :: a -> [Id]

instance (Foldable f, Functor f, Kinds a) => Kinds (f a) where
  apply s xs =
    fmap (apply s) xs
  fvs xs =
    nub $ concat $ fmap fvs xs

instance Kinds Kind where
  apply _ KType =
    KType
  apply s k@(KVar x) =
    case lookup x s of
      Just k  -> k
      Nothing -> k
  apply s (KFn a b) =
    KFn (apply s a) (apply s b)
  fvs KType =
    []
  fvs (KVar x) =
    [x]
  fvs (KFn a b) =
    nub (fvs a ++ fvs b)

compose s2 s1 =
  apply s2 s1 `union` s2

mgu :: Kind -> Kind -> Subs
mgu (KVar x) k'@(KVar y) =
  if x == y
  then empty
  else fromList [(x, k')]
mgu KType KType =
  empty
mgu (KVar x) k' =
  if elem x (fvs k')
  then undefined
  else fromList [(x, k')]
mgu k k'@(KVar _) =
  mgu k' k
mgu (KFn a b) (KFn a' b') =
  let u1 = mgu a a'
      u2 = mgu (apply u1 b) (apply u1 b')
  in u2 `compose` u1

type Infer
  = State Int

newId :: Infer Id
newId =
  do n <- get
     put (n + 1)
     return $ Id (show n)

mkTyVar =
  do b <- newId
     return (b, KVar b)

inferId a x =
  case lookup x a of
    Just k ->
      do return (empty,k)
    Nothing ->
      do undefined

infer :: Asms -> Type -> Infer (Subs,Kind)
infer a (TyVar x) =
  do inferId a x
infer a (TyConst x) =  
  do inferId a x
infer a (TyApp t []) =
  do infer a t
infer a (TyApp t ts) =
  do (s1,k1) <- infer a (TyApp t (init ts))
     (s2,k2) <- infer (apply s1 a) (last ts)
     (_,bk) <- mkTyVar
     let v = mgu (apply s2 k1) (KFn k2 bk)
     return (v `compose` s2 `compose` s1, apply v bk)

foo =
  let kVar x = KVar (Id x)
      tyVar x = TyVar (Id x)
      a = [(Id "f",kVar "0"),(Id "a",KFn (kVar "1") (kVar "2"))]
      t = TyApp (tyVar "f") [tyVar "a"]
      (s,k) = evalState (infer (fromList a) t) 3
  in do putStrLn $ render $ pprint s
        putStrLn $ show k
