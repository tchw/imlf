module Effects where

import Control.Monad.State
import Data.List
import Records hiding (get)
import Ast
import Id
import Type

addDefs :: AProg -> AProg
addDefs prog =
  let f = Id "f"
      ft = TyVar f
      a = Id "a"
      at = TyVar a

      -- over ap f g h i :: f (a -> g b) -> h a -> i b
      overAp =
        let 
            g = Id "g"
            h = Id "h"
            i = Id "i"
            gt = TyVar g
            ht = TyVar h
            it = TyVar i
            bt = TyVar (Id "b")
        in AOver IAp [f,g,h,i] [] $
           mkTyFn [ft `TyApp` mkTyFn [at] (gt `TyApp` bt), ht `TyApp` at] (it `TyApp` bt)

      -- over pack f :: a -> f a
      overPack =
        AOver IPack [f] [] $
        mkTyFn [at] (ft `TyApp` at)

      -- data P a = P(a)
      dataP =
        AData IP [a] [AConstr IP [at]]

      -- data E a = E(a)      
      dataE =
        AData IE [a] [AConstr IE [at]]
  in over #datas ([dataP,dataE] ++) $
     over #overs ([overAp,overPack] ++) prog

type Trans
  = State Int

newId :: Trans Id
newId =
  do i <- get
     put (i + 1)
     return $ Id ("i" ++ show i)

translateM :: AProg -> Trans AProg
translateM prog =
  let vPack =
        AVar IPack
      vAp =
        AVar IAp
      transConstr (AConstr i ts) =
        do is <- replicateM (length ts) newId
           return $ ABind (ITrans i) $ foldr
             (\i e -> AApp vPack [AFn [i] e])
             (AApp vPack [AApp (AVar i) (map AVar is)])
             is
      transData (AData _ _ cs) =
        do mapM transConstr cs
      transExpr cs is e@(AInt _) =
        do return $ AApp vPack [e]
      transExpr cs is e@(AVar i)
        | elem i is = return $ AApp vPack [e]
        | elem i cs = return $ AVar (ITrans i)
        | otherwise = return e
      transExpr cs is (AApp e es) =
        do e' <- transExpr cs is e
           es' <- mapM (transExpr cs is) es
           return $ foldl (\e1 e2 -> AApp vAp [e1,e2]) e' es'
      transExpr cs is (AFn as e) =
        do e' <- transExpr cs (is `union` as) e
           return $ foldr
             (\i e -> AApp vPack [AFn [i] e])
             e'
             as
      transExpr cs is (ALet r bs e) =
        do bs' <- mapM (transBind cs is) bs
           e' <- transExpr cs is e
           return $ ALet r bs' e'
      transExpr cs is (ACase e bs) =
        -- case e of ... => ap(pack((x) -> case x of ...),e')
        do e' <- transExpr cs is e
           bs' <- mapM (transBranch cs is) bs
           i <- newId
           return $ AApp vAp [AApp vPack [AFn [i] (ACase (AVar i) bs')],e']
      transBranch cs is (ABranch i ps e) =
        do e' <- transExpr cs (is `union` ps) e
           return $ ABranch i ps e'
      transBind cs is (ABind i e) =
        do e' <- transExpr cs is e
           return $ ABind i e'
      transOp i =
        do a <- newId
           b <- newId
           return $ ABind (ITrans i) $
             AApp vPack [
             AFn [a] (AApp vPack [AFn [b] (AApp vPack [AApp (AVar i) [AVar a, AVar b]])]) ]
  in do let datas' =
              [AData (Id "Bool") [] [AConstr (Id "True") [], AConstr (Id "False") []]] ++
              (prog^. #datas)
        dataBss <- mapM transData datas'
        let ctorIs =
              [i | ABind (ITrans i) _ <- concat dataBss] ++
              [IAdd,ISub,IMul,IDiv,IEq]
        opBs <- mapM transOp [IAdd,ISub,IMul,IDiv,IEq]
        bs <- mapM (transBind ctorIs []) (prog^. #binds)
        mainE <- transExpr ctorIs [] (prog^. #main)
        return AProg { _overs = prog^. #overs
                     , _insts = prog^. #insts
                     , _binds = concat dataBss ++ opBs ++ bs
                     , _datas = datas'
                     , _main = mainE }

translate g prog =
  runState (translateM prog) g
