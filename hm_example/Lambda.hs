module Lambda where
import qualified Data.Map as M
import Data.Maybe
import Control.Monad.State.Strict
import Debug.Trace

data Term = Lam String Term
          | Var String
          | App Term Term
          | KonstInt Int
          | Unit
      deriving (Eq, Ord, Show)

data Typ = TLam String Typ
         | TBVar String
         | TVar Int
         | TArr Typ Typ
         | TInt
         | TUnit
      deriving (Eq, Ord, Show)

-- naive not-capture avoiding substitution
substituteTy :: Typ -> Typ -> Typ -> Typ
substituteTy from to ty' | from == ty' = to
substituteTy from to ty' = case ty' of
  (TLam v ty) -> TLam v (substituteTy from to ty)
  (TArr ty1 ty2) -> TArr (substituteTy from to ty1) (substituteTy from to ty2)
  _ -> ty'


type Ctx = M.Map String Typ

prims :: Ctx
prims = M.fromList [("+", TArr TInt (TArr TInt TInt)),
                    ("print", TArr TInt TUnit),
                    ("id", TLam "a" (TArr (TBVar "a") (TBVar "a")))]

trm_id, trm_int, trm_id_unit, trm_higher, trm_occurs :: Term

trm_id = Lam "x" (Var "x")
trm_int = KonstInt 1
trm_id_unit = App trm_id Unit
trm_higher = Lam "x" (App (Var "x") trm_int)
trm_occurs = Lam "x" (App (Var "x") (Var "x"))


-- most naive thing that does not work

unify1 :: Typ -> Typ -> Bool
unify1 x y = x == y

infer1 :: Ctx -> Term -> Typ
infer1 c t = case t of
  KonstInt _ -> TInt
  Unit -> TUnit
  Var s -> fromMaybe (error ("lookup bad var: " ++ show s)) $ M.lookup s c
  App t1 t2 ->
    let ty1 = infer1 c t1
        ty2 = infer1 c t2
    in case ty1 of
        (TArr x y) -> case unify1 x ty2 of
                         True -> y
                         False -> error $ "cannot unify : " ++ show(x,t2)
  Lam v t1 ->
    let tv = error "no type for v"
    in TArr tv (infer1 (M.insert v tv c ) t1)

-- ================================================================ --


