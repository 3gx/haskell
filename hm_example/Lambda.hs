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


type Cxt = M.Map String Typ

prims :: Cxt
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

infer1 :: Cxt -> Term -> Typ
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

-- metavar store, context
data InferState = InferState {
        store :: M.Map Int Typ,
        context :: M.Map String Typ,
        var_count :: Int
     } deriving (Show, Eq, Ord)

initInferState :: InferState
initInferState = InferState mempty prims 0

runInfer :: Term -> Typ
runInfer trm = flip evalState initInferState $ resolve =<< infer2 trm

runInfer' :: Term -> (Typ, InferState)
runInfer' trm = flip runState initInferState $ resolve =<< infer2 trm

newMetaVar :: State InferState Typ
newMetaVar = do
  v <- var_count <$> get
  modify (\s -> s { var_count = v + 1})
  return $ TVar v

infer2 :: Term -> State InferState Typ
infer2 trm = case trm of
  KonstInt _ -> pure TInt
  Unit -> pure TUnit
  Var s -> instantiate =<< resolve
            . fromMaybe (error ("lookup bad var:" ++ show s))
            . M.lookup s . context =<< get
  App t1 t2 -> do
    ty1 <- infer2 t1
    ty2 <- infer2 t2
    case ty1 of
      (TArr x y) -> do
         unify2 x ty2
         return y
      (TVar y) -> do
         h <- newMetaVar
         t <- newMetaVar
         unify2 ty1 (TArr h t)
         unify2 h ty2
         return t
      _ -> error $ "applying nonarrow: " ++ show (ty1, ty2)
  Lam v t1 -> do
    mv <- newMetaVar
    modify (\s -> s { context = M.insert v mv (context s)})
    tbody <- infer2 t1
    pure $ TArr mv tbody

unify2 :: Typ -> Typ -> State InferState ()
unify2 ty1' ty2' = do
  ty1 <- resolve ty1'
  ty2 <- resolve ty2'
  case (ty1, ty2) of
    (TVar v1, _) -> modify (\s -> s { store = M.insert v1 ty2 (store s)})
    (_, TVar v2) -> modify (\s -> s { store = M.insert v2 ty1 (store s)})
    (TArr h1 t1, TArr h2 t2) -> unify2 h1 h2 >> unify2 t1 t2
    _ -> if ty1 == ty2 then return ()
                       else error $ "couldn't unify" ++ show(ty1,ty2)

-- get one type and returns the same type but with all tvar-references inside it
-- chased out
resolve' :: [Typ] -> Typ -> State InferState Typ
resolve' seen ty@(TVar v) = do
  mres <- M.lookup v . store <$> get
  case mres of
    Nothing -> return ty
    Just res' -> do
      -- occurs check
      when (res' `elem` seen) $ error ("occurs check: " ++ show(res',ty))
      res <- resolve' (res':seen) res'
      -- zonking?
      modify (\s -> s { store = M.insert v res (store s)})
      return res
resolve' seen (TArr h t) = do
  h' <- resolve' seen h
  t' <- resolve' seen t
  return $ TArr h' t'
resolve' seen (TLam v t) = TLam v <$> resolve' seen t
resolve' _seen x = return x

resolve :: Typ -> State InferState Typ
resolve = resolve' []

instantiate :: Typ -> State InferState Typ
instantiate (TLam v ty) = do
  mv <- newMetaVar
  instantiate $ substituteTy (TBVar v) mv ty
instantiate x = pure x

-- aka abstract, generalize

closeTyp :: Typ -> Typ
closeTyp ty =
  let fvs = freeVars ty
  in foldr introVar ty fvs

freeVars :: Typ -> [Int]
freeVars = undefined

introVar :: Int -> Typ -> Typ
introVar = undefined

