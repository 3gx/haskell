-- https://medium.com/@olxc/catamorphisms-and-f-algebras-b4e91380d134

import Debug.Trace

data Expression = Value Int
    | Add Expression Expression
    | Mult Expression Expression deriving Show

expr = Mult (Add (Value 1) (Value 2)) (Value 3)
evalExpr :: Expression -> Int
evalExpr (Value v) = v
evalExpr (Add e1 e2) = evalExpr e1 + evalExpr e2
evalExpr (Mult e1 e2) = evalExpr e1 * evalExpr e2

v1 = evalExpr expr

notReallyCata :: (Expression -> Int) -> Expression -> Int
notReallyCata evaluator e = evaluator e -- purposly not Eta reducing

v2 = notReallyCata evalExpr expr


data ExpressionF a = ValueF Int
    | AddF a a
    | MultF a a deriving Show

exprF = MultF (AddF (ValueF 1) (ValueF 2)) (ValueF 3)

instance Functor ExpressionF where
    fmap _ (ValueF a) = ValueF a
    fmap f (AddF e1 e2) = AddF (f e1) (f e2)
    fmap f (MultF e1 e2) = MultF (f e1) (f e2)

evalExprF :: ExpressionF Int -> Int
evalExprF (ValueF v) = v
evalExprF (AddF e1 e2) = e1 + e2
evalExprF (MultF e1 e2) = e1 * e2


fourtyTwo = evalExprF (ValueF 42)
five = evalExprF (AddF 2 3)
six = evalExprF (MultF 2 3)

newtype Fix f = Fx (f (Fix f))
unfix :: Fix f -> f (Fix f)
unfix (Fx x) = x
-- f1 = evalExpr(exprF)

newtype Mu a = Roll { unroll :: Mu a -> a }

regularExprF :: ExpressionF (ExpressionF (ExpressionF a))
regularExprF = MultF (AddF (ValueF 1) (ValueF 2)) (AddF (ValueF 3) (ValueF 4))

fixedExprF :: Fix ExpressionF
fixedExprF   = Fx $ MultF (Fx $ AddF (Fx $ ValueF 1) (Fx $ ValueF 2)) (Fx $ AddF (Fx $ ValueF 3) (Fx $ ValueF 4))

evalFixedExprF :: Fix ExpressionF -> Int
evalFixedExprF fixedExpr = case unfix fixedExpr of
  ValueF i -> i
  AddF fe1 fe2 -> evalFixedExprF fe1 + evalFixedExprF fe2
  MultF fe1 fe2 -> evalFixedExprF fe1 * evalFixedExprF fe2

valF = evalFixedExprF fixedExprF

almostCata :: (ExpressionF Int -> Int) -> Fix ExpressionF -> Int
almostCata eval expr = eval $ fmap (almostCata eval) (unfix expr)

valF1 = almostCata evalExprF fixedExprF

cata :: Functor f => (f a -> a) -> Fix f  -> a
cata alg expr = alg $ fmap (cata alg) (unfix expr)

valF2 = cata evalExprF fixedExprF

-- fixedExprM :: Mu ExpressionF
-- fixedExprM = undefined
-- fixedExprM   = Fx $ MultF (Fx $ AddF (Fx $ ValueF 1) (Fx $ ValueF 2)) (Fx $ AddF (Fx $ ValueF 3) (Fx $ ValueF 4))

main :: IO ()
main = do
  let _ = putStrLn $ show expr
  putStrLn "Hello, world"
