module Lambda where
import qualified Data.Map as M
import Data.Maybe
import Control.Monad.State.Strict
import Debug.Trace

data Term = Lam String Typ
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
