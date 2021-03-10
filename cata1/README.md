$ stack exec -- ghci cata1.hs
GHCi, version 8.10.4: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( cata1.hs, interpreted )
Ok, one module loaded.
*Main> v
v1     v2     valF   valF1  valF2
*Main> v1
9
*Main> v2
9
*Main> valF
21
*Main> valF2
21
*Main> valF1
21
*Main> exp
exp       exponent  expr      exprF
*Main> expr
Mult (Add (Value 1) (Value 2)) (Value 3)
*Main> exprF
MultF (AddF (ValueF 1) (ValueF 2)) (ValueF 3)
*Main>
Leaving GHCi.
