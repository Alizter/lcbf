module SKCalculus where

import Church

data CombTerm =   CombVar Int 
                | CombApp SKTerm SKTerm 
                | CombS
                | CombK
                | CombI
                deriving (Eq)

instance Show CombTerm where
    show (CombVar n)
        | n < 0             = "(" ++ show n ++ ")"
        | otherwise         = show n
    show (CombApp t1 t2)    = "(" ++ show t1 ++ show t2 ++ ")"
    show CombS              = "S"
    show CombK              = "K"
    show CombI              = "I"
    show Comb


(%) :: SKTerm -> SKTerm -> SKTerm
a % b = SKApp a b

i = S % K % K

suc = (S % (S % (K % S) % K) %)

addition = S % i % (K % (S % (S % (K % S) % K)))


