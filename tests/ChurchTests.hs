module ChurchTests where

import Church

test0 = printReduction ex0
test1 = printReduction ex1
test2 = printReduction ex2
test3 = printReduction ex3

exs = [ex0, ex1, ex2, ex3, ex4]

testExs :: [Term] -> IO()
testExs []       = do
    return ()
testExs (e : es) = do
    putStrLn $ show e
    testExs es


churchTests = do
    testExs exs
    
-- Example expressions

-- | ((λ 0) (λ (λ 2 2))) ((λ -1) (λ -2))
-- | ((λ x.y) (λ z.(λ q.z z))) ((λ f.g) (λ y.h))


ex0 :: Term
ex0 = App (App (Lam (Var 0))
                   (Lam (Lam (App (Var 2)(Var 2)))))
            (App (Lam (Var (-1)))
                   (Lam (Var (-2))))
ex1 ::  Term
ex1 = App (Lam $ Var 1) $ Var 0

ex2 :: Term
ex2 = skiL $ SKApp (SKApp S K) K

ex3 :: Term
ex3 = skiL S

x = Var 0
y = Var (-1)

ex4 :: Term
ex4 = (Lam $ Lam $ (Var 1) # (Var 2)) # x # y
