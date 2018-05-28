{-# LANGUAGE ViewPatterns #-}
module Church where

import Data.List as List
import Data.List.Split
import Data.Tree hiding (Tree )

-- De Bruijin indicies
   -- This is easier to work with as aloha-conversion done automatically
data Term = Var Int
            | Lam Term
            | App Term Term
    deriving (Eq)

instance Show Term where
    show (Var n)
        | n < 0             = "(" ++ show n ++ ")"
        | otherwise         = show n
    show (Lam t) = "\\ " ++ show t
    show (App t1 t2) = "(" ++ show t1 ++ show t2 ++ ")"

toDataTree (Var n)      = Node (show n) []
toDataTree (Lam t)      = Node "Lam" [toDataTree t]
toDataTree (App t1 t2)  = Node "App" [toDataTree t1, toDataTree t2]

-- pretty prining
redStepsHelper :: (Term, Bool) -> [Term] -> [Term]
redStepsHelper (t, False) ss    = ss
redStepsHelper (t, True) ss     = redStepsHelper p (ss ++ [t])
                                    where p = lmRed t

redSteps :: Term -> [Term]
redSteps t = redStepsHelper (t, True) []

fillList :: [a] -> Int -> a -> [a]
fillList xs n y = xs ++ replicate (n - length xs) y

maxLength :: (Ord a) => [[a]] -> Int
maxLength xs = fst $ maximum $ map (\x -> (length x, x)) xs

homoLines :: [String] -> [String]
homoLines ls = map (\x -> fillList x (maxLength ls) ' ') ls

homoRows :: [[String]] -> [[String]]
homoRows xs = map (\x -> fillList x (maxLength xs) []) xs

homogenise :: [[String]] -> [[String]]
homogenise xs = map homoLines $ homoRows xs

blockFormat :: [String] -> String
blockFormat bs = unlines $ map unwords $ transpose 
                    $ homogenise $ map (splitOn "\n") bs

-- tests
printReduction t = putStrLn $ blockFormat 
                        $ map (drawTree . toDataTree) $ redSteps t
printLReduction t = putStrLn $ unlines $ map show $ redSteps t

test0 = printReduction ex0
test1 = printReduction ex1
test2 = printReduction ex2
test3 = printReduction ex3



    
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

(#) :: Term -> Term -> Term
t1 # t2 = App t1 t2
   
    -- DB free vars of an DB terms
freeVars :: Term -> [Int]
freeVars (Var n)   = [n]
freeVars (Lam e)   = freeVars e \\ [foldr min 0 $ freeVars e]
freeVars (App f a) = freeVars f `union` freeVars a

    -- all db vars
allVars :: Term -> [Int]
allVars (Var n)   = [n]
allVars (Lam e)   = freeVars e
allVars (App f a) = freeVars f `union` freeVars a

-- Parsing lambda calculus
contains :: Term -> Int -> Bool
contains t n = n `elem` (allVars t)

level :: Term -> Int
level (App t1 t2) = max (level t1) (level t2)
level (Lam t) = 1 + level t
level (Var _) = 0

data SKTerm = SKVar Int 
            | SKApp SKTerm SKTerm 
            | S 
            | K 
            deriving (Eq)

instance Show SKTerm where
    show (SKVar n)
        | n < 0         = "(" ++ show n ++ ")"
        | otherwise     = show n
    show (SKApp t1 t2)  = "(" ++ show t1 ++ show t2 ++ ")"
    show S              = "S"
    show K              = "K"

-- Reduction of skTerms
skRed :: SKTerm -> (SKTerm, Bool)
skRed (SKApp (SKApp K t) _) = (fst $ skRed t, True)
skRed (SKApp S (SKApp t1 (SKApp t2 t3)))
    = (SKApp (SKApp (fst $ skRed t1) (fst $ skRed t3)) 
        (SKApp (fst $ skRed t2) (fst $ skRed t3)), True)
skRed t = (t, False)

  --Reduction strategy
skRedStrat :: SKTerm -> SKTerm
skRedStrat t
    | snd p     = skRedStrat $ fst p
    | otherwise = t
        where p = skRed t

-- A data type to help convert between lambda and SK terms
data CTerm =  CVar Int
            | CLam CTerm
            | CApp CTerm CTerm
            | CS
            | CK
            deriving (Eq, Show)

termToCTerm :: Term -> CTerm
termToCTerm (Var n)     = CVar n
termToCTerm (Lam t)     = CLam $ termToCTerm t
termToCTerm (App t1 t2) = CApp (termToCTerm t1) (termToCTerm t2)

cToSK :: CTerm -> SKTerm
cToSK (CVar n)        = SKVar n
cToSK (CApp t1 t2)    = SKApp (cToSK t1) (cToSK t2)
cToSK CS              = S
cToSK CK              = K



  --removing lambdas
    
    -- C free vars of an DB terms
freeCVars :: CTerm -> [Int]
freeCVars (CVar n)   = [n]
freeCVars t@(CLam e)   = freeCVars e \\ [clevel t]
freeCVars (CApp f a) = freeCVars f `union` freeCVars a
freeCVars _          = []

    -- all C vars
allCVars :: CTerm -> [Int]
allCVars (CVar n)   = [n]
allCVars (CLam e)   = freeCVars e
allCVars (CApp f a) = freeCVars f `union` freeCVars a
allCVars _          = []

ctermCont :: CTerm -> Int -> Bool
ctermCont t n = n `elem` (allCVars t)

ctermFCont :: CTerm -> Int -> Bool
ctermFCont t n = n `elem` (freeCVars t)

clevel :: CTerm -> Int
clevel (CApp t1 t2) = max (clevel t1) (clevel t2)
clevel (CLam t) = 1 + clevel t
clevel _        = 0
  
cshift :: Int -> Int -> CTerm -> CTerm
cshift d c (CVar i)
    | i  < c            = CVar i
    | i >= c            = CVar (i + d)
cshift d c (CLam t)     = CLam $ cshift d (c + 1) t
cshift d c (CApp t1 t2) = CApp (cshift d c t1) (cshift d c t2)
cshift d c t            = t
  
skise :: CTerm -> CTerm
skise t@(CVar _)                     = t
skise t@(CApp t1 t2)                 = CApp (skise t1) (skise t2)
skise t'@(CLam t)                    
    | not (ctermFCont t $ clevel t') = CApp CK $ skise $ cshift (-1) 1 t
skise t@(CLam (CVar 1))              = CApp (CApp CS CK) CK
skise t'@(CLam (CLam t))             
    | (ctermFCont t $ clevel t')     =  skise $ CLam $ skise $ CLam t
skise t@(CLam (CApp p q))            = CApp 
                                        (CApp CS (skise $ CLam p)) 
                                        (skise $ CLam q)
skise CS                             = CS
skise CK                             = CK

skiT :: Term -> SKTerm
skiT t = cToSK $ skise $ termToCTerm t

skiL :: SKTerm -> Term
skiL (SKApp t1 t2)  = App (skiL t1) (skiL t2)
skiL (SKVar n)      = Var n
skiL S              = Lam $ Lam $ Lam $ 
                            App (App (Var 3) (Var 1))
                                  (App (Var 2) (Var 1))
skiL K  = Lam $ Lam $ Var 2

--DB Beta reduction

    --shifts
shift :: Int -> Int -> Term -> Term
shift d c (Var i)
    | i  < c          = Var i
    | i >= c          = Var (i + d)
shift d c (Lam t)     = Lam $ shift d (c + 1) t
shift d c (App t1 t2) = App (shift d c t1) (shift d c t2)

sub :: Term -> Int -> Term -> Term
sub t j (Var i)
    | i == j        = t
    | otherwise     = (Var i)
sub t j (Lam t')    = Lam $ sub (shift 1 1 t) (j + 1) t'
sub t j (App t1 t2) = App (sub t j t1) (sub t j t2)

betaRed :: Term -> Term
betaRed (App (Lam t1) t2) = shift (-1) 1 $ sub (shift 1 1 t2) 1 t1
betaRed t = t


-- Reductions and reduction strategies

-- left-most reduction
lmRed :: Term -> (Term, Bool)
lmRed t@(App (Lam t1) t2)   = (betaRed t, True)
lmRed t@(App t1 t2)
    | snd p                 = (App (fst p) t2, True)
    | snd q                 = (App t1 (fst q), True)
    | otherwise             = (t, False)
                                where   p = lmRed t1
                                        q = lmRed t2
lmRed (Lam t)               = (\f (x, y) -> (f x, y)) Lam $ lmRed t
lmRed t = (t, False)

-- left-most reduction strategy
lmRedStrat :: Term -> Term
lmRedStrat t
    | snd p         = lmRedStrat $ fst p
    | otherwise     = t
        where p = lmRed t

-- head reduction
hdRed :: Term -> (Term, Bool)
hdRed t@(App (Lam t1) t2)   = (betaRed t, True)
hdRed (App t1 t2)           = (App (fst p) t2, snd p)
    where p = hdRed t1
hdRed (Lam t)               = (\f (x, y) -> (f x, y)) Lam $ lmRed t
hdRed t                     = (t, False)
