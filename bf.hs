module BF where

import System.IO (hFlush, stdout)
import Data.Char

data Tape a = Tape 
                [a] --Left of pivot
                 a  --Pivot
                [a] --Right of pivot

emptyTape :: Tape Int
emptyTape = Tape zeros 0 zeros
    where zeros = repeat 0
    
data BFCommand = BFCommand
                | MoveRight
                | MoveLeft
                | Increment
                | Decrement
                | Print
                | Read
                | LoopL
                | LoopR
                | Comment Char
                
type BFSource = [BFCommand]

parseBF :: String -> BFSource
parseBF = map charToBF
    where
        charToBF x = case x of
            '>' -> MoveRight
            '<' -> MoveLeft
            '+' -> Increment
            '-' -> Decrement 
            '.' -> Print
            ',' -> Read
            '[' -> LoopL
            ']' -> LoopR
            c   -> Comment c

bf :: String -> IO()
bf = runBF . parseBF

runBF :: BFSource -> IO()
runBF = run emptyTape . bfSource2Tape
    where bfSource2Tape (b:bs) = Tape [] b bs

run :: Tape Int -> Tape BFCommand -> IO()
run tape source@(Tape _ MoveRight _)         = advance (moveRight tape) source
run tape source@(Tape _ MoveLeft _ )         = advance (moveLeft tape) source
run (Tape l p r) source@(Tape _ Increment _) = advance (Tape l (p + 1) r) source
run (Tape l p r) source@(Tape _ Decrement _) = advance (Tape l (p - 1) r) source

run tape@(Tape _ p _) source@(Tape _ Print _) = do
    putChar (chr p)
    hFlush stdout
    advance tape source

run tape@(Tape l _ r) source@(Tape _ Read _) = do
    p <- getChar
    advance (Tape l (ord p) r) source

run tape@(Tape _ p _) source@(Tape _ LoopL _)
    | p == 0        = seekLoopR 0 tape source
    | otherwise     = advance tape source

run tape@(Tape _ p _) source@(Tape _ LoopR _)
    | p /= 0        = seekLoopL 0 tape source
    | otherwise     = advance tape source

run tape source@(Tape _ (Comment _) _) = advance tape source

seekLoopR :: Int -> Tape Int -> Tape BFCommand -> IO()
seekLoopR 1 tape source@(Tape _ LoopR _) = advance tape source
seekLoopR b tape source@(Tape _ LoopR _) = seekLoopR (b-1) tape (moveRight source)
seekLoopR b tape source@(Tape _ LoopL _) = seekLoopR (b+1) tape (moveRight source)
seekLoopR b tape source                  = seekLoopR  b    tape (moveRight source)

seekLoopL :: Int -> Tape Int -> Tape BFCommand -> IO()
seekLoopL 1 tape source@(Tape _ LoopL _) = advance tape source
seekLoopL b tape source@(Tape _ LoopL _) = seekLoopL (b-1) tape (moveLeft source)
seekLoopL b tape source@(Tape _ LoopR _) = seekLoopL (b+1) tape (moveLeft source)
seekLoopL b tape source                  = seekLoopL  b    tape (moveLeft source)

advance :: Tape Int -> Tape BFCommand -> IO()
advance tape (Tape _ _ []) = return ()
advance tape source = run tape (moveRight source)

moveRight :: Tape a -> Tape a
moveRight (Tape ls p (r : rs)) = Tape (p : ls) r rs

moveLeft :: Tape a -> Tape a
moveLeft (Tape (l : ls) p rs) = Tape ls l (p : rs)
