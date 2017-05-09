{-# Language TemplateHaskell #-}

module Brainfuck where

import Control.Lens
import Prelude hiding (Left, Right)
import Text.Parsec

-- our tape, infinite in both directions
--  Tape prev cur next
-- prev is in reverse order, so the previous cell is the top item, the second to last is the second item, etc.
data Mem = Mem { _prev ::[Int] 
               , _cur :: Int
               , _next :: [Int] 
               }
makeLenses '' Mem
-- Show prints out the local area of the tape
instance Show Mem where
  show mem = 
    "..." ++ (show . reverse . take 20 $ mem^.prev)
    ++ " " ++ show (mem^.cur) ++ " "
    ++ (show . take 20 $ mem^.next) ++ "..."
            
-- Some helper functions for working with Tape
initialTape :: Mem
initialTape = Mem{ _prev = (repeat 0), _cur = 0, _next = (repeat 0)}

moveLeft m = over prev (m^.cur :) . set cur newCur . set next newNext $ m
  where (newCur : newNext) = m^.next

moveRight m = set prev newPrev . set cur newCur . over next (m^.cur :) $ m 
  where (newCur : newPrev) = m^.prev

decr = over cur (\x-> x - 1)
incr = over cur (+ 1)

-- Our Abstract Syntax Tree for brainfuck
data Code = Incr 
          | Decr
          | Left
          | Right
          | Read
          | Write
          | Loop [Code]
          deriving (Eq, Show)

charCode (c,i) = char c >> return i

parseBrainfuck = parseLoop <|> parseNonLoop

parseNonLoop = foldl1 (<|>) $
  map charCode [ ('+' , Incr)
               , ('-' , Decr)
               , ('<' , Left)
               , ('>' , Right)
               , (',' , Read)
               , ('.' , Write)
               ]

parseLoop = char '[' >> parseBrainfuck >>= \loop -> char ']' >> return loop


eval :: [Code] -> Mem -> IO Mem  -- work through the entire tape
eval (c:cs) m = step c m >>= eval cs
eval [] m     = return m

step :: Code -> Mem -> IO Mem  -- execute a single instruction
step Incr  m = return $ incr m
step Decr  m = return $ decr m
step Left  m = return $ moveLeft m
step Right m = return $ moveRight m
step Read  m = getChar >>= \c -> return (set cur (fromEnum c) m) 
step Write m = do putChar . toEnum $ m^.cur
                  return m
step (Loop code) m = do m' <- eval code m
                        if m'^.cur == 0 
                        then return m
                        else step (Loop code) m
