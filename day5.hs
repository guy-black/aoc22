module Day5 where

import qualified Data.Map as M
import Data.Char

-- copy and pasted from day1 because i can't import it for some reason
splitOn :: [[a]] -> [a] -> (a -> Bool) -> [[a]]
splitOn acc [] _ = reverse acc
splitOn acc (x:xs) chck =
  if chck x then
    splitOn acc xs chck
  else
    let (fst,rst) = break chck (x:xs) in
      splitOn (fst:acc) rst chck

-- copypasted from day3 for same reason
chunk :: [[a]] -> Int -> [a] -> [[a]]
chunk acc ln [] = reverse acc
chunk acc ln lst =
  let (fst,rst) = splitAt ln lst in
    chunk (fst:acc) ln rst

-- it just occured to me that read can't read char okay
readChar :: Read a => Char -> a
readChar c = read (c:[])

safeStringChar :: String -> Char
safeStringChar "" = ' '
safeStringChar (s:_) = s

puzz1:: IO()
puzz1 = do
  rawInput <- readFile "input5"
  let (rawStacks:rawMoves:[]) = splitOn [] (lines rawInput) Prelude.null
  let filteredMoves = (map (filter isDigit) (rawMoves))
  let intMoves = map (map (\x->(readChar x)::Int)) filteredMoves
  let parsedMoves = genMoves [] intMoves
  let sortedStacks = tail $ reverse $ map (chunk [] 4) rawStacks
  let filteredStacks = map (map (filter isAlpha)) sortedStacks
  let charStacks = map (map safeStringChar) filteredStacks
  let indexedCharStacks = map (zip [1..]) charStacks
  let baseStack = M.fromList (zip [1..9] (repeat " "))
  let parsedStacks = genStacks baseStack indexedCharStacks
--  putStrLn "raw stacks"
--  putStrLn $ show $ rawStacks
--  putStrLn "raw moves"
--  putStrLn $ show $ rawMoves
--  putStrLn "parsed moves"
--  putStrLn $ show $ parsedMoves
--  putStrLn "parsed initial stacks"
--  putStrLn $ show $ parsedStacks
  putStrLn "hopefully properly manipulated stacks"
  putStrLn $ show $ (moveStacks parsedMoves parsedStacks)
-- so if i try to show it i get an empty list error
-- if I look up the length of the list at index 1 it shows 36
-- if I look up the head of the list at index 1 i get an empty list error
-- i think my stack moving algorithm is doing something wrong
-- i'm sleepy.
-- if i look up the length of any other index list it gives the same error
-- something about tail getting an empty list
-- made a safe tail, still doesnt work.  now head is complaining about blank lists
-- made a safe head too that just returns a space
-- i got a Stacks
-- it did not move right

-- ------------------
-- Special Datatypes
-- ------------------

-- Map StackNumber Stack
type Stacks = M.Map Int [Char]

-- Move amount motion
data Move = Move Int Motion deriving Show

-- Motion from too
data Motion = Motion Int Int deriving Show

-- -------------
-- Parsing input
-- -------------

-- stacks generater
genStacks :: Stacks -> [[(Int, Char)]] -> Stacks
genStacks st [] = st
genStacks st (ics:xs) = genStacks (genStack ics st) xs

genStack :: [(Int, Char)] -> Stacks -> Stacks
genStack [] st = st
genStack ((i,c):ics) st = genStack ics (M.adjust (c:) i st)

-- moves generator
-- take (map (filter isDigit) (lines rawMoves))
-- if list length 3 append it to the list if not skip cause error
genMoves :: [Move] -> [[Int]] -> [Move]
genMoves acc [] = reverse acc
genMoves acc (x:xs) =
  if length x == 3 then
    genMoves ((genMove x):acc) xs
  else
    genMoves acc xs

-- move generator.
genMove :: [Int] -> Move
genMove (a:b:c:[]) = Move a (Motion b c)

-- -------------------------------
-- Functions to do Moves on Stacks
-- -------------------------------

-- performs multiple Moves onto a given Stack and returns the new Stack after all Moves
moveStacks :: [Move] -> Stacks -> Stacks
moveStacks [] st = st
moveStacks (x:xs) st = moveStacks xs (doMove x st)

-- takes a single Move and a stack and returns a Stack with that move applied
doMove :: Move -> Stacks -> Stacks
doMove (Move amt mo) st =
  countMoves [1..amt] st mo

-- takes a list of whatever, a Stacks, and a single Motion
-- if the list is emptty it just returns the given stack
-- else it recurses with the tail of the given list and the adjusted stack
countMoves :: [a] -> Stacks -> Motion -> Stacks
countMoves [] st _ = st -- no move moves left to do
countMoves (_:xs) st (Motion frm to) =
  countMoves xs (M.adjust safetail frm (M.adjust ((safehead (st M.! frm)):) to st)) (Motion frm to)

safetail :: [a] -> [a]
safetail [] = []
safetail (_:xs) = xs

safehead :: [Char] -> Char
safehead [] = ' '
safehead (x:_) = x
