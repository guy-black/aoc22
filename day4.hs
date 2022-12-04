module Day4 where

import Data.List

parseLine :: String -> (Int,Int,Int,Int)
parseLine str = (read ("(" ++ (map (\x->if x == '-' then ',' else x) str) ++ ")"))::(Int,Int,Int,Int)

compPair :: (Int, Int, Int, Int) -> (Ordering, Ordering)
compPair (a1, a2, b1, b2) = (compare a1 b1,compare a2 b2)

chckCover :: (Ordering, Ordering) -> Bool
chckCover (fst, snd) = (fst == EQ)||(snd == EQ)||(fst /= snd)


puzz1 :: IO()
puzz1 = do
  rawInput <- readFile "input4"
  let parsedInput = map parseLine (lines rawInput)
  let compedInput = map compPair parsedInput
  putStrLn $ show $ length $ filter chckCover compedInput

pairRanges :: (Int, Int, Int, Int) -> ([Int],[Int])
pairRanges (a,b,c,d) = ([a..b],[c..d])

chckOver :: ([Int],[Int]) -> Bool
chckOver (a,b) =
  case (intersect a b) of
    [] -> False
    _ -> True

puzz2 :: IO()
puzz2 = do
  rawInput <- readFile "input4"
  let parsedInput = map parseLine (lines rawInput)
  let rangePairs = map pairRanges parsedInput
  putStrLn $ show $ length $ filter chckOver rangePairs

