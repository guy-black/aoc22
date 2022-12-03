module Day1 where

import Data.List

-- splits a list into a list of lists.
-- Splits the list at every element for which the  second argument makes True
-- Leaves the offending element out of any of the new lists
-- Maybe if i want to get fancy I could set an extra arg to determin whether to drop the elements that satisfy
-- the function, or to tag them at the end of the list breaking off, or leave it at the front of the next list.
-- I really thought there was a builtin function for this
-- i've read the hackage page for Data.List like over and over again not finding it
-- eventually realized i could just hand roll it for the time I'm spending looking for it
-- NOTE: technically the reverse there in the base case isn't necessary, but I just don't like the groups of
-- calorie counnts coming out backwards
splitOn :: [[a]] -> [a] -> (a -> Bool) -> [[a]]
splitOn acc [] _ = reverse acc
splitOn acc (x:xs) chck =
  if chck x then
    splitOn acc xs chck
  else
    let (fst,rst) = break chck (x:xs) in
      splitOn (fst:acc) rst chck

puzz1 :: IO()
puzz1 = do
  input <- readFile "input1"
  let splitInput = splitOn [] (lines input) null
  let prettyInput = zip [1..] splitInput
  let summedPrettyInput = map (\(i,x)-> (i,sum (map (\y-> (read y)::Int) x))) prettyInput
  putStrLn $ show $ maximum $ map snd summedPrettyInput


puzz2 :: IO()
puzz2 = do
  input <- readFile "input1"
  let splitInput = splitOn [] (lines input) null
  let prettyInput = zip [1..] splitInput
  let summedPrettyInput = map (\(i,x)-> (i,sum (map (\y-> (read y)::Int) x))) prettyInput
  putStrLn $ show$ sum $ take 3 $ reverse $ sort $ map snd summedPrettyInput
