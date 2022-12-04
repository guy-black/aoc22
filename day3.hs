module Day3 where

import GHC.Base
import Data.List


puzz1 :: IO()
puzz1 = do
  rawinput <- readFile "input3"
  let parsedInput = lines rawinput
  let pairedInput = map (\str->splitAt ((length str)`divInt` 2) str) parsedInput
  let lettersList = map (\(a,b)-> head (intersect a b)) pairedInput
  putStrLn $ show $ sum $ map charPrioritize lettersList

-- I'm like 99% sure theres a function in Data.List that will take 2 [a] and return a new [a]
-- of all values that were in both.  if not I'll just have to hand roll it
-- this should only bring me a list of one letter each, and then merge all those letters into a single long string
-- map charPrioritize over it then sum
-- nvm its Data.List.intersect


-- okay i really thought there was a builtin function for this already too
chunk :: [[a]] -> Int -> [a] -> [[a]]
chunk acc ln [] = reverse acc
chunk acc ln lst =
  let (fst,rst) = splitAt ln lst in
    chunk (fst:acc) ln rst

puzz2 :: IO()
puzz2 = do
  rawinput <- readFile "input3"
  let parsedInput = lines rawinput
  let chunkedInput = chunk [] 3 parsedInput
  putStrLn $ show $ sum $ map (charPrioritize . head) $ (map (\(x:xs)->foldl intersect x xs) chunkedInput)

charPrioritize :: Char -> Int
charPrioritize 'a' = 1
charPrioritize 'b' = 2
charPrioritize 'c' = 3
charPrioritize 'd' = 4
charPrioritize 'e' = 5
charPrioritize 'f' = 6
charPrioritize 'g' = 7
charPrioritize 'h' = 8
charPrioritize 'i' = 9
charPrioritize 'j' = 10
charPrioritize 'k' = 11
charPrioritize 'l' = 12
charPrioritize 'm' = 13
charPrioritize 'n' = 14
charPrioritize 'o' = 15
charPrioritize 'p' = 16
charPrioritize 'q' = 17
charPrioritize 'r' = 18
charPrioritize 's' = 19
charPrioritize 't' = 20
charPrioritize 'u' = 21
charPrioritize 'v' = 22
charPrioritize 'w' = 23
charPrioritize 'x' = 24
charPrioritize 'y' = 25
charPrioritize 'z' = 26
charPrioritize 'A' = 27
charPrioritize 'B' = 28
charPrioritize 'C' = 29
charPrioritize 'D' = 30
charPrioritize 'E' = 31
charPrioritize 'F' = 32
charPrioritize 'G' = 33
charPrioritize 'H' = 34
charPrioritize 'I' = 35
charPrioritize 'J' = 36
charPrioritize 'K' = 37
charPrioritize 'L' = 38
charPrioritize 'M' = 39
charPrioritize 'N' = 40
charPrioritize 'O' = 41
charPrioritize 'P' = 42
charPrioritize 'Q' = 43
charPrioritize 'R' = 44
charPrioritize 'S' = 45
charPrioritize 'T' = 46
charPrioritize 'U' = 47
charPrioritize 'V' = 48
charPrioritize 'W' = 49
charPrioritize 'X' = 50
charPrioritize 'Y' = 51
charPrioritize 'Z' = 52
charPrioritize _ = -42069 -- shouldn't happen
