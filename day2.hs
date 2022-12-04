module Day2 where


data Them = A
          | B
          | C
          deriving (Read, Show, Eq)

data Me = X
        | Y
        | Z
        deriving (Read, Show, Eq)

-- aight clever solution is out, brute force is in

-- a/x rock | b/y paper | c/z scissors

charpairtoint :: String -> Int
charpairtoint str =
  if      str == "A X" then (1 + 3)
  else if str == "A Y" then (2 + 6)
  else if str == "A Z" then (3 + 0)
  else if str == "B X" then (1 + 0)
  else if str == "B Y" then (2 + 3)
  else if str == "B Z" then (3 + 6)
  else if str == "C X" then (1 + 6)
  else if str == "C Y" then (2 + 0)
  else if str == "C Z" then (3 + 3)
  else -42069 -- this shouldn't happen


puzz1 :: IO()
puzz1 = do
  rawinput <- readFile "input2"
  let parsedInput = map charpairtoint (lines rawinput)
  putStrLn $ show $ sum $ parsedInput

-- a rock 1 | b paper 2 | c scissors 3
-- x lose 0 | y draw  3 | z win      6

newcharpairtoint :: String -> Int
newcharpairtoint str =
  if      str == "A X" then (3 + 0)
  else if str == "A Y" then (1 + 3)
  else if str == "A Z" then (2 + 6)
  else if str == "B X" then (1 + 0)
  else if str == "B Y" then (2 + 3)
  else if str == "B Z" then (3 + 6)
  else if str == "C X" then (2 + 0)
  else if str == "C Y" then (3 + 3)
  else if str == "C Z" then (1 + 6)
  else -42069 -- this shouldn't happen


puzz2 :: IO()
puzz2 = do
  rawinput <- readFile "input2"
  let parsedInput = map newcharpairtoint (lines rawinput)
  putStrLn $ show $ sum $ parsedInput

