{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches xs ys = length $ filter pairMatch $ zip xs ys

pairMatch :: (Peg, Peg) -> Bool
pairMatch (a, b) = a == b
-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors xs = foldr (accumulateColor xs) [] colors

accumulateColor :: Code -> Peg -> [Int] -> [Int]
accumulateColor c p a = (countColor p c):a

countColor :: Peg -> Code -> Int
countColor p xs = length $ filter (== p) xs

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches xs ys = sum $ findMins (countColors xs) (countColors ys)

findMins :: [Int] -> [Int] -> [Int]
findMins [] _ = [0]
findMins _ [] = [0]
findMins (x:xs) (y:ys) = (min x y):findMins xs ys

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove s g = Move g exactMatchCount (matchCount - exactMatchCount)
  where
    exactMatchCount = exactMatches s g
    matchCount = matches s g

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent m@(Move g _ _) c = m == getMove c g

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes m cs = filter isConsistentWithMove cs
  where
    isConsistentWithMove = isConsistent m

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = []
allCodes 1 = map (\x -> [x]) colors
allCodes n = genCodes $ allCodes (n - 1)

genCodes :: [Code] -> [Code]
genCodes codes = [x ++ [y] | x <- codes, y <- colors]

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve secret = tryCodes secret $ allCodes 8

tryCodes :: Code -> [Code] -> [Move]
tryCodes _ [] = []
tryCodes secret (x:[]) = [getMove secret x]
tryCodes secret (x:xs) = xMove:(tryCodes secret $ filterCodes xMove xs)
  where
    xMove = getMove secret x

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
