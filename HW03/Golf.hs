module Golf where

skips :: [a] -> [[a]]
skips xs = map (breakOn xs) [1..length xs]

breakOn :: [a] -> Int -> [a]
breakOn [] _ = []
breakOn xs n = (drop (n - 1) (take n xs)) ++ breakOn (drop n xs) n


{------------------}

localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:others) = isLocalMaxima (a, b, c) ++ localMaxima (b:c:others)
localMaxima (a:_) = []

isLocalMaxima :: (Integer, Integer, Integer) -> [Integer]
isLocalMaxima (a, b, c)
  | a < b && b > c = [b]
  | otherwise = []

{-----------------}

histogram :: [Int] -> String
histogram scores = unlines $ (rows (frequency scores)) ++ [footer]

frequency :: [Int] -> [Int]
frequency xs = [length $ filter (== score) xs | score <- [0..9]]

rows :: [Int] -> [String]
rows frequencies = reverse $
  [row rowNum frequencies | rowNum <- [0..maximum frequencies]]

row :: Int -> [Int] -> String
row rowNum frequencies = [starOrEmpty rowNum (frequencies !! score) | score <- [0..9]]

footer :: String
footer = "==========\n0123456789\n"

starOrEmpty :: Int -> Int -> Char
starOrEmpty rowNum score
  | rowNum < score = '*'
  | otherwise = ' '
