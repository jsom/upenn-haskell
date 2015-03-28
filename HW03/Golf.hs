module Golf where

skips :: [a] -> [[a]]
skips xs = map (breakOn xs) [1..length xs]

breakOn :: [a] -> Int -> [a]
breakOn [] _ = []
breakOn xs n = (drop (n - 1) (take n xs)) ++ breakOn (drop n xs) n
