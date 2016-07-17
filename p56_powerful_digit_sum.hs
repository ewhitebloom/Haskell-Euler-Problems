{-
Problem 56

A googol (10100) is a massive number: one followed by one-hundred zeros; 100100 is almost unimaginably large: one followed by two-hundred zeros. Despite their size, the sum of the digits in each number is only 1.

Considering natural numbers of the form, ab, where a, b < 100, what is the maximum digital sum?

SOLVED
-}

import Data.List

numToDigits :: Integer -> [Int]
numToDigits num = map (\x -> read [x] :: Int) $ show (num)

main = do
    print $ maximum [sum $ numToDigits (a^b) | a <- [0..99], b <- [0..99]]