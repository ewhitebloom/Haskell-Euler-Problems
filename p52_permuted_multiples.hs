{-
Problem 52:

It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits, but in a different order.

Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.

SOLVED
-}

import Data.List

solveProb :: Int -> Int
solveProb start = head $ [x | x <- [start..] , all (\mult -> isPermutation mult x) (map (*x) [2..6])]

intToDigits :: Int -> [Int]
intToDigits num = map (\x -> read [x] :: Int) $ show num

isPermutation :: Int -> Int -> Bool
isPermutation one two = let nums = map (\x -> sort $ intToDigits x ) [one, two]
                        in head nums == last nums

main = do
    print $ solveProb 2
    print $ "Hi"