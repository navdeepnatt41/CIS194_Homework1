module Homework1 where

-- Exercise 1 Code
toDigits_helper :: Integer -> [Integer] -> [Integer]
toDigits_helper num l
  | num <= 0 = l
  | otherwise = toDigits_helper (num `div` 10) ((mod num 10) : l)

toDigits :: Integer -> [Integer]
toDigits num = toDigits_helper num []

toDigitsRev :: Integer -> [Integer]
toDigitsRev num = reverse $ toDigits num

-- Exercise 2 Code
_doubleEveryOther :: [Integer] -> [Integer]
_doubleEveryOther [] = []
_doubleEveryOther (e:[]) = e:[]
_doubleEveryOther (x:x1:xs) = x:(x1 * 2):(_doubleEveryOther xs)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l = reverse $ _doubleEveryOther (reverse l)

-- Exercise 3 Code
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (sum $ toDigits x) + (sumDigits xs)

-- Exercise 4 Code
validate :: Integer -> Bool
validate num = (sumDigits $ doubleEveryOther $ toDigits num) `mod` 10 == 0

-- Exercise 5 Code
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 "a" "b" "c" = []
hanoi 1 "a" "b" "c" = [("a","c")]

