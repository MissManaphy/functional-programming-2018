module Lab2 where

import Data.Char

--Fritz's Functions
pal str = str == reverse str

--1
-- count :: (a -> Bool) -> [a] -> Int
count f x = length(filter f x) -- works in ghci terminal

--2
-- title :: String -> String
title x = unwords( map f (words x))
        where f (x:xs) = toUpper x : map toLower xs
--3
-- longest :: String -> [String]
longest x = words (unwords (map maxWord (words x)))
        where lengthList = map length (words x)
              maxNum = maximum lengthList
              maxWord x = if length x==maxNum then x else []
--4
-- subset :: (Foldable t, Eq a) => [a] -> t a -> Bool
subset x y = and (set x y)
	where set (x:xs) y = elem x y : set xs y
	      set [] y = [True]

--4b
--subset x y = set x y
--       where set (x:xs) y = elem x y && set xs y
--              set [] y = True

--5 
--think of what the power of the empty list is
--then think of what the power of (x:xs) is 
-- [ "",  "c",  "b",  "bc",  top line is bottom line w/out a
--			     1st 2 elems are the 2nd two elems w/out b
--			     1st elem is 2nd elem w/out c
--  "a", "ac", "ab", "abc"]
-- [ "",  "c",  
--   "b",  "bc",]
--[ "",
--  "c",]
-- empty list, 3rd elem, 2nd elem, 2nd elem+3rd elem  
-- power :: [a] -> [[a]]
power [] = [[]]
power (x:xs) = power xs ++ map (x:) (power xs)

