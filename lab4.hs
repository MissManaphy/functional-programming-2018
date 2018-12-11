import Data.Char
import Data.List

-- 1
-- write a function that compares two strings to find their longest common prefix

lcp (x:xs) (y:ys) = if x == y then x:(lcp xs ys) else []
lcp _ _ = []

--2 
-- write an extended version of the longest common prefix that will apply to a whole list of arguments

lcps x = foldr1 (lcp) x   


--3
-- define the longest common suffix function as well
lcs x y = reverse (lcp (reverse x) (reverse y)) 

--4 
--you should define the lcss function
lcss x = foldr1 (lcs) x 


--5 
-- Write a function that will return a list of all the even-numbered elements of a list (it's in the book)
evens [] = []
evens (_:[]) = []
evens (x:y:zs) = y:evens zs


--6
-- In lecture we saw the until function from the Prelude that applied a function to some initial "seed" value repeatedly, until it produced a result that met some predicate; write a while function of the same type, but which returns the first value that "breaks" the rule.
until' p f x = if p x then x else until' p f (f x) 

while p f x = if p x then while p f (f x) else x


-- 7
-- Write a function during :: (a -> Bool) -> (a -> a) -> a -> [a] that will return a list of all the values "generated" during a run of while.

during p f x = if p x then [x]++(during p f (f x)) else []






