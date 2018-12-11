import Data.List
import Data.Char


data Circle = Circle Float Float Float  deriving Show
--		       1     2     3  
-- Point = (Float1, Float2) Radius = Float3 

--1
-- overlap :: Circle -> Circle -> Bool

overlap (Circle x1 y1 r1) (Circle x2 y2 r2) = sqrt((x1-x2)^2 + (y1-y2)^2) <= r1+r2
--overlap ((Circle x1 y1 r1), (Circle x2 y2 r2)) = sqrt((x1-x2)^2 + (y1-y2)^2) <= r1+r2

--2
-- perms :: [a] -> [[a]]
--Hugs> perms "abc"
--["abc","bac","bca","acb","cab","cba"]
uniq x [] = x 
uniq [] (a:xs) = uniq [a] xs
uniq x (a:xs) = if a `elem` x then uniq x xs else uniq (a:x) xs -- grabbed uniq from stack exchange



perms [] =[[]]
perms x = uniq (concat (map iterate [0..((length x)-1)]))
        where iterate n = scatter (head (drop n x)) (take n x ++ drop (n+1) x) 

scatter x list = map insertAt [0..length list]
        where insertAt n = take n list ++ [x] ++ drop n list  

--3
combos x y = [(a,b) | a <- x,  b <- y]

--4
--overlaps :: [Circle] -> Int
--overlaps x = dropWhile (False) (pairs x)
--	where pairs x = [overlap a b | a <- x, b<-x]

--5
trans x = transpose x -- this is just from the Data.List prelude


-- code from https://codereview.stackexchange.com/questions/48451/implementing-transpose
--transpose' :: [[a]] -> [[a]]
--transpose' [[]]    = []
--transpose' [[], _] = []
--transpose' rows    = (map head' rows) : transpose' (map tail' rows)
--  where
--    head' (x:_) = x
--    tail' (_:xs) = xs



--6 Ceasar Cypher
let2int c = ord c - ord 'a'

int2let n = chr (ord 'a' + n)

shift n c = if isLower c then (int2let (mod (let2int c + n)  26)) else c

encode n xs = [shift n x | x <-xs]
