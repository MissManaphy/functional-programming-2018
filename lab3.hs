data Circle = Circle Float Float Float  deriving Show
--		       1     2     3  
-- Point = (Float1, Float2) Radius = Float3 

--1
-- overlap :: Circle -> Circle -> Bool

overlap (Circle x1 y1 r1) (Circle x2 y2 r2) = sqrt((x1-x2)^2 + (y1-y2)^2) <= r1+r2

--2
-- perms :: [a] -> [[a]]
--Hugs> perms "abc"
--["abc","bac","bca","acb","cab","cba"]

perms x = 

--3
combos x y = [(a,b) | a <- x,  b <- y]

--4
--overlaps :: [Circle] -> Int
overlaps x = map overlap (pairs x)
	where pairs x = [(a,b) | a <- x, b<-x, b!=a]

--5

--6
