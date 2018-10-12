--1
-- same3 :: Eq a => a -> a -> a -> Bool
same3 x y z = if x==y && y==z then True else False

--2
-- poly :: Num a => a -> a
poly x  = 5*x^2+3*x-2

--3
-- nor :: Bool -> Bool -> Bool
nor x y = if x/=True && y/=True then True else False

--4
-- echo :: Int -> [a] -> [a]
echo x y  = concat (replicate x y)

--5
-- reverb :: Int -> [a] -> [a]
reverb x y = concat (map (replicate x) y)

--6
-- twice :: (t -> t) -> t -> t
-- input looks like twice (+1) 0
twice f x = f (f x)

{-The following function twice applies its function argument twice to a value:
twice f x = f (f x)
Show how twice can be defined without reference to a second argument, i.e., without the x or any other variable in its place. Specifically, write into a file and be able to demonstrate an alternate version of the function twice (if you want to define both functions in the same file for the sake of comparison, you can call one of them twice', with a trailing single quote.)
-}

-- twice' :: (b -> b) -> b -> b
twice' f = f.f

--7
{- 
Consider the following functions defined using twice; for each one, describe what it does in plain English. Also give an example application and result for each; i.e., show an example of it being used to compute something concrete.


nice = twice twice
-- applies function four times to a variable
slice = twice (twice twice)
-- applies function sixteen times to a variable
dice = (twice twice) twice
-- applies function sixteen times to a variable
-}

--8
{-
Give alternate, simpler definitions for each of the following (they should be equivalent to the ones given here):
foo = twice (take 10)
-- only returns the first 10 items in a list 
bar = twice (drop 10)
-- removes the first 20 items in a list and returns the rest 
-}

foo' = take 10
bar' = drop 20

