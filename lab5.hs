import Data.Char
import Data.List

data BTree a = Tip | BNode a (BTree a) (BTree a)  deriving Show

data Rose a = Branch a [Rose a]  deriving Show

data Dir = L | R  deriving Show

leaf x = BNode x Tip Tip

tfold f u Tip = u
tfold f u (BNode x l r) = f x (g l)(g r)
        where g = tfold f u 

size = tfold(\x l r -> 1 + l + r) 0
height = tfold (\x l r -> 1+l `max` r) 0
inord = tfold (\x l r -> l ++ x : r) [] 

samplebin = BNode 'A' (BNode 'B' (leaf 'C')
                                 (leaf 'D'))
                      (BNode 'E' (BNode 'F' (leaf 'G') Tip)
                                 (BNode 'H' (BNode 'I' Tip (leaf 'J'))
                                            (leaf 'K')))

samplebst = BNode 37 (BNode 25 (leaf 12) (BNode 28 Tip (leaf 32)))
                     (BNode 51 (BNode 42 (leaf 39) (BNode 47 (leaf 43) (leaf 49)))
                               (BNode 69 Tip (BNode 81 (BNode 72 (leaf 71) Tip) Tip)))

sampler = Branch "Amarantha"
            [ Branch "Bethesda" [Branch "Edna" []],
              Branch "Callipygia"
                [ Branch "Flegma" [Branch "Indicia"[],
                                   Branch "Jaundice" [],
                                   Branch "Kalahari" []
                                  ],
                  Branch "Ganache" []],
              Branch "Depilitoria" [Branch "Hildegarde" [Branch "Ludmilla" [],
                                                         Branch "Miasma" []]]
            ]

--1 
--Write the postord function we left out in lecture; it should return the postorder traversal of a BTree, where the left and right sub-trees are traversed first, and the node value is "visited" last.

postord = tfold (\x l r -> l ++ r ++ [x]) []




--2
-- Write two functions (discussed in lecture!) to find the minimum and maximum values from a binary search tree; call them mint and maxt. These functions should use the Maybe type in the result in order to allow for empty trees (which don't have a minimum or maximum value).

minT Tip = Nothing
minT bst = Just (minimum (inord bst)) 
maxT Tip = Nothing 
maxT bst = Just (maximum (inord bst))


--3
--Write a function called path which will return a list of "directions" to take to get to a particular value in a binary search tree. For the directions you may use the following data type:
--Your function should accept a value that might be an item in a binary search tree, plus a tree itself, and return a path as a list of Dir values. You may use either an error message or the Maybe type to handle missing values.
--path :: Ord a => a -> BTree a -> [Dir]
--OR   (just one!)
--path :: Ord a => a -> BTree a -> Maybe [Dir]
--If you try the Maybe-fied version, you may want to use the following utility function to "push" a function "inside" a Maybe value (technically, this is really just the fmap function from the Maybe instance of the Functor type class, but you needn't worry too much about that):

--push :: (a -> b) -> Maybe a -> Maybe b
--push f  Nothing = Nothing
--push f (Just x) = Just (f x)

path bst x = if elem x (inord bst) then direct x bst else []
	where direct a Tip = []
	      direct a (BNode x l r) = if a == x then [] else if a > x then R:(direct a r) else L:(direct a l)

--4
-- Write each of the following functions for general trees (a.k.a. rose trees):
--rsize :: Rose a -> Integer, to count the number of node values in a rose tree;
--rheight :: Rose a -> Integer, to measure the height, or longest path from the root to a child (count the height of a leaf as one);
--rmap :: (a -> b) -> Rose a -> Rose b, to map a function across the node values in a rose tree
--rzip :: Rose a -> Rose b -> Rose (a,b), to zip together two rose trees into a tree of pairs; just as with the usual zip function on lists, you should trim the results to the minimum available structure between the two input trees.
--rmirror :: Rose a -> Rose a, to return the "symmetric opposite" of a rose tree, as if it were mirrored down the middle.

rsize (Branch x []) = 1
rsize (Branch x xs) = 1 + sum (map rsize xs) 

rheight (Branch x []) = 1
rheight (Branch x xs) = 1 + maximum (map rheight xs) 

rmap func (Branch x xs) = Branch (func x) (map (rmap func) xs)

--rzip [] _ = []
--rzip _ [] = []
--rzip (Branch x xs) (Branch y ys) = Branch (x,y) (map rzip xs ys)

rmirror (Branch x xs) = Branch x (map rmirror (reverse xs))
