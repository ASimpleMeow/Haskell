import Debug.Trace

multby2 :: Num a=> [a] -> [a]
multby2 [] = []
multby2 (x:xs) = x*2 : multby2 xs

multby2' :: Num a=> [a] -> [a]
multby2' [] = trace "running on empty" []
multby2' (x:xs) = trace "recursing" x*2 : multby2' xs

--Note use of (Num a, Show a) here - we need to use show. 
multby2'' :: (Num a, Show a)=> [a] -> [a]
multby2'' [] = trace "running on empty" []
multby2'' (x:xs) = trace ("value of x = "  ++ show x)  x*2 : multby2'' xs

--Factorial
fact :: Integer -> Integer
fact 0 = 1
fact n =  n * fact (n-1)

-- Sorting
sort :: (Ord a,Show a) => [a] -> [a]
sort [x] = [x]
sort ( x : xs ) = insert x ( sort xs )

insert :: (Ord a, Show a) => a -> [a] -> [a]
insert x [ ] = [ x ]
insert x ( y : ys ) = if x <= y  then x : (y : ys) else y : (insert x (ys))


-- Trees
data Tree a = EmptyTree | Node (Tree a) a (Tree a) deriving (Show, Read, Eq)
occurs x (Node l y r) | x == y = True
                      | x < y = occurs x l
                      | x > y = occurs x r

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = Node EmptyTree x EmptyTree
treeInsert x (Node a y b) = if (x < y) then treeInsert x a else treeInsert x b

flatten :: Tree a -> [a]
flatten EmptyTree = []
flatten (Node a x b) = flatten a ++ [x] ++ flatten b