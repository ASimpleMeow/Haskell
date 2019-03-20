-- Exercise 1 --
sumdown :: Int -> Int
sumdown 0 = 0
sumdown x = x + sumdown (x-1)


-- Exercise 2 --
exponention :: Int -> Int -> Int
exponention 0 _ = 0
exponention _ 0 = 1
exponention a 1 = a
exponention a b = a * exponention a (b-1)


-- Exercise 3 --
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x-1) + fibonacci (x-2)


-- Exercise 4 --
myInit :: [a] -> [a]
myInit [x] = []
myInit (x:xs) = x : myInit xs 


-- Exercise 5 --
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (xs:xss) = xs ++ myConcat xss

myReplicate :: Int -> a -> [a]
myReplicate 1 x = [x]
myReplicate n x = [x] ++ myReplicate (n-1) x

myNth :: [a] -> Int -> a
myNth (x:xs) 0 = x
myNth (x:xs) n = myNth xs (n-1)


myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem y [x] = y == x
myElem y (x:xs) = y == x || myElem y xs


-- Exercise 6 --
mySum :: [Int] -> Int
mySum [] = 0
mySum (x:xs) = 1 + mySum xs

myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake n (x:xs) = x : myTake (n-1) xs

myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs


-- Exercise 7 --
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] xs = xs
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

-- Exercise 8 --
halve :: [a] -> ([a],[a])
halve xs = (take len xs, drop len xs)
           where
             len = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort a) (msort b)
         where
           a = fst (h)
           b = snd (h)
           h = halve xs


