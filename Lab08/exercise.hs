import Data.Char

------------------------------
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myOr2 :: [Bool] -> Bool
myOr2 xs = foldr (||) False xs
------------------------------



-- EXERCISES --
--------------------------------------------------
-- Excersie 1 --
mapFilter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapFilter m f xs = map m (filter f xs)


-- Exercise 2 --
myAll :: (a -> Bool) -> [a] -> Bool
myAll f [b] = f b
myAll f (b:bs) = f b && myAll f bs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f [x] = f x
myAny f (x:xs) = f x || myAny f xs

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile f (x:xs) | f x = x : myTakeWhile f xs
                     | otherwise = []

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile f (x:xs) | f x = myDropWhile f xs
                     | otherwise = (x:xs)

-- Exercise 3 --
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x xs -> f x:xs) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr (\x xs -> if p x then x:xs else xs) []


-- Exercise 4 --
capitalises :: String -> String
capitalises = map toUpper


-- Exercise 5 --
squareall :: [Int] -> [Int]
squareall = map (^2)

-- Exercise 6 --
nestedreverse :: [String] -> [String]
nestedreverse = reverse.map reverse 

-- Exercise 7 --
atfront :: a -> [[a]] -> [[a]]
atfront x = map ([x]++) 

-- Exercise 8 --
lengths :: [String] -> [Int]
lengths [] = []
lengths (x:xs) = length x : lengths xs

-- Exercise 9 --
sumsq :: Int -> Int
sumsq n = sum $ map (^2) [1..n]

-- Exercise 10 --
myOtherFilter :: (a->Bool) -> [a] -> [a]
myOtherFilter p = concat.map box
                where
                  box x = if p x then [x] else []

-- Exercise 11 --
wvowel :: [Char] -> [Char]
wvowel = filter $ not.(\x -> toLower (x) `elem` ['a','o','u','i','e'])

-- Exercise 12 --
wiv :: [String] -> [String]
wiv = map wvowel
