--Import this for Exercise 6--
import Data.Char


-- Exercise 1 --
cubes :: [(Int, Int)]
cubes = [(x,x^2) | x <- [1..10]]


-- Exercise 2 --
myConstFunc :: [(Int,Int)]
myConstFunc = [(x,1) | x <- [1..5]]


-- myZip function --
myZip :: [a] -> [b] -> [(a,b)]
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys
myZip _ _ = []


-- Exercise 3 --
--f1 = [(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]--
f1 :: [(Int, Int)]
f1 = [(x,y) | x <- [1..3], y <- [4..5]]


--f2 = [(4,1),(5,1),(4,2),(5,2),(4,3),(5,3)]
f2 :: [(Int, Int)]
f2 = [(x,y) | x <- [4..5], y <- [1..3]]

--f3 = [(4,1),(5,1),(4,2),(5,2),(4,3),(4,3)]
f3 :: [(Int,Int)]
f3 = [(y,x) | x <- [1..3], y <- [4..5]]


-- Exercise 4 --
isEven :: Integer -> Bool
isEven n = (n `mod` 2 == 0)

--[2*n | n <- [2,4,7], isEven n, n > 3]--
--[8]--
isEvenList = [2*n | n <- [2,4,7], isEven n, n > 3]


-- Exercise 5 --
doubleAll :: [Integer] -> [Integer]
doubleAll xs = [x*2 | x <- xs]


-- Exercise 6 --
capitalize :: String -> String
capitalize s = [toUpper c | c <- s]


-- Exercise 7 --
sumOf1_100 :: Int
sumOf1_100 = sum [i^2 | i <- [1..100]]


-- Exercise 8 --
sigma' :: Int -> Int
sigma' n = sum [i^2 | i <- [1..n]]


-- Exercise 9 --
matches :: Integer -> [Integer] -> [Integer]
matches x xs = [x' | x' <- xs, x' == x]


elem' :: Integer -> [Integer] -> Bool
elem' x xs = length (matches x xs) > 0


-- Exercise 10 --
grid :: Int -> Int -> [(Int, Int)]
grid x y = [(x', y') | x' <- [0..x], y' <- [0..y]]


-- Exercise 11 --
square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- (grid n n), not (x == y)]


-- Exercise 12 --
myReplicate :: Int -> a -> [a]
myReplicate n x = [x | i <- [1..n]]


-- Exercise 13 --
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]



-- Exercise 14 --
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], (sum (factors x) - x) == x]
