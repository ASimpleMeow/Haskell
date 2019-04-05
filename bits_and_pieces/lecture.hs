import Data.List
-- Lecture Exercise 1
bigCubes :: (Num a, Ord a) => [a] -> [a]
bigCubes xs = filter (\x -> x > 500) $ map (^3) xs

-- Lecture Exercise 2
lottaBiggest :: (Num a, Ord a) => [a] -> [a]
lottaBiggest xs = replicate 4 $ maximum xs

-- Lecture Exercise 3
powers :: (Num a) => a -> [a]
powers x = zipWith ($) [(^2),(^3),(^4)] $ replicate 3 x

-- Lecture Exercise 4
pcts = [0.15,0.2,0.21]
amts = [20.5,30,25]

calcBill :: (Fractional a, Num a) => [a] -> [a] -> [a]
calcBill p a = map (\x -> x * 1.04) $ zipWith (+) a $ zipWith (*) p a
