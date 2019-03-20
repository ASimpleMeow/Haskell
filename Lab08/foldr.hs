-- Exercise 1 --
sumsq :: Integral a => a -> a
sumsq n = foldr (\x y -> x*x+y) 0 [1..n]

-- Exercise 2 --
lengthr :: [a] -> Int
lengthr xs = foldr (\_ y -> y+1) 0 xs

-- Exercise 3 --
minlist :: [Int] -> Int
minlist = foldr1 op
          where
            op x y | x < y = x
                   | otherwise = y

-- Exercise 4 --
myreverse :: [a] -> [a]
myreverse = foldr (\x xs -> xs ++ [x]) []

-- Exercise 5 --
remove :: Eq a => [a] -> [a] -> [a]
remove s = foldr (\x y -> (op x s) ++ y) []
           where
             op :: Eq a => a -> [a] -> [a]
             op x xs| x `elem` xs = []
                    | otherwise = [x]

-- Exercise 6 --
remdups :: Eq a => [a] -> [a]
remdups = foldr op []
        where
          op :: Eq a => a -> [a] -> [a]
          op x ys | null ys = [x]
                  | head ys == x = ys
                  | otherwise = [x] ++ ys
