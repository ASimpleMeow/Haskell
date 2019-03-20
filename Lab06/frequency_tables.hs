table :: [Float]
table = [8.1, 1.5, 2.8, 4.1, 12.7, 2.2, 2.0,
         6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7,
         7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8,
         1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

count :: Eq a => a -> [a] -> Int
count x xs = length [ x' | x' <- xs, x==x']

lowers :: [Char] -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
           where
             n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]
