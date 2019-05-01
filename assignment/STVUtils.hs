module STVUtils where

getBallots :: String -> IO [[String]]
getBallots file = do ballots <- readFile file
                     readIO ballots

getQuota :: [[a]] -> Int -> Int
getQuota vss n = (length vss `div` (n + 1)) + 1

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map $ filter (/=x)

firsts :: [[a]] -> [a]
firsts = map head