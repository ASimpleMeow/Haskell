module STVUtils where

ballotsList :: String -> IO [[String]]
ballotsList file = do ballots <- readFile file
                      readIO ballots

quota :: [[a]] -> Int -> Int
quota vss n = (length vss `div` (n + 1)) + 1

weight :: Int -> Int -> Int -> Int
weight oldweight surplus total = oldweight * (surplus `div` total)

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map $ filter (/=x)

firsts :: [[a]] -> [a]
firsts = map head