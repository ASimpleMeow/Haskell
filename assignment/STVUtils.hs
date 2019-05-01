-- Student Name: Oleksandr Kononov
-- Student Number: 20071032
-- Student Course: Entertainment Systems

-- Utility functions for the single transferable voting class

module STVUtils where

-- Given a file of votes (list of lists) read it and return the list of lists structure
getBallots :: String -> IO [[String]]
getBallots file = do ballots <- readFile file
                     readIO ballots

-- Given the votes and n number of candidates to elect, calculate the necessary quota
getQuota :: [[a]] -> Int -> Int
getQuota vss n = (length vss `div` (n + 1)) + 1

-- Given a list, count the number of occurance of the given element
count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

-- Eliminate a given element from a given list of lists
elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map $ filter (/=x)

-- Get the first elements from a list of lists
firsts :: [[a]] -> [a]
firsts = map head