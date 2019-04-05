import Debug.Trace
import Data.List

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

rank :: Ord a => [a] -> [(a,Int)]
rank vs = sortBy moreVotes [(v, count v vs) | v <- nub vs]
          where
            moreVotes (_, vc1) (_, vc2) = compare vc2 vc1

winners :: (Ord a, Show a) => Int -> Int -> [[a]] -> [a] -> [a]
winners 0 _ _ elected = reverse elected
winners n quota votes elected | snd (head rankedVotes) >= quota = winners (n-1) quota newVotes newElected
                              | otherwise = winners n quota elimVotes elected
                              where
                                rankedVotes = rank $ head $ transpose votes
                                newVotes = elim (fst $ head rankedVotes) votes
                                elimVotes = elim (fst $ last rankedVotes) votes
                                newElected = fst (head rankedVotes) : elected

stv :: Int -> FilePath -> IO ()
stv n ballotsFile = do ballots <- ballotsList ballotsFile
                       --let weightedBallots = [(v, 1000) | v <- ballots]
                       print ("Quota : " ++ show (quota ballots n))
                       print (winners n (quota ballots n) ballots [])