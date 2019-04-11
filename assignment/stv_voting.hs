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

votesOf :: Eq a => [[a]] -> a -> [[a]]
votesOf votes a = [vote | vote <- votes, head vote == a]

redoVotes :: (Ord a, Show a) => [[a]] -> (a,Int) -> Int -> [[a]]
redoVotes votes winner q | surplus > 0 = take surplus winnerlessVotes ++ restVotes
                         | otherwise = elim (fst winner) votes
                          where
                           winnerVotes = votesOf votes $ fst winner
                           restVotes = [v | v <- votes, v `notElem` winnerVotes]
                           winnerlessVotes = elim (fst winner) winnerVotes
                           surplus = snd winner - q

winners :: (Ord a, Show a) => Int -> Int -> [[a]] -> [a] -> [a]
winners 0 _ _ elected = reverse elected
winners n q votes elected | snd (head rankedVotes) >= q = winners (n-1) q newVotes newElected
                          | otherwise = winners n q elimVotes elected
                           where
                            rankedVotes = rank $ head $ transpose votes
                            newVotes = redoVotes votes (head rankedVotes) q
                            elimVotes = elim (fst $ last rankedVotes) votes
                            newElected = fst (head rankedVotes) : elected

stv :: Int -> FilePath -> IO ()
stv n ballotsFile = do ballots <- ballotsList ballotsFile
                       let quota' = quota ballots n
                       print ("Quota : " ++ show quota')
                       print (winners n quota' ballots [])