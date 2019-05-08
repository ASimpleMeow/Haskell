import Debug.Trace
import Data.List
import Data.Ord

readVotesFile :: String -> IO [[String]]
readVotesFile file = do votes <- readFile file
                        readIO votes

quota :: [[a]] -> Int -> Int
quota vss n = (length vss `div` (n + 1)) + 1

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map $ filter (/=x)

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

votesOf :: Eq a => [[a]] -> a -> [[a]]
votesOf votes a = [vote | vote <- votes, head vote == a]

getSecondPreferences :: Eq a => a -> [[a]] -> [a]
getSecondPreferences x votes = [head $ tail v | v <- votesOf votes x]

rank :: Ord a => [a] -> [(a,Int)]
rank votes = sortBy moreVotes [(v, count v votes) | v <- nub votes]
          where
            moreVotes (_, vc1) (_, vc2) = compare vc2 vc1

getWeightOf :: Eq a => a -> [(a, Int)] -> Int
getWeightOf _ [] = 0
getWeightOf candidate weights | null filteredList = 0
                            | otherwise = snd $ head filteredList
                            where
                                filteredList = filter ((==candidate).fst) weights

getHighestCandidate :: Eq a => [(a,Int)] -> [(a, Int)] -> (a, Int)
getHighestCandidate rankedVotes weights = minimumBy highest (addWeights rankedVotes weights)
                                          where
                                           highest (_,w1) (_,w2) = compare w2 w1

getLowestCandidate :: Eq a => [(a,Int)] -> [(a, Int)] -> (a, Int)
getLowestCandidate rankedVotes weights = minimumBy lowest (addWeights rankedVotes weights)
                                         where
                                           lowest (_,w1) (_,w2) = compare w1 w2
            
addWeights :: Eq a => [(a,Int)] -> [(a,Int)] -> [(a,Int)]
addWeights rankedVotes weights = [(fst v, snd v + getWeightOf (fst v) weights) | v <- rankedVotes]

weightVotes :: Eq a => (a, Int) -> [[a]] -> [(a,Int)] -> Int -> [(a,Int)]
weightVotes winner votes weights q = [(second, calcWeight second) | second <- secondPrefs]
                                     where
                                        calcWeight x = (surplus * countVotes x) `div` snd winner
                                        countVotes x = length $ filter (== x) secondPrefs
                                        secondPrefs = getSecondPreferences (fst winner) votes
                                        surplus = snd winner - q

-- Parameters: votes rankedCandidateVotes weightsOfCandidates quota seatsToFill count electedCandidates                                        
winners :: (Ord a, Show a) => [[a]] -> [(a, Int)] -> [(a,Int)] -> Int -> Int -> Int -> [(a,Int)] -> [(a,Int)]
winners _ _ _ _ 0 _ elected = reverse elected
winners votes rankedVotes weightings q n c elected | length rankedVotes == 1 = winners winnerVotes winnerRankedVotes weightings q (n-1) (c+1) ((fst top, c): elected)
                                                   | topWeight > q = winners winnerVotes winnerRankedVotes winnerWeightings q (n-1) (c+1) ((fst top, c): elected)
                                                   | topWeight == q = winners winnerVotes winnerRankedVotes weightings q (n-1) (c+1) ((fst top, c) : elected)
                                                   | length rankedVotes <= n = reverse elected ++ [(fst v, c) | v <- rankedVotes]
                                                   | otherwise = winners loserVotes loserRankedVotes weightings q n c elected
                                                   where
                                                       top = getHighestCandidate rankedVotes weightings
                                                       topWeight = snd top
                                                       bottom = getLowestCandidate rankedVotes weightings
                                                       bottomWeight = snd bottom
                                                       winnerWeightings = if not $ null weightings then addWeights weightings newWeightings else newWeightings
                                                       newWeightings = weightVotes top winnerVotes weightings q
                                                       winnerVotes = filter (not.null) $ elim (fst top) votes
                                                       winnerRankedVotes = rank $ head $ transpose winnerVotes
                                                       loserVotes = filter (not.null) $ elim (fst $ last rankedVotes) votes
                                                       loserRankedVotes = rank $ head $ transpose loserVotes

-- Entry point for the STV counting program                                                    
stv :: Int -> FilePath -> IO ()
stv seats votesFile = do votes <- readVotesFile votesFile
                         let quota' = quota votes seats
                         let rankedVotes = rank $ head $ transpose votes
                         print ("Quota : " ++ show quota')
                         let elected = winners votes rankedVotes [] quota' seats 1 []
                         print("Elected : " ++ show elected)
                       