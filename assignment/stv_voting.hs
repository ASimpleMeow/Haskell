-- Student Name: Oleksandr Kononov
-- Student Number: 20071032
-- Student Course: Entertainment Systems

-- The main single transferable vote file to load.
-- Call the function "stv" for voting

import STVUtils
import Debug.Trace
import Data.List
import Data.Ord (comparing)

-- Ranks votes based on their weight (frequency of occurance)
-- Note: nub is a nice little function that finds only unique elements of a list
rank :: Ord a => [a] -> [(a,Int)]
rank vs = sortBy moreVotes [(v, count v vs) | v <- nub vs]
          where
            moreVotes (_, vc1) (_, vc2) = compare vc2 vc1

-- Find and returns all the votes that belong to a given candidate
votesOf :: Eq a => [[a]] -> a -> [[a]]
votesOf votes a = [vote | vote <- votes, head vote == a]

-- When candidate is elected, redo the votes and weights of votes based on surplus from quota
redoVotes :: (Ord a, Show a) => [[a]] -> (a,Int) -> Int -> [[a]]
redoVotes votes winner q | surplus > 0 = take surplus winnerlessVotes ++ restOfVotes
                         | otherwise = elim (fst winner) votes
                          where
                           winnerVotes = votesOf votes $ fst winner
                           restOfVotes = [v | v <- votes, v `notElem` winnerVotes]
                           winnerlessVotes = elim (fst winner) winnerVotes
                           surplus = snd winner - q

-- Parameters:
-- Number of canidates left to elect, count number, quota, votes, already elected candidates
winners :: (Ord a, Show a) => Int -> Int -> Int -> [[a]] -> [(a, Int)] -> [(a, Int)]
winners 0 _ _ _ elected = reverse elected
winners n c q votes elected | length rankedVotes == 1 = reverse newElected
                            | snd (head rankedVotes) >= q = winners (n-1) q (c+1) newVotes newElected
                            | length rankedVotes <= n = reverse elected ++ [(v, c) | v <- map fst rankedVotes]
                            | otherwise = winners n c q elimVotes elected
                             where
                              rankedVotes = rank $ head $ transpose $ filter (not.null) votes
                              newVotes = filter (not.null) $ redoVotes votes (head rankedVotes) q
                              elimVotes = elim (fst $ last rankedVotes) votes
                              newElected = (fst (head rankedVotes), c) : elected

-- Call this function to start calculating the votes
stv :: Int -> FilePath -> IO ()
stv n ballotsFile = do ballots <- getBallots ballotsFile
                       let quota' = getQuota ballots n
                       print ("Quota : " ++ show quota')
                       let winners' = winners n 1 quota' ballots []
                       print $ sortBy counts winners'
                    where
                        counts = comparing snd