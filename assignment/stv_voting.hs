import STVUtils
import Debug.Trace
import Data.List
import Data.Ord (comparing)

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

stv :: Int -> FilePath -> IO ()
stv n ballotsFile = do ballots <- ballotsList ballotsFile
                       let quota' = quota ballots n
                       print ("Quota : " ++ show quota')
                       let winners' = winners n 1 quota' ballots []
                       print $ sortBy counts winners'
                    where
                        counts = comparing snd