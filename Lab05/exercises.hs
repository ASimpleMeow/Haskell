halve :: [a] -> ([a], [a])
halve xs = ((take len xs), (drop len xs))
           where
             len = (length xs) `div` 2

f :: [a] -> ([a], [a])
f (x:xs) = ([x], xs)


--second :: [a] -> a--
--second (x:xs) = head xs--

--third :: [a] -> a--
-- using head and tail --
{-third xs = if not (null xs) then head(tail(tail xs))
           else error "List is less than length of 3"-}

-- using !! --
{-third xs = if (length xs) >= 3 then xs !! 2
           else error "List is less than length of 3"-}

-- using pattern matching --
--third (_:(_:(x:_))) = x--

safetail :: [a] -> [a]
--safetail xs = if (null xs) then [] else tail xs--

{-safetail xs | null xs = []
            | otherwise = tail xs-}

safetail [] = []
safetail (_:xs) = xs

(||) :: Bool -> Bool -> Bool
{-True || _ = True
_    || True = True-}

{-a || b | a == True = True
       | b == True = True
       | otherwise = False-}

{-a || b = if a == True then True else
         if b == True then True else False-}

False || b = b
b || False = b


lucky :: Integral a => a -> String
lucky x | x == 7 = "Lucky you.. Proceed directly to buy a lottery ticket"
        | x == 13 = "You, sadly are quite unlucky. Do not, under any circumstances, invest money today"
        | otherwise = "Mmmmm... Can't really say..."


-- \x y z -> x*y*z --


first :: (a, b, c) -> a
first (x,_,_) = x
second :: (a, b, c) -> b
second ( _ , y, _ ) = y
third :: (a, b, c) -> c
third ( _ , _ , z) = z


luhnDouble :: Int -> Int
luhnDouble x | num > 9 = num - 9
             | otherwise = num
             where
               num = x*2


luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (luhnDouble a + b + luhnDouble c + d) `mod` 10 == 0


luhnGetCheck :: Int -> Int -> Int -> Int
luhnGetCheck x y z = 10 - ((luhnDouble x + y + luhnDouble z) `mod` 10)
