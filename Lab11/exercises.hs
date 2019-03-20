import System.IO

-- Exercise 1 --
palindrome :: String -> Bool
palindrome word = word == rword
                  where
                    rword = reverse word

palindromeTest :: IO ()
palindromeTest = do putStr "Enter a word : "
                    word <- getLine
                    if palindrome word == True then
                        putStrLn "That word is a Palindrome"
                    else 
                        putStrLn "That word is not a Palindrome"

-- Exercise 2 --
getInt :: IO Integer
getInt = do line <- getLine
            return (read line)

userSum :: IO ()
userSum = do putStr "Give first number: "
             num1 <- getInt
             putStr "Give second number: "
             num2 <- getInt
             putStrLn "Calculating ... "
             putStrLn ("Sum is " ++ show (num1 + num2))

-- Exercise 3 --
putNtimes :: Integer -> String -> IO ()
putNtimes 0 _ = return ()
putNtimes n str = do putStrLn str
                     putNtimes (n-1) str

-- Exercise 4 --
getNInts :: Integer -> IO Integer
getNInts 0 = return 0
getNInts n = do putStr ("Enter Integer "++show(n)++": ")
                num <- getInt
                nextNum <- getNInts (n-1)
                return (num + nextNum)

sumNInts :: IO ()
sumNInts = do putStr "Give integer n: "
              n <- getInt
              putStrLn ("Now give " ++ show(n) ++ " Integers")
              sum <- getNInts n
              putStrLn ("Sum Of "++show(n)++" Integers is "++show(sum))
