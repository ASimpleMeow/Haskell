import System.IO

act :: IO (Char, Char)
act = do x <- getChar
         getChar
         y <- getChar
         return (x,y)

put4times :: String -> IO ()
put4times str = do putStr str
                   putStr str
                   putStr str
                   putStr str

read2lines :: IO ()
read2lines = do getLine
                getLine
                putStrLn "two lines read"

getNput :: IO ()
getNput = do line <- getLine
             putStrLn line

reverse2lines :: IO ()
reverse2lines = do line1 <- getLine
                   line2 <- getLine
                   putStrLn(reverse line1)
                   putStrLn(reverse line2)

strlen :: IO ()
strlen = do putStr "Enter a string: "
            xs <- getLine
            putStr "The string has "
            putStr (show (length xs))
            putStr " characters\n"

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                 do putChar x
                    return []
              else
                 do putChar '-'
                    xs <- sgetLine
                    return (x:xs)

match :: String -> String -> String
match xs ys = [if elem x ys then x else '-' | x <- xs]

play :: String -> IO ()
play word = do putStr "? "
               guess <- getLine
               if guess == word then
                  putStrLn "You got it!"
               else
                  do putStrLn (match word guess)
                     play word

hangman :: IO ()
hangman = do putStrLn "Think of a word: "
             word <- sgetLine
             putStrLn "Try to guess it: "
             play word
