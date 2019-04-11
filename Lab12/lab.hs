fmap' :: (a -> b) -> IO a -> IO b
fmap' f m = do x <- m
               return (f x)

repeat' :: IO Bool -> IO () -> IO()
repeat' test m = do res <- test
                    if res then return ()
                    else do m
                            repeat' test m
