n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

mylast xs = xs !! (length xs - 1)

myinit xs = take (length xs - 1) xs