iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate f (f x)

primes :: [Int]
primes = map head (iterate' sieve [2..])

sieve (p:xs) = [x | x <- xs, x `mod` p /= 0]

myprimes = takeWhile (< 1000000) primes
