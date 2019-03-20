sub :: (Int, Int) -> Int
sub (x,y) = x-y

add' :: Int -> (Int -> Int)
add' x y = x+y

mult :: Int -> Int -> Int -> Int
mult x y z = x*y*z


bools :: [Bool]
bools = [True, False]

nums :: [[Int]]
nums = [[1,2], [3,4,5]]

add :: Int -> Int -> Int -> Int
add x y z = x+y+z

copy :: a -> (a,a)
copy x = (x,x)

apply :: (a -> b) -> a -> b
apply a b = a (b)

second :: [a] -> a
second xs = head (tail xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair :: a -> b -> (a,b)
pair x y = (x,y)

double :: Num a => a -> a
double x = x*2

pallindrome :: Eq a => [a] -> Bool
pallindrome xs = reverse xs == xs

twice :: (t -> t) -> t -> t
twice f x = f (f x)
