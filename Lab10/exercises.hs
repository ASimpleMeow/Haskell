-- Exercise 1 & 2 --
data Expr = Val Int | Add Expr Expr | Mult Expr Expr
data Op = EVAL_MULT Expr | EVAL_ADD Expr | ADD Int | MULT Int

type Cont = [Op]

eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVAL_ADD y: c)
eval (Mult x y) c = eval x (EVAL_MULT y:c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL_ADD y : c) n = eval y (ADD n:c)
exec (EVAL_MULT y : c) n = eval y (MULT n:c)
exec (ADD n : c) m = exec c (n + m)
exec (MULT n: c) m = exec c (n * m)

val :: Expr -> Int
val e = eval e []


ex1 = val (Add (Add (Val 2) (Val 3)) (Val 4))

-- Exercise 3 --
data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult (Succ m) n = add n (mult m n)


-- Exercise 4 --
data MyTree a = MyLeaf a | MyNode (MyTree a) a (MyTree a)
data MyOrdering = L | E | G
comp :: Ord a => a -> a -> MyOrdering
comp x y | x > y = G
         | x < y = L
         | otherwise = E

occurs :: Ord a => a -> MyTree a -> Bool--
occurs x (MyLeaf y) = x == y
occurs x (MyNode l y r) = case comp x y of 
                        E -> True
                        L -> occurs x l
                        G -> occurs x r

-- Exercise 5 --
data Tree a = Leaf a | Node (Tree a) (Tree a)

tree :: Tree Int
tree = Node (Node (Leaf 1) (Leaf 4)) (Node (Leaf 6) (Node (Leaf 1) (Leaf 2)))

balancedTree :: Tree Int
balancedTree = Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4))

leaves :: Tree a -> Int
leaves (Leaf _) = 1
leaves (Node a b) = leaves a + leaves b 

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node a b) = abs (leaves a - leaves b) <= 1


-- Exercise 6 --
halve :: [a] -> ([a], [a])
halve xs = (take len xs, drop len xs)
         where
           len = length xs `div` 2

balance :: [a] -> Tree a
balance [x] = Leaf x
balance xs = Node (balance x) (balance y)
             where
               x = fst h
               y = snd h
               h = halve xs
