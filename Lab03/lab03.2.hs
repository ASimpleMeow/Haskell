----------------------------------
-- add a type declaration
-- to each of the named expressions
i1:: Integer  -- I have done the first one for you
i1 = 45

i2 :: String
i2 = "123"

i3 :: Bool
i3 = 45 <= i1

i4 :: Char
i4 = 'c'

i5 :: [String]
i5 = ["abc","ok"]

i6 :: String
i6 = head i5

i7 :: [Char]
i7 = tail "abc"  -- Recall a string is a shorthand for a list of Char

i8 :: (Bool, Double)
i8 = (True,4.5)

i9 :: [Integer]
i9 = [i1,34]

-------------------------------------------------
-- For each named expression replace "undefined"
-- with an expression with the same type as the declaration


j1:: (String,Integer)
j1 = ("Test", 12)

j2:: [Integer]
j2 = [1,2,3]

j3:: Char
j3 = 'c'


j4:: Double
j4 = 1.1


j5:: (Integer,String,Integer,Char)
j5 = (1,"test", 2, 'c')

j6:: ([Char],(Bool,String))
j6 = (['c'],(True, "test"))

j7:: [[Bool]]
j7 = [[True, False], [True]]

j8:: [(String,Bool)]
j8 = [("test1", True), ("test2", False)]
