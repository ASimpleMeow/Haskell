-- Lab test for Haskell. This is worth 5% of your overall score. This test is worth 100 Marks. 
-- This file is made up of 
-- 		SECTION A - 20 Mark
--	 	SECTION B - 20 Marks
--		SECTION C - 40 Marks
-- The other file (clas_test_fixing errors.hs) is worth 20 Marks.
-- Please put your name below (where indicated) and zip the files into a .zip file using the naming convention 
-- first letter of first name + last name (e.g. mmeagher.zip)


-- PLEASE FILL THIS IN
-- Student Name : Oleksandr Kononov

--SECTION A - 20 Marks

-- For each named declaration below. Use the comment preceeding
-- it as a guide to creating a comprehension that computes the
-- same value as that displayed in the comment.

-- [1, 3, 5, 7, 9, 11]
x1 = [x | x <- [1..11], x `mod` 2 == 1]

-- [300,400,500]
x2 = [x*100 | x <- [3..5]]

-- [(1,1),(2,4),(3,9),(4,16), (5,25)]
x3 = [(x,x^2) | x <- [1..5]]


--x4:: fill in type
x4 :: Char -> Char -> Char
x4 a b = b


-- write a function mysum that 
-- takes an integer (n) as a parameter and returns the sum 
-- of all the numbers or all the numbers from 1 to n
-- Hint: use the following scheme
-- e.g. mysum 4 = 10
-- The sum function will be useful
-- sum [1,4,6] ---> 1 + 4 + 6
mysum :: Integer -> Integer
mysum n = sum [1..n]



--SECTION B - 20 Marks
-- For each named expression replace "undefined"
-- with an expression with the same type as the declaration


j1:: (String,String)
j1 = ("Test1","Test2")

j2:: [Double]
j2 = [1.0,2.0,3.0]

j3:: Char
j3 = 'c'


j4:: Double
j4 = 1.0

--- For the next section, write in a type definition with the associated declaration
j5:: (Char, Bool, Integer, Double)
j5 = ('c',True,1,1.0)

j6:: (String, [Integer])
j6 = ("Test", [1,2,3])

j7:: [[Bool]]
j7 = [[True,False],[False,True]]

j8:: Num a => [(a, a, a)]
j8 = [(1,2,3), (4,5,6)]

-- SECTION C  - 40 Marks
-- For each named expression replace "undefined"
-- with an expression that computes as per give description. 
people = 
  [("Tim",24,"Waterford")
  ,("Tom",36,"Kilkenny")
  ,("Mary",19,"Waterford")
  ,("Zach",41,"Kilkenny")
  ,("Ann",9,"Waterford")
  ,("Jane",50,"Tipperary")
  ,("Harry",71,"Wexford")  
  ,("Jim",80,"Wexford")
  ,("Robert",23,"Tipperary")
  ,("Lois",32,"Waterford")
  ,("Barbara",50,"Tipperary")
  ,("Caleb",15,"Tipperary")
  ,("Vicki",24,"Kilkenny")
  ,("David",50,"Waterford")
  ,("Justin",50,"Kilkenny")
  ,("Andrew",29,"Kilkenny")
  ]
  
name   (nm,ag,cy) = nm  
age   (nm,ag,cy)  =  ag
county (nm,ag,cy)  = cy 

------------------------------------
-- the names of all people who live in Wexford

p1 = [name p | p <- people, county p == "Wexford"]
-------------------------------------------
-- How many people live in Waterford

p2 = length [p | p <- people, county p == "Waterford"]


-------------------------------------------------
-- the list of people (with their ages) of people who live in Kilkenny

p3 = [(name p, age p) | p <- people, county p == "Kilkenny"]
-------------------------------------------------
-- the average age of people who live in Tipperary

p4 = (sum tip) `div` (length tip)
   where
     tip = [age p | p <- people, county p == "Tipperary"]
