-- Lab test for Haskell. This is worth 10% of your overall score. This test is worth 100 Marks. 
-- This file is made up of 
-- 		SECTION A - 20 Mark
--	 	SECTION B - 20 Marks
--		SECTION C - 40 Marks
-- The other file (clas_test_fixing errors.hs) is worth 20 Marks.
-- Please put your name below (where indicated) and zip the files into a .zip file using the naming convention 
-- first letter of first name + last name (e.g. mmeagher.zip)


-- PLEASE FILL THIS IN
-- Student Name : 

--SECTION A - 20 Marks

-- For each named declaration below. Use the comment preceeding
-- it as a guide to creating a comprehension that computes the
-- same value as that displayed in the comment.

-- [8,9,10,11,12]
x1 = [8..12]

-- [10,20,30,40,50,60]
x2 = [x*10 | x <- [1..6]]

-- -- [(1,2),(2,3),(3,4),(4,5)]
x3 = [(x,x+1) | x <- [1..4]]

-- write the sign function that returnss
--  +1 if its argument is positive
--  -1 if its argument is negative
--   0 if its argument is zero
sign x | x > 0 = 1
       | x < 0 = -1
       | otherwise = 0


-- write the factorial function that multiplies
-- all the numbers less then n. Eg.
-- fact 1 --> 1
-- fact 2 --> 2
-- fact 3 --> 6
-- fact 4 --> 24
-- Hint: use the following scheme
-- 5 ---> [1,2,3,4,5] -> 120
-- The product function will be useful
-- product [1,4,6] ---> 1*4*6 --> 24

fact n = product [1..n]



--SECTION B - 20 Marks
-- For each named expression replace "undefined"
-- with an expression with the same type as the declaration


j1:: (String,Integer)
j1 = ("Test", 1)

j2:: [Integer]
j2 = [1,2,3]

j3:: Char
j3 = 'c'


j4:: Double
j4 = 1.0


j5:: (Integer,String,Integer,Char)
j5 = (1,"Test", 2, 'c')

j6:: ([Char],(Bool,String))
j6 = (['t','e','s','t'],(True, "Test"))

j7:: [[Bool]]
j7 = [[True, False], [True]]

j8:: [(String,Bool)]
j8 = [("Test1", True), ("Test2", False)]

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
-- the names of all people who live in Oregon

p1 = [name p | p <- people, county p == "Oregon"]
-------------------------------------------
-- How many people live in Waterford

p2 = length [p | p <- people, county p == "Waterford"]


-------------------------------------------------
-- the list of ages of people who live in Kilkenny

p3 = [age p | p <- people, county p == "Kilkenny"]
-------------------------------------------------
-- the average age of people who live in Kilkenny

p4 = sum p3 `div` length p3
