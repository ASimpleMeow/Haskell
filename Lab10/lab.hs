module ComparingSortingGrouping where
import Data.List(sort,sortBy,group,groupBy)

data Colour = Blue | Green | Orange | Purple | Red | Yellow
    deriving (Eq, Ord, Show)

people = 
    [("Tim",24,Red,"Oregon"),
    ("Tom",36,Blue,"Ohio"),
    ("Mary",19,Yellow,"Vermont"),
    ("Zach",41,Blue,"California"),
    ("Ann",9,Purple,"Michigan"),
    ("Jane",50,Red,"Oregon"),
    ("Harry",71,Green,"Utah"),
    ("Jim",80,Blue,"Vermon"),
    ("Robert",23,Red,"California"),
    ("Lois",32,Green,"Michigan"),
    ("Barbara",50,Red,"Oregon"),
    ("Caleb",15,Yellow,"Utah"),
    ("Vicki",24,Red,"Oregon"),
    ("David",50,Green,"Oregon"),
    ("Justin",50,Purple,"Oregon"),
    ("Andrew",29,Red,"Oregon")
    ]

name (nm,ag,clr,st) = nm
age (nm,ag,clr,st) = ag
color(nm,ag,clr,st) = clr
state(nm,ag,clr,st) = st

------------------------------------
-- the names of all people who live in Oregon

p1 = sort [name p | p <- people, state p == "Oregon"]
-------------------------------------------
-- How many people live in California

p2 = length  [name p | p <- people, state p == "California"] 


-------------------------------------------------
-- the list of ages of people who live in Utah

p3 = [age p | p <- people, state p == "Utah"]

----------------------------------------------
-- The names of all people in the survey
-- in alphabetical order

p4 = sort [name p | p <- people]

----------------------------------------------------
-- The names and color of all people, sorted by color

p5:: [(String,Colour)]
p5 = sortBy c [(name p, color p) | p <- people]
    where c (n1,c1) (n2,c2) = compare c1 c2
--------------------------------------------------------
-- The color and the count of all those with that color

p6:: [(Colour,Int)]
p6 =  [(c, colorCount c) | c <- colors]
    where
        colors = [Blue, Green, Orange, Purple, Red, Yellow]
        colorCount c = length [color p | p <- people, color p == c]

-----------------------------------------------------
-- The color and a list of all names with that color

p7:: [(Colour,[String])]
p7 = [(c, namesOfColor c) | c <- colors]
    where
        colors = [Blue, Green, Orange, Purple, Red, Yellow]
        namesOfColor c = [name p | p <- people, color p == c]


----------------------------------------------
-- The names and ages of all those who live in Oregon
-- grouped by age.

p8:: [[(String,Integer)]]
p8 = groupBy a [(name p, age p) | p <- people, state p == "Oregon"]
    where a (n1, a1) (n2, a2) = a1 == a2
