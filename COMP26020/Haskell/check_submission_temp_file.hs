data QuadTree = NewQuadTree Cell Cell Cell Cell deriving (Eq, Show) -- A QuadTree is made up of 4 component cells

data Cell = Cell Char | CellQuadTree QuadTree deriving (Eq, Show) -- A cell can either be an individual cell or another QuadTree

data BinaryTree a = Leaf a | Node (BinaryTree a) (BinaryTree a) deriving (Eq, Show) -- A binary tree used to store values of cells as we recurse

allBlack :: Int -> Cell -- Sets all values to Black
allBlack a = Cell 'b'

allWhite :: Int -> Cell -- Sets all values to White
allWhite a = Cell 'w'

clockwise :: Cell -> Cell -> Cell -> Cell -> Cell -- Defines the clockwise order of cells in a QuadTree
clockwise a b c d = CellQuadTree (NewQuadTree a b c d)

anticlockwise :: Cell -> Cell -> Cell -> Cell -> Cell -- Defines the anticlockwise order of cells in a QuadTree
anticlockwise a b c d = CellQuadTree (NewQuadTree a d c b)

quadTreeFinder :: QuadTree -> [Cell] -- Used to convert the values of cells in a QuadTree into a list so they can be manipulated later
quadTreeFinder(NewQuadTree a b c d) = [a, b, c, d]

treeLeftHalf :: BinaryTree Int -> BinaryTree Int -- Used to seperate out the left hand side of a binary tree into a new binary tree
treeLeftHalf (Node a b) = a -- The left hand side of the tree is returned as a new binary tree
treeLeftHalf (Leaf x) = Leaf x -- If the input is a leaf this is the only item in the binary tree and can't be split into two so is just returned

treeRightHalf :: BinaryTree Int -> BinaryTree Int -- Used to seperate out the right hand side of a binary tree into a new binary tree
treeRightHalf (Node a b) = b -- The right hand side of the tree is returned as a new binary tree
treeRightHalf (Leaf x) = Leaf x -- If the input is a leaf this is the only item in the binary tree and can't be split into two so is just returned

convertTo :: Char -> Int -- Used to convert cell Char representation of black and white into an usable numeric representation
convertTo (x)
    | x == 'b' = -1 -- Black cells are represented as -1
    | x == 'w' = 1 -- White cells are represented as 1
    | otherwise  = 0

convertBack :: Int -> Char -- Used to convert a number back into its Char representation of a cell
convertBack (y)
    | y == (-1) = 'b' -- Cells with the value -1 are converted back to being represented as black
    | y == (1) = 'w' -- Cells with the value 1 are converted back to being represented as white
    | otherwise  = '0'

flipColour :: Cell -> Char -- Used to flip the colour of a cell when it needs to be blurred
flipColour (Cell z)
    | z == 'b' = 'w' -- Black cells are flipped to white cells
    | z == 'w' = 'b' -- White cells are flipped to black cells
    | otherwise = '0'

fillBinaryTree :: Cell -> String -> BinaryTree Int -- Function to fill a binary tree with the values of cells
fillBinaryTree (Cell c) (s) -- Triggered when a leaf is reached
   | c == 'b' = Leaf (-1) -- If the leaf is a black leaf it is given the value of -1
   | c == 'w' = Leaf 1 -- If the leaf is a white leaf it is given the value of 1
   | otherwise = Leaf 0
fillBinaryTree (CellQuadTree x) (s) 
   | s == "top" = Node (fillBinaryTree (quadTreeFinder(x) !! 1) s) (fillBinaryTree (quadTreeFinder(x) !! 0) s) -- The top of a QuadTree is the cells represented as a and b, this function recursively goes through all the cells above them and adds them to a binary tree
   | s == "left" = Node (fillBinaryTree (quadTreeFinder(x) !! 0) s) (fillBinaryTree (quadTreeFinder(x) !! 3) s) -- The left of a QuadTree is the cells represented as a and d, this function recursively goes through all the cells to the left and adds them to a binary tree    
   | s == "right" = Node (fillBinaryTree (quadTreeFinder(x) !! 2) s) (fillBinaryTree (quadTreeFinder(x) !! 1) s) -- The right of a QuadTree is the cells represented as b and c, this function recursively goes through all the cells to the right and adds them to a binary tree
   | s == "bottom" = Node (fillBinaryTree (quadTreeFinder(x) !! 3) s) (fillBinaryTree (quadTreeFinder(x) !! 2) s) -- The bottom of a QuadTree is the cells represented as c and d, this function recursively goes through all the cells below them and adds them to a binary tree

computeSum :: BinaryTree Int -> Int -- Used to sum up all the values in a binary tree
computeSum (Leaf x) = x -- If it's the only cell left in a binary tree return its value
computeSum (Node a b) = (computeSum a) + (computeSum b) -- Recursively sums all the values in a binary tree
    
bEvaluator :: Cell -> BinaryTree Int -> BinaryTree Int -> BinaryTree Int -> BinaryTree Int-> Cell -- Used to determine whether a cell is black or white by looking at the cells above, to the left, to the right and below the cell
bEvaluator (Cell x) (top) (right) (bottom) (left)
    | (((computeSum top) + (computeSum left) + (computeSum right) + (computeSum bottom)) > 0) = Cell 'w' -- If the sum of the values of the surrounding cells is positive then the cell will blur to white as this means there are more white cells surrounding it
    | (((computeSum top) + (computeSum left) + (computeSum right) + (computeSum bottom)) < 0) = Cell 'b' -- If the sum of the values of the surrounding cells is negative then the cell will blur to black as this means there are more black cells surrounding it
    | otherwise = Cell x -- If the sum of the values of the surrounding cells is equal to zero then no change should be made to the colour of the cell

actualBlur :: Cell -> BinaryTree Int -> BinaryTree Int -> BinaryTree Int -> BinaryTree Int -> Cell -- This function is where the blurring take splace recursively, it takes in either a cell or QuadTree as well as binary tree edge arguments and returns a cell or QuadTree
actualBlur(Cell x) (top) (right) (bottom) (left) = bEvaluator (Cell x) top left right bottom
actualBlur (CellQuadTree y) (top) (right) (bottom) (left)   =
    CellQuadTree (NewQuadTree (actualBlur (quadTreeFinder(y) !! 0) (treeLeftHalf top) (fillBinaryTree (quadTreeFinder(y) !! 1) "left") (fillBinaryTree (quadTreeFinder(y) !! 3) "top") (treeRightHalf left)) -- This function is recursively called on each of the quadrants and the relevant edge binary trees
                            (actualBlur (quadTreeFinder(y) !! 1) (treeRightHalf top) (treeLeftHalf right) (fillBinaryTree (quadTreeFinder(y) !! 2) "top") (fillBinaryTree (quadTreeFinder(y) !! 0) "right")) -- The edge binary tree is halved with each recursive call and either the left or right side taken depending on which quadrant is being used
                            (actualBlur (quadTreeFinder(y) !! 2) (fillBinaryTree (quadTreeFinder(y) !! 1) "bottom") (treeRightHalf right) (treeLeftHalf bottom) (fillBinaryTree (quadTreeFinder(y) !! 3) "right")) -- Information about previous level recursion edges is passed through which makes the process of knowing edge values easier
                            (actualBlur (quadTreeFinder(y) !! 3) (fillBinaryTree (quadTreeFinder(y) !! 0) "bottom") (fillBinaryTree (quadTreeFinder(y) !! 2) "left") (treeRightHalf bottom) (treeLeftHalf left))) --

blur :: Cell -> Cell -- This is the function to blur a QuadTree
blur(Cell x) = Cell x -- This is the base case, where a QuadTree is just a single cell of one colour, the same cell is then returned
blur(CellQuadTree y) =
    CellQuadTree (NewQuadTree (actualBlur (quadTreeFinder(y) !! 0) (Leaf 0) (fillBinaryTree (quadTreeFinder(y) !! 1) "left") (fillBinaryTree (quadTreeFinder(y) !! 3) "top") (Leaf 0)) -- Each of the four quadrants are passed into the blur function, in this quardant the top and right edges are already 0 as it is the top left corner
                            (actualBlur (quadTreeFinder(y) !! 1) (Leaf 0) (Leaf 0) (fillBinaryTree (quadTreeFinder(y) !! 2) "top") (fillBinaryTree (quadTreeFinder(y) !! 0) "right")) -- The top and right edges are already 0 as this is the top right corner
                            (actualBlur (quadTreeFinder(y) !! 2) (fillBinaryTree (quadTreeFinder(y) !! 1) "bottom") (Leaf 0) (Leaf 0) (fillBinaryTree (quadTreeFinder(y) !! 3) "right")) -- The bottom and right edges are already 0 as this is the bottom right corner
                            (actualBlur (quadTreeFinder(y) !! 3) (fillBinaryTree (quadTreeFinder(y) !! 0) "bottom") (fillBinaryTree (quadTreeFinder(y) !! 2) "left") (Leaf 0) (Leaf 0))) -- The bottom and left edges are already 0 as this is the bottom left corner
           -- This file is used to test your submission
-- it checks whether your submission will work with the automated testing script
-- but it does not provide many examples.
-- More testing examples will be provided on Blackboard
-- It needs to be pasted after your submission, which is what the script check_submission.sh does
-- before compiling and running

-- testing exercise 1

t1a = clockwise (allBlack 1) (allBlack 1) (allWhite 1) (allWhite 1) == anticlockwise (allBlack 1) (allWhite 1) (allWhite 1) (allBlack 1)

t1b = clockwise (allBlack 2) (anticlockwise (allBlack 1) (allWhite 1) (allWhite 1) (allBlack 1)) (allWhite 2) (allWhite 2) == anticlockwise (allBlack 2) (allWhite 2) (allWhite 2) (clockwise (allBlack 1) (allBlack 1) (allWhite 1) (allWhite 1))

t1c = clockwise (allBlack 1) (allBlack 1) (allWhite 1) (allWhite 1) /= anticlockwise (allWhite 1) (allWhite 1) (allWhite 1) (allBlack 1)

mainS = putStrLn("Joe: Tests running...")
        >> putStrLn(if t1a then "Joe: Simple check for ex 1 passed!" else "ERROR: Simple check for ex1 FAILED!")
        >> putStrLn(if t1c then "Joe: Inequality check for ex 1 passed!" else "ERROR: Inequality check for ex1 FAILED!")
        >> putStrLn(if t1b then "Joe: Larger check for ex 1 passed!" else "ERROR: Larger check for ex1 FAILED!")

--

mainE = putStrLn("Joe: **************************************************************************************************")
        >> putStrLn("Joe: If any tests above FAILED your submission is not correct")
        >> putStrLn("Joe: If all tests above passed then your submission is in the correct format but...")
        >> putStrLn("Joe: ...it may not be correct: this script does NOT test thoroughly!")
--
-- This file is used to test your submission
-- it checks whether your submission will work with the automated testing script
-- but it does not provide many examples.
-- More testing examples will be provided on Blackboard
-- It needs to be pasted after your submission, which is what the script check_submission.sh does
-- before compiling and running

-- texting exercise 2

-- single cells

str1a_r = ((allWhite 1) == (blur (allWhite 1)))
str1b_r = ((allBlack 1) == (blur (allBlack 1)))

single_r = str1a_r && str1b_r

-- STRIPED

-- 4 cells

str2a_i = let
  c = clockwise
  w = allWhite
  b = allBlack in
   (c(w 1)(w 1)(b 1)(b 1))
str2a_o = let
  c = clockwise
  w = allWhite
  b = allBlack in
   (c(w 1)(w 1)(b 1)(b 1))
str2a_r = (str2a_o == (blur str2a_i))

-- 10 cells

str3_i = let
  c = clockwise
  w = allWhite
  b = allBlack in
   (c(w 2)(w 2)(c(b 1)(w 1)(w 1)(b 1))(c(b 1)(w 1)(w 1)(b 1)))
str3_o = let
  c = clockwise
  w = allWhite
  b = allBlack in
   (c(w 2)(w 2)(c(w 1)(w 1)(w 1)(w 1))(c(w 1)(w 1)(b 1)(b 1)))
str3_r = (str3_o == (blur str3_i))


-- 16 cells

str4_i = let
  c = clockwise
  w = allWhite
  b = allBlack in
   (c(c(b 1)(w 1)(w 1)(b 1))(c(b 1)(w 1)(w 1)(b 1))(c(b 1)(w 1)(w 1)(b 1))(c(b 1)(w 1)(w 1)(b 1)))
str4_o = let
  c = clockwise
  w = allWhite
  b = allBlack in
   (c(c(b 1)(b 1)(w 1)(b 1))(c(w 1)(w 1)(w 1)(b 1))(c(b 1)(w 1)(w 1)(w 1))(c(b 1)(w 1)(b 1)(b 1)))
str4_r = (str4_o == (blur str4_i))

--- large

str_mid_i = let
  c = clockwise
  w = allWhite
  b = allBlack in
   (c(c(w 16)(w 16)(c(c(c(b 2)(b 2)(b 2)(b 2))(c(b 2)(b 2)(b 2)(b 2))(w 4)(w 4))(c(c(b 2)(b 2)(b 2)(b 2))(c(b 2)(b 2)(b 2)(b 2))(w 4)(w 4))(c(w 4)(w 4)(c(b 2)(b 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))(c(b 2)(b 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1))))(c(w 4)(w 4)(c(b 2)(b 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))(c(b 2)(b 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))))(c(c(c(b 2)(b 2)(b 2)(b 2))(c(b 2)(b 2)(b 2)(b 2))(w 4)(w 4))(c(c(b 2)(b 2)(b 2)(b 2))(c(b 2)(b 2)(b 2)(b 2))(w 4)(w 4))(c(w 4)(w 4)(c(b 2)(b 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))(c(b 2)(b 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1))))(c(w 4)(w 4)(c(b 2)(b 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))(c(b 2)(b 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1))))))(c(w 16)(w 16)(c(c(c(b 2)(b 2)(b 2)(b 2))(c(b 2)(b 2)(b 2)(b 2))(w 4)(w 4))(c(c(b 2)(b 2)(b 2)(b 2))(c(b 2)(b 2)(b 2)(b 2))(w 4)(w 4))(c(w 4)(w 4)(c(b 2)(b 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))(c(b 2)(b 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1))))(c(w 4)(w 4)(c(b 2)(b 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))(c(b 2)(b 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))))(c(c(c(b 2)(b 2)(b 2)(b 2))(c(b 2)(b 2)(b 2)(b 2))(w 4)(w 4))(c(c(b 2)(b 2)(b 2)(b 2))(c(b 2)(b 2)(b 2)(b 2))(w 4)(w 4))(c(w 4)(w 4)(c(b 2)(b 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))(c(b 2)(b 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1))))(c(w 4)(w 4)(c(b 2)(b 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))(c(b 2)(b 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1))))))(w 32)(w 32))
str_mid_o = let
  c = clockwise
  w = allWhite
  b = allBlack in
   (c(c(b 16)(b 16)(c(c(c(b 2)(b 2)(b 2)(b 2))(c(b 2)(b 2)(b 2)(b 2))(w 4)(w 4))(c(c(b 2)(b 2)(b 2)(b 2))(c(b 2)(b 2)(b 2)(b 2))(w 4)(w 4))(c(w 4)(w 4)(c(w 2)(w 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))(c(w 2)(w 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1))))(c(w 4)(w 4)(c(w 2)(w 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))(c(w 2)(w 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))))(c(c(c(b 2)(b 2)(b 2)(b 2))(c(b 2)(b 2)(b 2)(b 2))(w 4)(w 4))(c(c(b 2)(b 2)(b 2)(b 2))(c(b 2)(b 2)(b 2)(b 2))(w 4)(w 4))(c(w 4)(w 4)(c(w 2)(w 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))(c(w 2)(w 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1))))(c(w 4)(w 4)(c(w 2)(w 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))(c(w 2)(w 2)(c(w 1)(w 1)(b 1)(b 1))(c(b 1)(w 1)(b 1)(w 1))))))(c(b 16)(b 16)(c(c(c(b 2)(b 2)(b 2)(b 2))(c(b 2)(b 2)(b 2)(b 2))(w 4)(w 4))(c(c(b 2)(b 2)(b 2)(b 2))(c(b 2)(b 2)(b 2)(b 2))(w 4)(w 4))(c(w 4)(w 4)(c(w 2)(w 2)(c(w 1)(b 1)(w 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))(c(w 2)(w 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1))))(c(w 4)(w 4)(c(w 2)(w 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))(c(w 2)(w 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))))(c(c(c(b 2)(b 2)(b 2)(b 2))(c(b 2)(b 2)(b 2)(b 2))(w 4)(w 4))(c(c(b 2)(b 2)(b 2)(b 2))(c(b 2)(b 2)(b 2)(b 2))(w 4)(w 4))(c(w 4)(w 4)(c(w 2)(w 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))(c(w 2)(w 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1))))(c(w 4)(w 4)(c(w 2)(w 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))(c(w 2)(w 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1))))))(b 32)(b 32))
str_mid_r = (str_mid_o == (blur str_mid_i))


--- NOT STRIPED

-- 4 cells

str2b_i = let
  c = clockwise
  w = allWhite
  b = allBlack in
   (c(w 1)(b 1)(w 1)(b 1))
str2b_o = let
  c = clockwise
  w = allWhite
  b = allBlack in
   (c(b 1)(w 1)(b 1)(w 1))
str2b_r = (str2b_o == (blur str2b_i))

-- a medium-sized example

mid_i = let
  c = clockwise
  w = allWhite
  b = allBlack in
   (c(c(c(b 2)(b 2)(c(b 1)(b 1)(w 1)(b 1))(b 2))(c(c(b 1)(b 1)(w 1)(b 1))(c(b 1)(b 1)(w 1)(w 1))(w 2)(w 2))(c(c(w 1)(b 1)(w 1)(b 1))(c(b 1)(w 1)(b 1)(w 1))(w 2)(w 2))(c(b 2)(w 2)(w 2)(c(b 1)(w 1)(w 1)(b 1))))(c(c(c(b 1)(b 1)(w 1)(w 1))(c(b 1)(b 1)(b 1)(w 1))(w 2)(w 2))(c(b 2)(b 2)(b 2)(c(b 1)(b 1)(b 1)(w 1)))(c(c(w 1)(w 1)(w 1)(b 1))(b 2)(c(w 1)(b 1)(b 1)(w 1))(w 2))(c(c(w 1)(w 1)(b 1)(w 1))(c(b 1)(b 1)(w 1)(w 1))(w 2)(w 2)))(c(c(w 2)(w 2)(c(w 1)(w 1)(b 1)(b 1))(w 2))(c(w 2)(c(w 1)(b 1)(b 1)(w 1))(c(w 1)(b 1)(b 1)(w 1))(w 2))(c(w 2)(c(w 1)(b 1)(b 1)(w 1))(b 2)(c(b 1)(w 1)(b 1)(b 1)))(c(w 2)(b 2)(b 2)(c(w 1)(b 1)(b 1)(b 1))))(c(c(c(b 1)(w 1)(w 1)(b 1))(w 2)(w 2)(c(b 1)(w 1)(w 1)(b 1)))(c(w 2)(w 2)(c(w 1)(w 1)(w 1)(b 1))(c(w 1)(w 1)(b 1)(w 1)))(c(c(w 1)(b 1)(b 1)(w 1))(c(b 1)(w 1)(w 1)(b 1))(b 2)(b 2))(c(c(b 1)(w 1)(b 1)(b 1))(w 2)(c(b 1)(w 1)(b 1)(b 1))(b 2))))
mid_o = let
  c = clockwise
  w = allWhite
  b = allBlack in
   (c(c(c(b 2)(b 2)(c(b 1)(b 1)(w 1)(b 1))(b 2))(c(c(b 1)(b 1)(w 1)(b 1))(c(b 1)(b 1)(w 1)(w 1))(w 2)(w 2))(c(c(w 1)(w 1)(w 1)(w 1))(c(w 1)(w 1)(w 1)(w 1))(w 2)(w 2))(c(b 2)(w 2)(w 2)(c(b 1)(w 1)(w 1)(b 1))))(c(c(c(b 1)(b 1)(w 1)(w 1))(c(b 1)(b 1)(b 1)(w 1))(b 2)(w 2))(c(b 2)(b 2)(b 2)(c(b 1)(b 1)(b 1)(w 1)))(c(c(w 1)(w 1)(w 1)(w 1))(w 2)(c(w 1)(b 1)(b 1)(w 1))(w 2))(c(c(w 1)(w 1)(w 1)(w 1))(c(w 1)(w 1)(w 1)(w 1))(w 2)(w 2)))(c(c(w 2)(w 2)(c(w 1)(w 1)(b 1)(b 1))(w 2))(c(w 2)(c(w 1)(b 1)(b 1)(w 1))(c(w 1)(b 1)(b 1)(w 1))(w 2))(c(w 2)(c(w 1)(b 1)(b 1)(w 1))(b 2)(c(b 1)(b 1)(b 1)(b 1)))(c(w 2)(b 2)(b 2)(c(b 1)(b 1)(b 1)(b 1))))(c(c(c(b 1)(w 1)(w 1)(b 1))(w 2)(w 2)(c(b 1)(w 1)(w 1)(b 1)))(c(w 2)(w 2)(c(w 1)(w 1)(w 1)(b 1))(c(w 1)(w 1)(b 1)(w 1)))(c(c(w 1)(b 1)(b 1)(w 1))(c(b 1)(w 1)(w 1)(b 1))(b 2)(b 2))(c(c(b 1)(w 1)(b 1)(b 1))(w 2)(c(b 1)(b 1)(b 1)(b 1))(b 2))))
mid_r = (mid_o == (blur mid_i))



main =     mainS
        >> putStrLn(if single_r  then "Joe: Single cell check for ex2 passed!" else "ERROR: Single cell check for ex2 FAILED!")
        >> putStrLn(if str2a_r then "Joe: 4 cell striped check for ex2 passed!" else "ERROR: 4 cell striped check for ex2 FAILED!")
        >> putStrLn(if str3_r then "Joe: 10 cell striped  check for ex2 passed!" else "ERROR: 10 cell striped check for ex2 FAILED!")
        >> putStrLn(if str4_r then "Joe: 16 cell striped check for ex2 passed!" else "ERROR: 16 cell striped check for ex2 FAILED!")
        >> putStrLn(if str_mid_r then "Joe: big striped check for ex2 passed!" else "ERROR: big striped check for ex2 FAILED!")
        >> putStrLn(if str2b_r then "Joe: 4 cell not striped check for ex2 passed!" else "ERROR: 4 cell not striped check for ex2 FAILED!")
        >> putStrLn(if mid_r then "Joe: medium size not striped check for ex2 passed!" else "ERROR: medium size not striped check for ex2 FAILED!")
        >> mainE
--
