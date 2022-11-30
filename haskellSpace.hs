

import Data.Bits (Bits(xor))
import CodeWorld
-- Assignment 2 
-- This assignment needs to run in CodeWorld environment 

data Line a = Line [a] a [a] deriving (Show)

-- Data and running for testing
integers :: Line Integer
integers = Line [-1, -2.. -5] 0 [1, 2.. 5]

sampleLine :: Line Picture
sampleLine = Line [a,b,c,d,e,f,g] c [g,d,b,c,a,f]
  where
    a = colored red (solidCircle 0.5)
    b = colored green (solidCircle 0.5)
    c = colored blue (solidCircle 0.5)
    d = colored yellow (solidCircle 0.5)
    e = colored purple (solidCircle 0.5)
    f = colored brown (solidCircle 0.5)
    g = colored pink (solidCircle 0.5)
blinker = (Space (Line blinker' (Line [Alive, Dead] Alive [Alive, Dead]) blinker')) 
  where
    blinker' = replicate 2 (Line (replicate 2 Dead) Dead (replicate 2 Dead))
main :: IO()
main = do
    -- AC: Line [-1,-2,-3] 0 [1,2,3]
    print (cutLine 3 integers) 
    -- AC: Line [1, 4, 9, ..] 0 [1, 4, 9, ..]
    print( mapLine (^2) integers) 
    -- AC: Line [(-1,-1),(-2,-2),..] (0,0) [(1,1),(2,2),..]
    print ( zipLines integers integers )
    -- AC: Alive
    print (rule30 (Line [Dead, Alive, Alive] Alive [Alive, Alive, Alive]))
    -- AC: Dead
    print (rule30 (Line [Alive, Alive, Alive] Alive [Alive, Alive, Alive]))
    -- AC: Just (Line [1,0,1,1] 1 [1,1])
    print(shiftRight (Line [0,1,1] 1 [1,1,1]))
    --  AC: Just (Line [1,1] 0 [1,1,1,1]
    print(shiftLeft (Line [0,1,1] 1 [1,1,1]))
    -- Line [Line [1] 2 [3,4,5],Line [] 1 [2,3,4,5]] (Line [2,1] 3 [4,5]) [Line [3,2,1] 4 [5],Line [4,3,2,1] 5 []]
    print ( lineShifts (Line [2,1] 3 [4,5]))
    -- AC: Line [Dead,Dead,Alive,Alive] Alive [Dead,Dead,Dead,Alive]
    print(applyRule30 (Line ([Dead, Alive, Alive, Dead] ) Alive ([Alive, Alive, Alive, Dead] )))
    -- AC: check refrerence, works 
    print (productOfLines (cutLine 3 integers) (cutLine 2 integers))
    --drawingOf (renderLine sampleLine)
    --drawingOf( renderSpace( mapSpace cellToPicture (blinker) ) )
    --drawingOf( renderSpace (mapSpace cellToPicture (applyConwayRule blinker) )  )
    animateConway( blinker)
     
-- 1.1 Lines

-- Exercise 1.1

-- | Keep up to a given number of elements in each direction in a line.
cutLine :: Int -> Line a -> Line a
cutLine c (Line x y z) = Line (take c x)  y (take c z)

-- Exercise 1.2

-- | A utility function that applies f to x until reaching Nothing, and returns 
-- a list of the results.
generateUntilNothing :: (a -> Maybe a) -> a -> [a]
generateUntilNothing f x = 
    case (f x) of
        Nothing -> []
        (Just y) -> (y:(generateUntilNothing f y) )
        
-- | Generates a line with x in focus, then applies f to x until reaching Nothing
-- to produce a list of elements to the left of x, and then same with applying 
-- g to x to produce a list of elements to the right of x.
genLine :: (a -> Maybe a) -> a -> (a -> Maybe a) -> Line a
genLine f x g = Line (generateUntilNothing f x) x (generateUntilNothing g x) 

-- Exercise 1.3 

-- | Apply a function to all elements on a line.
mapLine :: (a -> b) -> Line a -> Line b
mapLine f (Line l x r ) = Line (map f l) (f x) (map f r)

-- Exercise 1.4 

-- | A utility function that merges two lists by applying a combining function 
-- that takes two elements on them first: f x y-> a list of [f x y]. 
mergeLists :: [a] -> [b] -> (a -> b -> c) -> [c]
mergeLists x [] f = []
mergeLists [] y f = []
mergeLists (x:xs) (y:ys) f = ( (f x y):(mergeLists xs ys f) )

-- | Zip together two lines.
zipLines :: Line a -> Line b -> Line (a, b)
zipLines (Line a b c)  (Line x y z) = Line (zip a x) (b,y) (zip c z)

-- | Zip together two lines with a given combining function.
zipLinesWith :: (a -> b -> c ) -> Line a -> Line b -> Line c
zipLinesWith f (Line a b c)  (Line x y z) = Line (mergeLists a x f) (f b y) (mergeLists c z f) 

-- 1.2 Rule 30

-- Exercise 1.5
data Cell = Alive | Dead deriving (Show, Eq)

-- | A utility function to convert Cell to Bool. 
parseCell :: Cell -> Bool
parseCell a = case a of 
    Alive -> True
    otherwise -> False
    
-- | A utility function to convert Bool to Cell.
parseToCell :: Bool -> Cell
parseToCell a 
  | (a) = Alive
  | otherwise = Dead
  
-- Apply rule30 to Line: [left_cell XOR (central_cell OR right_cell)] to get new Cell.
-- If left_cell or right_cell don't exist, they're considered Dead. 
rule30 :: Line Cell -> Cell
rule30  (Line a b c ) = do
    let left_cell = c1
    let right_cell = c2
    let central_cell = parseCell b
    let new_cell = xor left_cell (central_cell || right_cell)
    parseToCell new_cell
    where
        c1 | (length a == 0) = False
           | otherwise = parseCell (head a)
        c2 | (length c == 0) = False
           | otherwise = parseCell (head c)


-- Exercise 1.6

-- | Shift the focus of the line to the left if possible; if not, return Nothing.
shiftLeft :: Line a -> Maybe (Line a)
shiftLeft (Line [] b c ) = Nothing
shiftLeft (Line a b c) = Just (Line (drop 1 a) (head a) ([b] ++ c) )

-- | Shift the focus of the line to the right if possible; if not, return Nothing.
shiftRight :: Line a -> Maybe (Line a)
shiftRight (Line a b [] ) = Nothing
shiftRight (Line a b c) = Just (Line ([b] ++ a) (head c) (drop 1 c) ) 

-- Exercise 1.7
    
-- | A function that maps every cell in a line into a version of the original 
-- line where that cell is in focus. 
-- The new line of lines should have the original line in focus.
-- This function uses genLine to apply shiftLeft and shiftRight and generate all 
-- the possible line shifts.
lineShifts :: Line a -> Line (Line a)
lineShifts l = genLine shiftLeft l shiftRight

-- | A apply rule30 to each shifted version of the line to get the new state
-- for each cell. It utilizes lineShifts and rule30.
applyRule30 :: Line Cell -> Line Cell
applyRule30 line = mapLine rule30 (lineShifts line)

-- Exercise 1.8

-- | A utility function that converts a cell to a picture, where Alive cells 
-- are represented by a black square, and Dead ones by a white/empty square.
cellToPicture :: Cell -> Picture 
cellToPicture c = case c of 
    Alive -> solidRectangle 1 1
    Dead -> rectangle 1 1

-- | A utility function that converts a line of Cells to a line of Pictures.
-- It makes use of cellToPicture
cellsToPictures :: Line Cell -> Line Picture
cellsToPictures line = mapLine cellToPicture line

-- | A utility function that translates a list of pictures by 1 extra position 
-- each to place them next to each other.
translatePictures :: [Picture] -> Double -> [Picture]
translatePictures [] i = []
translatePictures (x:xs) i = ((translated i 0 x):translatePictures xs (i+1) ) 

--  | Render a line of 1x1 pictures.
renderLine :: Line Picture -> Picture
renderLine (Line a b c) = pictures (translatePictures ( reverse (a) ++ [b] ++ c ) 0 )

-- A utility function that joins a line of cells to make one picture of the cells
-- next to each other. Width is made to limit infinite lines 
renderRule30Helper :: Int -> Int -> Line Cell -> Picture
renderRule30Helper _ 0 _ = blank
renderRule30Helper width n currentLine = current <> next 
  where 
    current = renderLine(cellsToPictures (cutLine(n) currentLine) ) 
    next = translated 0 (-1)(renderRule30Helper width (n-1) (applyRule30 currentLine) )
    
 -- | Render the fist N steps of Rule 30,
renderRule30 :: Int -> Line Cell -> Picture
renderRule30 n line = renderRule30Helper (n-1) n line

---

-- 1.3 Discrete spaces 

-- | A descrete 2D space with a focus.
-- A 2D space is merely a (vertical) line
-- where each element is a (horizontal) line.
data Space a = Space (Line (Line a)) deriving (Show)

-- Exercise 1.9
-- | A utility function to compute the product of an element by a line.
productOfElementByLine :: a -> Line b -> Line (a,b)
productOfElementByLine x (Line a b c ) = (Line ( map (\y -> (x,y)) a) (x,b) (( map (\y -> (x,y)) c)) )

-- | A utility function that computes the product of a list by a line.
productOfListByLine :: [a] -> Line b -> [Line (a,b)]
productOfListByLine xs b = map (\x -> productOfElementByLine x b) xs

-- | A function that computes the cartesian product of two lines.
productOfLines :: Line a -> Line b -> Space (a, b)
productOfLines (Line a b c) x = Space (Line (productOfListByLine a x) (productOfElementByLine b x) (productOfListByLine c x) )

-- Exercise 1.10 + 1.11

-- | A utility function that applies a function to a list of lines.
mapListOfLines :: (a -> b) -> [Line a] -> [Line b]
mapListOfLines f xs = map (\x -> mapLine f x) xs

-- | A function that applies a function to a space. It makes use of mapListOfLines.
mapSpace :: (a -> b) -> Space a -> Space b
mapSpace f (Space (Line a b c) ) = Space ( Line (mapListOfLines f a) (mapLine f b) (mapListOfLines f c) )

-- | A utility function that zips two lists of Lines.
zipListOfLines :: [Line a] -> [Line b] -> [Line (a,b) ]
zipListOfLines [] _ = []
zipListOfLines _ [] = []
zipListOfLines (x:xs) (y:ys) = ( zipLines x y : (zipListOfLines xs ys) )

-- | A function that zips two spaces together and makes use of zipListOfLines.
zipSpaces :: Space a -> Space b -> Space (a, b)
zipSpaces  (Space (Line a b c) ) (Space ( Line x y z) ) = Space ( Line (zipListOfLines a x) (zipLines b y) (zipListOfLines c z) )

-- | A utility function that zips two lists of lines with a combining function.
zipListOfLinesWith:: (a -> b -> c) -> [Line a] -> [Line b] -> [Line c]
zipListOfLinesWith f [] _ = []
zipListOfLinesWith f _ [] = []
zipListOfLinesWith f (x:xs) (y:ys) = ( (zipLinesWith f x y): (zipListOfLinesWith f xs ys) )

-- | A function that zips two Spaces. It makes use of zipListOfLinesWith. 
zipSpacesWith :: (a -> b -> c) -> Space a -> Space b -> Space c
zipSpacesWith f  (Space (Line a b c) ) (Space ( Line x y z) ) =  Space ( Line (zipListOfLinesWith f a x) (zipLinesWith f b y) (zipListOfLinesWith f c z) )

-- 1.4 Conway’s Game of Life

-- Exercise 1.12

-- A utility function that counts Alive cells in a list of cells. 
getAliveNeighborsCount :: [Cell] -> Int
getAliveNeighborsCount [] = 0
getAliveNeighborsCount (x:xs) = case x of 
  Alive -> 1 + getAliveNeighborsCount xs
  otherwise -> getAliveNeighborsCount xs

-- | Apply conwayRule to a space of cells, when there's only a central line.
-- If a cell doesn't exist, it's considered dead.
conwayRule :: Space Cell -> Cell
conwayRule (Space ( Line [] (Line is j ks) []  ) ) = do
  let i = iValue
  let k = kValue
  let count = getAliveNeighborsCount [i, k]
  let result = (if ( j == Alive && ( count == 2  || count == 3) ) then Alive else if ( j == Dead && count == 3 ) then Alive else Dead)
  result
  where
    iValue | (length is == 0) = Dead
           | otherwise = head is
    kValue | (length ks == 0) = Dead
           | otherwise = head ks
           
-- | Apply conwayRule to a space of cells, when there's only a central line and 
-- right lines.
conwayRule (Space ( Line [] (Line is j ks)  ( (Line ls m ns):ms)  ) ) = do
  let l = lValue
  let n = nValue
  let i = iValue
  let k = kValue
  let count = getAliveNeighborsCount [i, k, l, m, n]
  let result = (if ( j == Alive && ( count == 2  || count == 3) ) then Alive else if ( j == Dead && count == 3 ) then Alive else Dead)
  result
  where
    lValue | (length ls == 0) = Dead
           | otherwise = head ls
    nValue | (length ns == 0) = Dead
           | otherwise = head ns
    iValue | (length is == 0) = Dead
           | otherwise = head is
    kValue | (length ks == 0) = Dead
           | otherwise = head ks

-- | Apply conwayRule to a space of cells, when there's only a central line and 
-- left lines.
conwayRule (Space  ( Line ( (Line xs y zs):ys ) (Line is j ks) [] ) )  = do
  let x = xValue
  let z = zValue
  let i = iValue
  let k = kValue
  let count = getAliveNeighborsCount [x, y, z, i, k]
  let result = (if ( j == Alive && ( count == 2  || count == 3) ) then Alive else if ( j == Dead && count == 3 ) then Alive else Dead)
  result
  where
    xValue | (length xs == 0) = Dead
           | otherwise = head xs
    zValue | (length zs == 0) = Dead
           | otherwise = head zs
    iValue | (length is == 0) = Dead
           | otherwise = head is
    kValue | (length ks == 0) = Dead
           | otherwise = head ks
           
-- | Apply conwayRule to a space of cells, when there's a central line, right 
-- lines, and left lines.
conwayRule (Space  ( Line ( (Line xs y zs):ys ) (Line is j ks) ( (Line ls m ns):ms) ) )  = do
  let x = xValue
  let z = zValue
  let l = lValue
  let n = nValue
  let i = iValue
  let k = kValue
  let count = getAliveNeighborsCount [x, y, z, i, k, l, m, n]
  let result = (if ( j == Alive && ( count == 2  || count == 3) ) then Alive else if ( j == Dead && count == 3 ) then Alive else Dead)
  result
  where
    xValue | (length xs == 0) = Dead
           | otherwise = head xs
    zValue | (length zs == 0) = Dead
           | otherwise = head zs
    lValue | (length ls == 0) = Dead
           | otherwise = head ls
    nValue | (length ns == 0) = Dead
           | otherwise = head ns
    iValue | (length is == 0) = Dead
           | otherwise = head is
    kValue | (length ks == 0) = Dead
           | otherwise = head ks
           
  
-- Exercise 1.13
-- | A utility function that unwraps a list of Maybe a to have a list of 
-- values of a. It ignores the Nothing we could find at the end.
unwrapMaybe :: [Maybe a] -> [a]
unwrapMaybe [] = []
unwrapMaybe (Just x:xs) = (x:unwrapMaybe xs)
unwrapMaybe (Nothing:xs) = []

-- | A utility function that applies a shift left to all the lines of the space as long as it's possible.
-- The shift happens cells wise like it happened in the shiftLeft function above.
-- So, the cell focus within the line shifts.
shiftSpaceLeftCellWise :: Space a -> [Space a]
shiftSpaceLeftCellWise (Space (Line a b c) ) = case bShifted of 
    Just bResult -> [(Space (Line aShifted bResult cShifted) ) ] ++ shiftSpaceLeftCellWise (Space (Line aShifted  bResult cShifted ) )
    Nothing -> []
  where 
     bShifted = shiftLeft b
     aShifted = unwrapMaybe (map shiftLeft a)
     cShifted = unwrapMaybe (map shiftLeft c)

-- | A utility function that applies a shift right to all the lines of the space as long as it's possible.
-- The shift happens cells wise like it happened in the shiftRight function above.
-- So, the cell focus within the line shifts.
shiftSpaceRightCellWise :: Space a -> [Space a]
shiftSpaceRightCellWise (Space (Line a b c) ) = case bShifted of 
    Just bResult -> [(Space (Line aShifted bResult cShifted) ) ] ++ shiftSpaceRightCellWise (Space (Line aShifted bResult cShifted) )
    Nothing -> []
  where 
     bShifted = shiftRight b
     aShifted = unwrapMaybe (map shiftRight a)
     cShifted = unwrapMaybe (map shiftRight c)

-- | A utility function that applies a shift Left to the space, so that the line
-- in focus gets shifted to the left as long as it's possible. It then shifts the 
-- cells in focus to compute all possible shifts of cell focus using 
-- shiftSpaceLeftCellWise and shiftSpaceRightCellWise.
-- Meaning that this a shift from a higher dimension: so returns a list of lines 
-- of spaces.
shiftSpaceLeftLineWise :: Space a -> [Line (Space a) ]
shiftSpaceLeftLineWise (Space (Line [] b c) ) = []
shiftSpaceLeftLineWise (Space (Line a b c) ) = do
  let newSpace = Space ( Line (drop 1 a) (head a) ([b] ++ c) )
  let newLine = Line (shiftSpaceLeftCellWise newSpace ) newSpace (shiftSpaceRightCellWise newSpace) 
  let result = [newLine] ++ (shiftSpaceLeftLineWise newSpace)
  result
  
-- | A utility function that applies a shift Right to the space, so that the line
-- in focus gets shifted to the right as long as it's possible. It then shifts the 
-- cells in focus to compute all possible shifts of cell focus using 
-- shiftSpaceLeftCellWise and shiftSpaceRightCellWise.
-- Meaning that this a shift from a higher dimension: so, returns a list of lines 
-- of spaces.
shiftSpaceRightLineWise :: Space a -> [Line (Space a) ]
shiftSpaceRightLineWise (Space (Line a b []) ) = []
shiftSpaceRightLineWise (Space (Line a b c) ) =  do
  let newSpace =  Space( Line ([b] ++ a) (head c) (drop 1 c) )
  let newLine = Line (shiftSpaceLeftCellWise newSpace ) newSpace (shiftSpaceRightCellWise newSpace) 
  let result = [newLine] ++ (shiftSpaceLeftLineWise newSpace)
  result

-- | A function that converts each cell in a discrete space into a version of the
-- original space with focus shifted to that cell. The new space (of spaces)
-- must have the original space in focus. 
-- It achieves that by making use of shiftSpaceLeftLineWise to compute all the 
-- spaces to the left, and shiftSpaceRightLineWise to compute all sapces to the right.
-- It then makes use of shiftSpaceLeftCellWise and shiftSpaceRightCellWise for the 
-- central line.
spaceShifts :: Space a -> Space (Space a)
spaceShifts s = (Space( Line (shiftSpaceLeftLineWise s) (Line (shiftSpaceLeftCellWise s) s (shiftSpaceRightCellWise s) ) ( shiftSpaceRightLineWise s) ) )

-- apply conwayRule to each shifted version of the space to get new state for every cell.
applyConwayRule :: Space Cell -> Space Cell
applyConwayRule space = mapSpace conwayRule (spaceShifts space)
 
-- Exercise 1.14

-- | A utility function to render a list of lines
renderListOfLines :: [Line Picture] -> Picture
renderListOfLines [] = blank
renderListOfLines (x:xs) = renderLine x <> (translated 0 (-1) (renderListOfLines xs))

-- | Render a space of 1x1 pictures.
renderSpace :: Space Picture -> Picture
renderSpace (Space (Line xs y zs)) =   
  (translated 0 1 (renderListOfLines xs )) <> (renderLine y) <> (translated 0 (-1) (renderListOfLines zs ))


-- | Animate Conway's Game of Life, starting with a given space and updating it every second.
animateConway :: Space Cell -> IO ()
animateConway initial = activityOf (initial, 0) helper (renderSpace . mapSpace cellToPicture . fst)
  where
    helper (TimePassing dt) (cells, t)
      | t > 1     = (applyConwayRule cells, 0)
      | otherwise = (cells, t + dt)
    helper _ s = s

