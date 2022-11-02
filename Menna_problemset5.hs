main :: IO()
main = do
  -- AC: 22
  print (binaryToDecimal binaryList)
  -- AC: 2
  print (countZeroes leadingZeroesBinaryList ) 
  print ( binaryOdd binaryOddFalseList )
  print ( binaryOdd binaryOddTrueList )
  -- AC: [1,0,1,0,1]
  print ( decrement [1,0,1,1,0] )
  -- AC: [1,1,1,1]
  print ( decrement [1,0,0,0,0] )
  -- AC: [0]
  print( decrement [0] )
  -- AC: [2,1,3,2]
  print( encodeWithLengths [0,0,0,1,1,0,1,1,1,0,0]  )
  -- AC: [(False,1),(False,2),(False,3)]
  print ( propagate (False, [1, 2, 3]) ) 
  -- AC: [(True,1),(True,1)]
  print ( propagate (True, [1, 1]) )
  

binaryList :: [Int]
binaryList = [1, 0, 1, 1, 0]

leadingZeroesBinaryList :: [Int] 
leadingZeroesBinaryList = [0, 0, 0, 1, 0, 1, 1, 0]

binaryOddFalseList = [1,0,1,1,0]
binaryOddTrueList = [1,0,1,1,1] 

-- 1.a
binaryToDecimal [] = 0
binaryToDecimal (x:xs) =  x * 2 ^ (length xs) + binaryToDecimal xs

-- 1.b
countZeroes :: [Int] -> Int
countZeroes [] = 0
countZeroes (x:xs) =   calculateZeroCount (x:xs) False

calculateZeroCount [] _ = 0
calculateZeroCount (0:xs) False = calculateZeroCount xs False
calculateZeroCount (0:xs) True = calculateZeroCount xs True + 1 
calculateZeroCount (1:xs) _ = calculateZeroCount xs True
  
-- 1.c
encodeWithLengths :: [Int] -> [Int]
encodeWithLengths [] = []
encodeWithLengths (0:xs) = drop 1 (encodeHelper (0:xs) 0 0)
encodeWithLengths (x:xs) = (encodeHelper (x:xs) 0 0)
  
encodeHelper :: [Int] -> Int -> Int -> [Int]
encodeHelper [] _ count = [count]

encodeHelper (0:xs) 0 count = encodeHelper xs 0 (count + 1) 
encodeHelper (1:xs) 0 count = count:encodeHelper xs 1 1

encodeHelper (1:xs) 1 count = encodeHelper xs 1 (count + 1) 
encodeHelper (0:xs) 1 count = count:encodeHelper xs 0 1

-- 1.d

binaryOdd [] = False
binaryOdd (x:[]) = (x==1)
binaryOdd (x:xs) = binaryOdd xs

-- 1.e
decrement [] = []
decrement [0] = [0]
decrement xs 
  | (y == 0) = ys
  | otherwise = y:ys
  where (y:ys) = decrementHelper xs
  
decrementHelper :: [Int] -> [Int]
decrementHelper [] = []
decrementHelper(x:xs)
   |(doesSubListHaveOne xs) = x:(decrementHelper xs)
   |otherwise = (1-x):decrementHelper xs  


doesSubListHaveOne [] = False
doesSubListHaveOne (1:_) = True
doesSubListHaveOne (0:xs) = doesSubListHaveOne xs

-- 1.f
propagate :: (Bool, [Int]) -> [(Bool, Int)]
propagate (b,[]) = []
propagate (b,(x:xs)) = (b,x):(propagate (b, xs) )

-- 2.a
alternatingSum :: [Int] -> Int
alternatingSum [] = 0
alternatingSum (a:[]) = a
alternatingSum (a:b:xs) = a - b + alternatingSum(xs)

-- 2.b
-- alternatingSum [1,2,3,4,5]   is called
-- alternatingSum (1, 2, [3,4,5])
-- returns 1 - 2 + alternatingSum (3,4,[5]) 
-- -> returns 1 - 2 + 3 - 4 + alternatingSum [5]
-- -> returns 1 - 2 + 3 - 4 + 5 
-- 5 - 4 + 3 -2 + 1 = 3

-- 3.
data Radians = Radians Double
data Degrees = Degrees Double
pI :: Double
pI = 3.14159

toDegrees :: Radians  -> Degrees
toDegrees (Radians r) = Degrees (r * 180 / pI) 

fromDegrees :: Degrees  -> Radians
fromDegrees ( Degrees d ) = Radians (d/180 * pI )
