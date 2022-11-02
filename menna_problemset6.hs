
type Name = String
data Grade = A | B | C | D
data Student = Student Name Grade 

data Result a = 
  Success a | 
  Failure String deriving(Show)
  
main = do
  exercise01
  -- 2. AC: ["Jane"]
  print (studentsWithA [Student "Jack" D, Student "Jane" A])
  let example1 = ( whileSuccess doubleIntUntil100 1 )
  -- 3.a AC: 128
  print(example1)
  -- 3.b AC: Success 3
  let ex3b = applyResult (Success length) (Success [1, 2, 3])
  print(ex3b)
  -- 3.b AC:  Failure "no function"
  let ex3b2 = (applyResult (Failure "no function") (Failure "no arg") )
  print ( ex3b2 :: Result Int )
  -- 3.c AC:4
  let ex3c =  fromResult (+1) length (Success 3)
  print(ex3c)
  -- 3.c AC: 12
  let ex3c2 =  fromResult (+1) length (Failure "not a number")
  print( ex3c2)
  -- 3.d AC: Success 5
  let ex3d = combineResultsWith (+) (Success 2) (Success 3)
  print(ex3d)
  -- 3.d AC: Failure "x is undefined"
  let ex3d2 = combineResultsWith (+) (Failure "x is undefined") (Failure "crash")
  print (ex3d2)


-- 1.
-- The most generic type of dup is: 'dup :: (a -> a -> b) -> a -> b'
-- since dup is applied to 2 arguments, the first being a function that
-- takes 2 arguments and returns a different argument, the second being same type arguemnt,
-- and then dup returns the different argument. 
dup :: (a -> a -> b) -> a -> b
dup f x = f x x
-- The most generic type of dip is: 'dip :: (a -> a -> a) -> a -> a -> a'
-- since dip is applied to 3 arguments, the first being a function that
-- takes 2 arguments and returns 1, the second and third being same type
-- arguemnts, and then dip returns a same type argument. 
dip :: (a -> a -> a) -> a -> a -> a
dip f x = f ( f x x )
-- The most generic type of twice is: 'twice:: (a -> a) -> a -> a'
-- since twice is applied to 2 arguments, the first being a function that
-- takes 1 argument and returns 1, the second being same type argument,
-- and then twice returns a same type argument. 
twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- (a)
exercise01 = do
  print ("exercise 01:")
  -- 1) Type: Int. 
  print ( dip (+) 1 2 )
  -- b) Type: Int 
  print( dup (dip (+)) 1)
  -- c) Not enough arguments for twice or for dip.
  -- print(twice dip)
  -- d) Type error: Cannot construct the infinite type. 
  -- print(dip dip)
  -- e) Not enough arguments
  -- print(twice twice twice)
  -- f) Type error: Cannot construct the infinite type.
  -- print(dup twice)
  print("_____________")

-- 2.
studentsWithA :: [Student] -> [Name]
studentsWithA [] = []
studentsWithA (Student name A : xs ) = name:studentsWithA xs  
studentsWithA(x:xs) = studentsWithA xs

-- 3.a

doubleIntUntil100 :: Int -> (Result Int)
doubleIntUntil100 n  
  | n > 100 = Failure "Input is too large"
  | otherwise = Success (2 * n)
  
whileSuccess :: (a -> Result a ) -> a -> a   
whileSuccess f x = whileSuccessHelper (f x) x
  where 
    -- helper is made to process the result of fx
    whileSuccessHelper (Success y) x = whileSuccessHelper (f y) y
    whileSuccessHelper (Failure _) x = x
    
-- 3.b
applyResult :: Result (a -> b) -> Result a -> Result b
applyResult (Success x) (Success y) = Success (x y)
applyResult (Failure x) _ = Failure x
applyResult _ (Failure y) = Failure y

-- 3.c
fromResult :: (a -> b) -> (String -> b) -> Result a -> b
fromResult f _ (Success x ) = (f x)
fromResult _ g (Failure x) = ( g(x) ) 

-- 3.d
combineResultsWith :: (a -> b -> c) -> Result a -> Result b -> Result c
combineResultsWith f (Failure a) _ = (Failure a)
combineResultsWith f _ (Failure b) = (Failure b)
combineResultsWith f (Success a) (Success b) = ( Success (f a b) )