-- | A value with explicitly separated computation steps.
data Iter a = Done a | Step (Iter a) deriving (Show) 

factorialIter :: Int -> Iter Int
factorialIter = go 1 where
    go current n
        | n <= 1 = Done current
        | otherwise = Step (go (n * current) (n - 1))

main :: IO()
main = do 
    -- AC: Step (Step (Done 6))
    print ( factorialIter 3 )
    -- AC: [1,2,3,5,7]
    print( insert 3 [1,2,5,7] ) 
    -- AC: [0,1,1,3]
    print ( insert 3 [0,1,1] )
    -- AC: [1,2,3,3,4]
    print(take 5 (insert 3 [1..]) )
    -- AC: Step (Step (Done 0.75))
    print( approximate (\x -> x^2 < 1) (/ 2) 3 )
    -- AC: Step (Step (Step (Step (Step (Done 9.375e-2)))))
    print (approximate (\x -> x^2 < 0.01) (/ 2) 3 )
    -- AC: Step (Step (Done [3]))
    print (approximate isSingleton (drop 1) [1..3])
    -- AC: 7.8125e-2
    print (eval (approximate (\x -> x^2 < 0.01) (/ 2) 10))
    -- AC: Step (Step (Step (Step (Step (Done (Just 9.375e-2))))))
    print (limit 100 (approximate (\x -> x^2 < 0.01) (/ 2) 3) )
    -- AC: Step (Step (Step (Done Nothing)))
    print( limit 3 (approximate (\x -> x^2 < 0.01) (/ 2) 3) )
    -- AC: Done Nothing
    print ( limit 0 (approximate (\x -> x^2 < 0.01) (/ 2) 3) )
    -- AC: Done 9.375e-2
    print( partialEval 100 (approximate (\x -> x^2 < 0.01) (/ 2) 3) )
    -- AC:  Step (Step (Done 9.375e-2))
    print( partialEval 3 (approximate (\x -> x^2 < 0.01) (/ 2) 3) )
    -- AC: 5
    print(  steps (approximate (\x -> x^2 < 0.01) (/ 2) 3) )
    -- AC: Done 4
    print ( mapIter (+1) (Done 3) )
    -- AC: Step (Step (Done 4))
    print ( mapIter (+1) (Step (Step (Done 3))) )
    -- AC: Step (Step (Done 3))
    print (joinIter (Step (Done (Step (Done 3)))))
    -- AC: (Done [1,2,3])
    print (insertIter 1 [2, 3] )
    -- AC: Step (Step (Done [2,3,4]))
    print(insertIter 4 [2, 3])
    -- AC: Step (Step (Step (Done [1,2,3,4])))
    print( insertionSortIter [1..4] )
    -- AC: Step (Step (Step (Step (Step (Step (Done [1,2,3,4]))))))
    print( insertionSortIter [4,3..1] )
    -- AC: 9
    print( steps (insertionSortIter [1..10]) )
    -- AC: 45
    print( steps (insertionSortIter [10,9..1]) )


-- 1.
insert :: Int -> [Int] -> [Int]
insert a (x:[]) = case (a >= x) of
    True -> [x,a]
    False -> [a,x]

insert a (x:xs) 
    | (a <= (head xs)) && (a >= x) = ([x,a] ++ xs)
    | (a <= x) = [a,x] ++ xs
    | otherwise  = (x:(insert a xs)) 

-- 2.
isSingleton :: [a] -> Bool
isSingleton (x:[]) = True
isSingleton (x:xs) = False

approximate :: (a -> Bool) -> (a -> a)  -> a -> Iter a
approximate f g x = case (f x) of 
    True -> (Done x)
    False -> Step ( approximate f g (g x) )

-- 3.a
eval :: Iter a -> a
eval x = case x of 
    (Step y) -> eval y 
    (Done y) -> y

-- 3.b
limit :: Int -> Iter a -> Iter (Maybe a)
limit n x
    | (n==0) = case x of 
        (Step y ) -> (Done Nothing)
        (Done y) -> (Done (Just y)) 
    | otherwise = case x of 
        (Step y ) -> Step (limit (n-1) y)
        (Done y) -> (Done (Just y) ) 

-- 3.c
partialEval :: Int -> Iter a -> Iter a
partialEval n x
    | (n <= 0) = x
    | otherwise = case x of 
        (Step y) -> partialEval (n-1) y
        (Done y) -> (Done y)
-- 3.d
steps :: Iter a -> Int
steps x = case x of 
    (Step y) -> 1 + steps y
    (Done y) -> 0

-- 4.a
mapIter :: (a -> b) -> Iter a -> Iter b
mapIter f x = case x of 
    (Step y) -> Step (mapIter f y)
    (Done y) -> Done (f y)

complexMapIter :: ([Int] -> Iter [Int]) -> Iter [Int]-> Iter [Int]
complexMapIter f x = case x of 
    (Step y) -> Step (complexMapIter f y)
    (Done y) -> (f y)
-- 4.b
joinIter :: Iter (Iter a) -> Iter a
joinIter x = case x of 
     (Step y) -> Step (joinIter y)
     (Done (Step z)) -> Step (partialEval 100 (Step z))
     (Done (Done z) ) -> Step (Done z)
-- 5
insertIter :: Int -> [Int] -> Iter [Int]
insertIter a [] = Done [a]
insertIter a (x:[]) = case (a >= x) of
    True -> Step (Done [x,a])
    False -> Step (Done [a,x])
insertIter a (x:xs) 
    | (a <= (head xs)) && (a >= x) = Step(Done ([x,a] ++ xs))
    | (a <= x) = Step( Done ([a,x] ++ xs) )
    | otherwise  = do
        let result = insertIter a xs
        Step ( mapIter (x:) result )
-- 6
insertionSortIter :: [Int] -> Iter [Int]
insertionSortIter [] = Done []
insertionSortIter (x:[]) = insertIter x []
insertionSortIter (x:xs) = (complexMapIter (insertIter x) )  (insertionSortIter xs) 
