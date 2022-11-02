{-# LANGUAGE ParallelListComp #-}
import Distribution.Simple.PackageIndex (deletePackageName)
main :: IO()
main = do
    -- AC: True
    print(isSingleton [1])
    -- AC: False
    print (isSingleton [1..])
    -- AC: True
    print (isSingleton [[1..]])
    -- AC: [1,2,3,5,7]
    print( insert 3 [1,2,5,7])
    -- AC: [0,1,1,3]
    print( insert 3 [0,1,1] )
    -- AC: [1,2,3,3,4]
    print( take 5 (insert 3 [1..]) )
    -- AC: [1,0,2,0,3]
    print( take 5 (separateBy 0 [1..]) )
    -- AC: ("Hello,"," world!")
    print (splitWhenNot (/= ' ') "Hello, world!")
    -- AC: ["Here","are","some","words!"]
    print( groupsSeparatedBy (== ' ') "Here are some words!")
    -- AC: [[1,2,3],[5,6,7],[9,10,11]]
    print( take 3 (groupsSeparatedBy (\n -> n `mod` 4 == 0) [1..]) )
    -- AC: [1,2,2,3,3,3]
    print( replicateWithPos [1..3] )
    -- AC: "Heelllllllooooo"
    print( replicateWithPos "Hello")
    -- AC: [1,2,2,3,3,3,4,4,4,4]
    print( take 10 (replicateWithPos [1..]) )
    -- AC: [2,1,3,4,7,11,18,29,47,76]
    print (take 10 lucas)
    -- AC: [1.0,1.5,1.4166666666666665,1.4142156862745097,1.4142135623746899]
    print (take 5 (approximationsOfRoot2 1) )




-- 1.a
isSingleton :: [a] -> Bool
isSingleton (x:[]) = True
isSingleton (x:xs) = False
-- 1.b
insert :: Int -> [Int] -> [Int]
insert a (x:[]) = case (a >= x) of
    True -> [x,a]
    False -> [a,x]

insert a (x:xs) 
    | (a <= (head xs)) && (a >= x) = ([x,a] ++ xs)
    | otherwise  = (x:(insert a xs)) 

-- 1.c
separateBy :: a -> [a] -> [a]
separateBy c [] = []
separateBy c (x:xs) = [x,c] ++ (separateBy c xs)


-- 1.d
splitWhenNot :: (a -> Bool) -> [a] -> ([a], [a])
splitWhenNot f (x:xs) = case f x of 
     True -> do
          let (y,z) = splitWhenNot f xs
          ((x:y), z)
     False -> ([],(x:xs))

-- 1.e
groupsSeparatedBy :: (a -> Bool) -> [a] -> [[a]]
groupsSeparatedBy f [] = [[]]
groupsSeparatedBy f (x:xs) = do
    case (f x) of 
        True -> [[x]] ++ groupsSeparatedBy f xs
        False -> do
            let y = (groupsSeparatedBy f xs)
            [(x:(head y))] ++ (drop 1 y) 

-- 1.f
replicateWithPos :: [a] -> [a]
replicateWithPos [] = []
replicateWithPos xs = replicateWithPosHelper xs 1

replicateWithPosHelper :: [a] -> Int -> [a]
replicateWithPosHelper [] i = []
replicateWithPosHelper (x:xs) i = replicateA x i ++ replicateWithPosHelper xs (i+1)

replicateA :: a -> Int -> [a]
replicateA x i 
    | (i == 0) = []
    | otherwise = ([x] ++ replicateA x (i-1))

-- 2.a
lucas :: [Int]
lucas = (2 : 1 : [a+b | a <- lucas | b <- tail lucas])

-- 2.b
approximationsOfRoot2 :: Double -> [Double]
approximationsOfRoot2 d = (d:approximationsOfRoot2(d - d/2 + 1/d))
