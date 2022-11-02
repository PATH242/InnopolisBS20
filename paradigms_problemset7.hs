import Data.Char (toUpper)
import Distribution.Simple.PackageIndex (deletePackageName)

verboseCons :: Int -> [Int] -> IO [Int]
verboseCons x xs = do
    putStrLn ("prepending " ++ show x ++ " to " ++ show xs)
    return (x:xs)

main :: IO ()
main = do
    s <- forStateIO_ [] [1, 2, 3] verboseCons
    print s
    example

-- 1.
-- getLine :: IO String
-- s :: String:r
-- g :: (String -> IO t1)
-- g s :: IO t1
-- x :: t1
-- p :: (t1 -> Bool)
-- p x :: Bool
-- return x :: IO t1

guess :: (t1 -> Bool) -> (String -> IO t1) -> IO t1
guess p g = do
  s <- getLine
  x <- g s
  case p x of
    True -> pure x
    False -> guess p g


-- 2.
echo :: IO ()
echo = do
  x <- getLine
  putStrLn (map toUpper x)
  echo
-- 3.a
foreverIO :: IO a -> IO b
foreverIO a = do
    a
    foreverIO a
-- 3.b
whenIO :: Bool -> IO () -> IO () 
whenIO True a = a 
whenIO False b = return ()

-- 3.c
maybeIO :: Maybe (IO a) -> IO (Maybe a) 
maybeIO Nothing = return Nothing
maybeIO (Just a) = do
    b <- a
    return (Just b)

-- 3.d
sequenceMaybeIO :: [IO (Maybe a)] -> IO [a]
sequenceMaybeIO (x:xs) = do
    m <- x
    case m of 
        Just a -> do
                xsUnwrapped <- sequenceMaybeIO xs
                return (a:xsUnwrapped)
        Nothing ->  sequenceMaybeIO xs
        
-- 3.e 
whileJustIO :: (a -> IO (Maybe a)) -> a -> IO ()
whileJustIO f a = do
    b <- (f a)
    case b of
        Nothing -> return ()
        Just b -> ( whileJustIO f b )

 -- 3.f


forStateIO_ :: s -> [a] -> (a -> s -> IO s) -> IO s
forStateIO_ st [] f = return st
forStateIO_ st (x:xs) f = do
    i <- (f x st)
    b <- forStateIO_ i xs f 
    return b


iforIO_ :: [a] -> (Int -> a -> IO ()) -> IO ()
iforIO_ [] f = return()
iforIO_ (x:xs) f = do
    helper (x:xs) f 0
    where
        helper [] f i = return()
        helper (x:xs) f i = do
            y <- (f i x)
            helper xs f (i+1)


example :: IO ()
example = do
    iforIO_ [1, 2] (\i n ->
        iforIO_ "ab" (\j c ->
            print ((i, j), replicate n c)))
