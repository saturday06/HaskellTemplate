module Nanashi.Action where

import Data.Function ((&))
import Data.Char
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Trans.Maybe
import Debug.Trace
import System.Random
import Data.IORef
import Data.Array.IO

randAlpha = getStdRandom $ randomR ('A', 'Z')

dice = getStdRandom $ randomR (1, 6) :: IO Int

test = do
  alpha <- randAlpha
  print alpha

printRandomUntilZ = do
  alpha <- randAlpha
  print alpha
  if alpha == 'Z' then
    print "END"
  else
    printRandomUntilZ

add x y = do
  x + y & pure

fact 0 = return 1
fact n | n > 0 = do
  x <- fact (n - 1)
  return $ n * x

setElementAt :: [Int] -> Int -> Int -> [Int]
setElementAt list pos value = before ++ [value] ++ after
  where
    before = take pos list
    after = drop (pos + 1) list

sorted :: [Int] -> Bool
sorted [] = True
sorted [x] = True
sorted (x:y:rest) = (x < y) && (sorted $ y:rest)

shuffle :: [Int] -> IO [Int]
shuffle [] = return []
shuffle list = do
  index <- getStdRandom $ randomR (0, length list - 1) :: IO Int
  let x = list !! 0
      y = list !! index
  list <- return $ setElementAt list 0 y
  list <- return $ setElementAt list index x
  (head:tail) <- return list
  tail <- shuffle tail
  return (head:tail)

bogosort :: [Int] -> IO [Int]
bogosort [] = return []
bogosort list = do
  if sorted list then
    return list
  else do
    shuffledList <- shuffle list
    bogosort shuffledList

add3 x y z = x + y + z

fib 0 = return 0
fib 1 = return 1
fib n | n > 1 = (+) <$> fib (n - 1) <*> fib (n - 2)

qsort :: [Int] -> IO [Int]
qsort []     = return []
qsort (n:xs) = do
    putStrLn dbg
    l <- qsort lt
    r <- qsort gteq
    return $ l ++ [n] ++ r
    where
        dbg  = "qsort " ++ show (n:xs) ++ " = qsort " ++
               show lt ++ " ++ " ++ show [n] ++ " ++ " ++ show gteq
        lt   = [x | x <- xs, x <  n]
        gteq = [x | x <- xs, x >= n]

counter = do
  ioref <- newIORef 0
  return $ do
    var <- readIORef ioref
    writeIORef ioref $ var + 1
    readIORef ioref

jsloop :: IO ()
jsloop = do
  s <- newIORef 0
  let
    acc count | count < 100 = do
      var <- readIORef s
      writeIORef s $ var + count
      acc $ count + 1
    acc _ = return 0
  acc 1
  var <- readIORef s
  putStrLn $ show var
  return ()

jsloop2 :: IO ()
jsloop2 = do
  let
    acc :: Int -> Int
    acc i | i < 100 = i + (acc $ i + 1)
    acc _ = 0
    total = acc 1
  putStrLn $ show total
  return ()
  a <- newArray (0, 2) 0 :: IO (IOUArray Int Int)
  return ()

inc :: Monad m => Int -> m Int
inc x = return $ x + 1
