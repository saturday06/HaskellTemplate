module Nanashi where

import Data.Function ((&))
import Data.Char
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Trans.Maybe

fib x = case x of
  1 -> 1
  2 -> 1
  x -> fib (x - 1) + fib (x - 2)

first(x:xs) = x

length' [] = 0
length'(_:xs) = length' xs + 1

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

product' [] = 1
product' (x:xs) = x * product' xs

reverse' :: [Int] -> [Int]
reverse' [] = []
reverse' (x:xs) = xs ++ [x]

take' :: Int -> [Int] -> [Int]
take' 0 list = []
take' n (x:xs) = x : (take (n - 1) xs)

drop' :: Int -> [Int] -> [Int]
drop' 0 list = []
drop' n (x:xs) = (drop (n - 1) xs)

fact' :: Int -> Int
fact' n = product [1 .. n]

addsub x y = (x + y, x - y)

perpPoint :: (Int, Int) -> (Int, Int) -> (Int, Int)
perpPoint (a, b) (c, d) = (a * c, b * d)

rot13 :: [Char] -> [Char]
rot13 [] = ""
rot13 (x:xs) = rot13Char x : (rot13 xs)
  where
    rot13Char' char baseChar = chr ((ord baseChar) + ((ord x) - (ord baseChar) + 13) `mod` 26)
    rot13Char :: Char -> Char
    rot13Char x | ord x < ord 'A' = x
    rot13Char x | ord x < ord 'a' = rot13Char' x 'A'
    rot13Char x | ord x <= ord 'z' = rot13Char' x 'a'
    rot13Char x = x

binSmalls :: Int -> Int -> Maybe Int
binSmalls acc x
  | x > 9 = Nothing
  | otherwise = Just (acc + x)

writeAccW :: Int -> Int -> WriterT String Maybe Int
writeAccW acc x
  | x > 9 = fail "fail"
  | otherwise = do
    tell $ "acc=" ++ show acc ++ ",x=" ++ show x ++ " | "
    return $ acc + x
{--
newtype MyMaybeT m a = MyMaybeT { runMyMaybeT :: m (Maybe a) }

instance Monad m => Monad (MyMaybeT m) where
  return x = MyMaybeT $ return $ Just x
  (>>=) x y = MyMaybeT $ case x of
     Nothing -> return
     Just a -> (y a)
-}
{--
writeAccM :: Int -> Int -> MyMaybeT (Writer String) Int
writeAccM acc x
  | x > 9 = fail "fail"
  | otherwise = do
    myTell $ "acc=" ++ show acc ++ ",x=" ++ show x ++ " | "
    return $ acc + x
-}
