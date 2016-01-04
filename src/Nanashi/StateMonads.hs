module Nanashi.StateMonads where

import Test.Hspec    (Spec, describe, it, shouldBe)
import Data.Function ((&))
import Data.Char
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Trans.Maybe
import Debug.Trace
import Data.IORef
import Data.Array.IO
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.ST
import Data.STRef
import Data.Array.ST
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.List

foo :: State s Int
foo = return 1

bar :: State s Int
bar = state (\s -> (1, s))

test :: State String Int
test = do
  str <- get
  put $ str ++ "o"
  modify (++ "x")
  str <- get
  return $ length str

sumx :: [Int] -> IO Int
sumx list = (`execStateT` 0) $ do
  forM list $ \x -> do
    modify (+ x)
    var <- get
    return $ putStrLn $ "var " ++ show var

stateident :: Maybe Int
stateident = do
  let st = StateT $ \s -> Just (100, s)
  evalStateT st ()

return' a = StateT $ \x -> Identity (a, x)

runState' s i = runIdentity $ runStateT s i

getch f = StateT getch' where
  getch' (x:xs) = if f x then Just (x, xs) else Nothing
  getch' _ = Nothing

testa :: String -> Maybe String
testa s = (`evalStateT` s) $ do
  ch1 <- getch isUpper
  ch2 <- getch isLower
  ch3 <- getch isDigit
  return [ch1, ch2, ch3]

testR :: Int -> IO ()
testR x = (`runReaderT` x) $ do
  a <- ask
  lift $ putStrLn $ show a

testW :: Int -> IO String
testW x = snd <$> (runWriterT $ do
    tell "Hello!"
    tell "Hello!"
    lift $ putStrLn $ show x
  )

testL :: Int -> IO [Int]
testL x = runListT $ do
  lift $ putStrLn $ show x
  return $ x + 1

test3 = liftM (\x -> "foo") (Just 3)
