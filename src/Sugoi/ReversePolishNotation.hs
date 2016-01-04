module Sugoi.ReversePolishNotation where

import Data.List
import Control.Monad

solveRPN :: String -> Maybe Double
solveRPN str = fmap head evaluated where
  list = words str
  evaluated = foldM foldingFunction [] list

readMaybe :: String -> Maybe Double
readMaybe str = case reads str of
  [(x, "")] -> Just x
  _ -> Nothing

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) "*" = Just ((y * x):ys)
foldingFunction (x:y:ys) "+" = Just ((y + x):ys)
foldingFunction (x:y:ys) "-" = Just ((y - x):ys)
foldingFunction xs numberString = fmap (:xs) (readMaybe numberString)
