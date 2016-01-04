module Sugoi.ReversePolishNotationSpec where

import Sugoi.ReversePolishNotation
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

spec :: Spec
spec = do
  it "works!" $ do
    -- solveRPN "1" `shouldBe` Just 1
    -- solveRPN "1 2 +" `shouldBe` Just 3
    -- solveRPN "1 2 --" `shouldBe` Just 3
    readMaybe "1" `shouldBe` Just 1
    readMaybe "str" `shouldBe` Nothing
