module Nanashi.ActionSpec where

import Nanashi.Action
import Test.Hspec    (Spec, describe, it, shouldBe)
import Data.Function ((&))
import Data.Char
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Trans.Maybe
import Debug.Trace

spec :: Spec
spec = do
  it "works!" $ do
    1 `shouldBe` 1
