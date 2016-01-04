module Nanashi.StateMonadsSpec where

import Nanashi.StateMonads
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
    1 `shouldBe` 1
    shouldBe 1 $ evalState foo ()
    shouldBe 1 $ evalState bar ()

  it "tests!" $ do
    5 `shouldBe` evalState test "foo"
    "fooox" `shouldBe` execState test "foo"
    s <- sumx [1..100]
    s `shouldBe` 5050
    stateident `shouldBe` Just 100
    testa "Ab1" `shouldBe` Just "Ab1"
    testa "aaa" `shouldBe` Nothing

    testR 5
    tw <- testW 3
    tw `shouldBe` "Hello!Hello!"
    -- testL 0
    1 `shouldBe` 1
