module Nanashi.FirstSpec where

import Nanashi.First
import Test.Hspec    (Spec, describe, it, shouldBe)
import Data.Function ((&))
import Data.Char
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Trans.Maybe
import Debug.Trace

spec :: Spec
spec = do
  describe "ok" $ do
    it "yes precure" $ do
      1 `shouldBe` 1

  it "works!" $ do
    fib 1 `shouldBe` 1
    fib 2 `shouldBe` 1
    fib 3 `shouldBe` 2
    fib 4 `shouldBe` 3
    fib 5 `shouldBe` 5
    2:1:[1,2,3]++[0] `shouldBe` [2,1,1,2,3,0]
    first [1,2,3] `shouldBe` 1
    length [1,2,3] `shouldBe` 3
    sum [1,2,3] `shouldBe` 6
    product [1,2,4] `shouldBe` 8
    reverse [1..3] `shouldBe` [3,2,1]
    length' [] `shouldBe` 0
    length' [1,2,3] `shouldBe` 3
    let datum = [[], [0], [1], [1, 2, 5, -2], [-1, -2]]
    (datum & map sum') `shouldBe` (datum & map sum)
    (datum & map product') `shouldBe` (datum & map product)
    take 2 [1,2,3] `shouldBe` [1, 2]
    reverse' [2] `shouldBe` reverse [2]
    reverse' [2, 1] `shouldBe` [1, 2]
    take' 1 [1, 2] `shouldBe` take 1 [1, 2]
    take' 2 [1, 2] `shouldBe` take 2 [1, 2]
    drop' 1 [1, 2] `shouldBe` drop 1 [1, 2]
    drop' 2 [1, 2] `shouldBe` drop 2 [1, 2]
    fact' 3 `shouldBe` 6
    fact' 5 `shouldBe` (5 * 4 * 3 * 2 * 1)
    addsub 1 2 `shouldBe` (3, -1)
    let (a, s) = addsub 1 2
    a `shouldBe` 3
    s `shouldBe` (-1)
    snd (1, 2) `shouldBe` 2
    let (f, s, t) = (1, 2, 3)
    t `shouldBe` 3
    perpPoint (1, 1) (2, 2) `shouldBe` (2, 2)
    ord 'A' `shouldBe` 65
    chr 65 `shouldBe` 'A'
    rot13 "Hello, World!" `shouldBe` "Uryyb, Jbeyq!"

  it "works!" $ do
    -- (runWriterT $ foldM writeAccW 0 [2,8,3,1]) `shouldBe` Just (14, "acc=0,x=2 | acc=2,x=8 | acc=10,x=3 | acc=13,x=1 | ")
    -- (runWriter $ runMyMaybeT $ foldM writeAccM 0 [2,8,3,1]) `shouldBe` (Just 14, "acc=0,x=2 | acc=2,x=8 | acc=10,x=3 | acc=13,x=1 | ")
    fib 1 `shouldBe` 1

  it "inserts" $ do
    insert 1 [2] `shouldBe` [1, 2]
    insert 2 [1] `shouldBe` [1, 2]
    insert 2 [1, 3] `shouldBe` [1, 2, 3]

  it "isorts" $ do
    isort [4, 6, 9, 8, 3, 5, 1, 7, 2] `shouldBe` [1, 2, 3, 4, 5, 6, 7, 8, 9]

  it "bubblesort" $ do
    bubblesort [8, 6, 9, 4] `shouldBe` [4, 6, 8, 9]

  it "merges" $ do
    merge [1] [2] `shouldBe` [1, 2]
    merge [2] [1] `shouldBe` [1, 2]
    merge2 [1] `shouldBe` [1]

  it "mergesort" $ do
    bubble [4, 3, 2, 1] `shouldBe` [1, 4, 3, 2]
    mergesort [8, 6, 9, 4] `shouldBe` [4, 6, 8, 9]
    [x * 2 | x <- [1], x < 5] `shouldBe` [2]

  it "pita" $ do
    pita `shouldBe` [(3,4,5),(5,12,13),(6,8,10),(8,15,17),(9,12,15),(12,16,20)]
