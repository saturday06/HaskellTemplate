module Nanashi.AlgebraicDataTypeSpec where

import Test.Hspec    (Spec, describe, it, shouldBe)
import Data.Function ((&))
import Data.Char
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Trans.Maybe
import Debug.Trace
import Nanashi.AlgebraicDataType

spec :: Spec
spec = do
  it "works!" $ do
    print Red
    -- (Just 3 >>= (\x -> (if True then Nothing else Just 3))) `shouldBe` (Just 6)
    fromEnum Blue `shouldBe` 2
    fromEnum Red `shouldBe` 0
    (toEnum 0 :: Color) `shouldBe` Red
    let Point x y = Point 3 4
    (x, y) `shouldBe` (3, 4)
    contains (Rect 2 2 3 3) (Point 1 1) `shouldBe` False
    contains (Rect 2 2 3 3) (Point 2 2) `shouldBe` True
    contains (Rect 2 2 3 3) (Point 3 3) `shouldBe` True
    contains (Rect 2 2 3 3) (Point 4 4) `shouldBe` True
    contains (Rect 2 2 3 3) (Point 5 5) `shouldBe` False
    formatTest (TestInt 3) `shouldBe` "TestInt 3"
    formatTest (TestStr "x") `shouldBe` "TestStr x"

    contains (Rect 2 2 3 3) (Point 1 1) `shouldBe` False
    contains (Rect 2 2 3 3) (Point 2 2) `shouldBe` True
    contains (Rect 2 2 3 3) (Point 3 3) `shouldBe` True
    contains (Rect 2 2 3 3) (Point 4 4) `shouldBe` True
    contains (Rect 2 2 3 3) (Point 5 5) `shouldBe` False

    contains (Rect3D 2 2 2 3 3 3) (Point3D 1 1 1) `shouldBe` False
    contains (Rect3D 2 2 2 3 3 3) (Point3D 2 2 2) `shouldBe` True
    contains (Rect3D 2 2 2 3 3 3) (Point3D 3 3 3) `shouldBe` True
    contains (Rect3D 2 2 2 3 3 3) (Point3D 4 4 4) `shouldBe` True
    contains (Rect3D 2 2 2 3 3 3) (Point3D 5 5 5) `shouldBe` False
    let bar = Bar { hoge = 3, hige = "3" }
    let bar2 = bar { hoge = 4 }
    let Bar { hoge = h } = bar
    (bar & hoge) `shouldBe` 3
    h `shouldBe` 3

