module Nanashi.ActionSpec where

import Nanashi.Action
import Test.Hspec    (Spec, describe, it, shouldBe)
import Data.Function ((&))
import Data.Char
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Trans.Maybe
import Debug.Trace
import Data.IORef
import Data.Array.IO

spec :: Spec
spec = do
  it "works!" $ do
    {-
    alpha <- randAlpha
    (print . show) =<< randAlpha
    -- alpha `shouldBe` 'a'

    test
    test
    test
    test
-}
    -- printRandomUntilZ
    {-
    print =<< add 1 2
    fact5 <- fact 5
    fact5 `shouldBe` 120
    print =<< dice
    print =<< dice
    print =<< dice -}
    print =<< shuffle [1, 2, 3, 4, 5]
    print =<< shuffle [1, 2, 3, 4, 5]
    print =<< shuffle [1, 2, 3, 4, 5]
    print =<< shuffle [1, 2, 3, 4, 5]
    print =<< shuffle [1, 2, 3, 4, 5]

  it "setElementAt" $ do
    setElementAt [1] 0 100 `shouldBe` [100]
    setElementAt [1, 2] 0 100 `shouldBe` [100, 2]
    setElementAt [1, 2] 1 100 `shouldBe` [1, 100]
    setElementAt [1, 2, 3] 0 100 `shouldBe` [100, 2, 3]
    setElementAt [1, 2, 3] 1 100 `shouldBe` [1, 100, 3]
    setElementAt [1, 2, 3] 2 100 `shouldBe` [1, 2, 100]

  it "bogosort" $ do
    l <- bogosort [6, 3, 5, 1]
    l `shouldBe` [1, 3, 5, 6]
    let a = print "foo"
    a
    a
    a
    b <- add3 <$> (return 3) <*> (return 4) <*> (return 5)
    b `shouldBe` 12
    c <- ((return 3) & fmap add3) <*> (return 3) <*> (return 3)
    c `shouldBe` 9
    fib6 <- fib 6
    fib6 `shouldBe` 8
    s <- qsort [4, 6, 9, 8, 3, 5, 1, 7, 2]
    s `shouldBe` [1, 2, 3, 4, 5, 6, 7, 8, 9]

    var1 <- newIORef 100
    writeIORef var1 100
    shouldBe 100 =<< readIORef var1
    {-
    let c = counter
    print =<< c
    print =<< c
    print =<< c
    print =<< c-}
    2 `shouldBe` 2

  it "loop" $ do
    2 `shouldBe` 2
    x <- jsloop
    x `shouldBe` ()
    x <- jsloop2
    x `shouldBe` ()

    join (Just (Just 3)) `shouldBe` (Just 3)