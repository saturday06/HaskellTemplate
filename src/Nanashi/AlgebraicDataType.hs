module Nanashi.AlgebraicDataType where

data Color = Red | Green | Blue | Cyan | Magenta | Yellow deriving (Show, Enum, Eq)

mix :: Color -> Color -> Color
mix l r
 | l == r = l
 | fromEnum l > fromEnum r = mix r l
mix Red Green = Yellow
mix Red Blue = Magenta
mix Green Blue = Cyan

data Point = Point { px :: Int, py :: Int } | Point3D Int Int Int deriving (Show, Eq)

offset :: Point -> Point -> Point
offset (Point x y) (Point a b) = Point (x - a) (y - b)

newtype Foo = Foo Int

data Rect = Rect { x :: Int, y :: Int, w :: Int, h :: Int} | Rect3D Int Int Int Int Int Int

contains (Rect3D x y z w h t) (Point3D px py pz)
  | x <= px && y <= py && z <= pz && px < x + w && py < y + h && pz < z + t = True
  | otherwise = False

contains r p
  | (x r) <= (px p) && (y r) <= (py p) && (px p) < (x r) + (w r) && (py p) < (y r) + (h r) = True
  | otherwise = False

data Test = TestInt Int | TestStr String deriving Show

formatTest (TestInt x) = "TestInt " ++ show x
formatTest (TestStr x) = "TestStr " ++ x

type MyString = [Char]

data Bar = Bar { hoge :: Int, hige :: String }
