module Main where

data Point = Point { x :: Number, y :: Number }

data Foo = Foo | Bar String

newtype Percentage = Percentage Number

point :: Number -> Number -> Point
point x y = Point { x: x, y: y }