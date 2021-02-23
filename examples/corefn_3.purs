module Main where

class MyShow a where
  myShow :: a -> String

instance myShowString :: MyShow String where
  myShow s = s

instance myShowBoolean :: MyShow Boolean where
  myShow true = "true"
  myShow false = "false"

instance myShowInt :: MyShow Int where
  myShow 0 = "Zero"
  myShow 1 = "One"
  myShow 2 = "Two"
  myShow 3 = "Three"
  myShow 4 = "Four"
  myShow 5 = "Five"
  myShow _ = "More than five"
  
test_string_instance :: String
test_string_instance = myShow "test test test"

test_bool_instance :: String
test_bool_instance = myShow true

test_int_instance :: String
test_int_instance = myShow 10