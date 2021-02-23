module Main where

f_id :: forall a. a -> a
f_id a = a

f_fst :: forall a b. a -> b -> a
f_fst a b = a

f_snd :: forall a b. a -> b -> b
f_snd a b = b

alias_f_snd :: forall a b. a -> b -> b
alias_f_snd a b = f_snd a b

i_value :: Int
i_value = 5

f_value :: Number
f_value = 10.0

s_value :: String
s_value = "test"