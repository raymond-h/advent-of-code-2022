module Lib where

addNumbers :: Num n => n -> n -> n
addNumbers a b = a + b

doThing :: IO ()
doThing = do
  putStrLn "hello world"
  print $ addNumbers (5 :: Integer) 4
