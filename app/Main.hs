module Main where

import System.Random (randomRIO)
--import Lib


lowercaseChars = ['a'..'z']

main :: IO ()
main = do
  i <- randomRIO (0 :: Int, (length lowercaseChars) - 1)
  let c = lowercaseChars !! i
  putStrLn $ "Hello, world: " ++ (show c)
  return ()
