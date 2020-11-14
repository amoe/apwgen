module Main where

import System.Random (randomRIO)
--import Lib


lowercaseChars = ['a'..'z']

randomChar :: IO Char
randomChar = do
  i <- randomRIO (0 :: Int, (length lowercaseChars) - 1)
  return $ lowercaseChars !! i

f :: [IO Char]
f = take 2 $ repeat randomChar


-- sequence takes a list of actions of type `m a` -- [IO Char] in this case
-- and re-wraps them so that the IO is OUTSIDE the list and forms a big container
-- rather than a lot of small containers
main :: IO ()
main = do
  x <- randomChar
  let y = sequence (take 2 $ repeat randomChar)
  y >>= putStrLn
  return ()
