module Main where

import System.Random (randomRIO)
--import Lib


lowercaseChars = ['a'..'z']
uppercaseChars = ['A'..'Z']

type CharSet = [Char]

randomChar :: CharSet -> IO Char
randomChar xs = do
  i <- randomRIO (0 :: Int, (length xs) - 1)
  return $ xs !! i


f :: [IO Char]
f = take 2 $ repeat $ randomChar lowercaseChars


-- sequence takes a list of actions of type `m a` -- [IO Char] in this case
-- and re-wraps them so that the IO is OUTSIDE the list and forms a big container
-- rather than a lot of small containers
main :: IO ()
main = do
  y <- randomChar uppercaseChars
  let y = sequence (take 2 $ repeat $ randomChar lowercaseChars)
  y >>= putStrLn
  return ()
