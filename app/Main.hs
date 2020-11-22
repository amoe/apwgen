module Main where

import System.Random (randomRIO)
import qualified Data.Map as M
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
pwMain :: IO ()
pwMain = do
  y <- randomChar uppercaseChars
  let y = sequence (take 2 $ repeat $ randomChar lowercaseChars)
  y >>= putStrLn
  return ()


miniDictionary :: [String]
miniDictionary = [
  "yaw",
  "midge",
  "constraint",
  "cost",
  "lamented"
  ]


testSet :: [Integer]
testSet = [1, 2, 2, 3]


genHistogram :: [Integer] -> M.Map Integer Integer
genHistogram [] = M.empty
genHistogram (x:xs) = M.insertWith inc x 1 (genHistogram xs)
  where inc _ oldValue = (+1) oldValue

main :: IO ()
main = do
  putStrLn "foo"

frequencies :: [(String, Integer)]
frequencies = [
    ("fry", 1),
    ("bender", 5),
    ("leela", 2)
  ]

-- final arg is the accumulator -- must be supplied by user to init to 0
pickWeighted :: Integer -> [(String, Integer)] -> Integer -> Maybe String
pickWeighted _ [] _ = Nothing
pickWeighted n (x:xs) i = if n >= i && n < endpoint
                          then Just $ fst x
                          else pickWeighted n xs endpoint
  where endpoint = i + (snd x)
  
