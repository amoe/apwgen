module Main where

import System.Random (randomRIO)
import qualified Data.Map as M
--import Dictionary (dictionaryWords)
--import Lib

leftHandAlpha = "qwertasdfgzxcvb"
rightHandAlpha = "yuiophjklnm"

leftHandNumeric = "12345"
rightHandNumeric = "67890"

lowercaseChars = ['a'..'z']
uppercaseChars = ['A'..'Z']

-- Hopefully this is all that's required because we won't include multiple
-- special characters in passwords  

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
  "condemned",
  "cost",
  "lamented"
  ]


testSet :: [Integer]
testSet = [1, 2, 2, 3]

--trigrams "midge" => ["mid", "idg", "dge"]
--trigrams "idge" => ["idg", "dge"]  
trigrams :: String -> [String]
trigrams [] = []
trigrams [_] = []
trigrams [_, _] = []
trigrams s = (take 3 s) : trigrams (tail s)


f' d = concatMap trigrams d

  
-- Map keys always need Ord
genHistogram :: Ord k => [k] -> M.Map k Integer
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
-- first arg should be the random int that was picked that must be in range of sum all weights. 0 - 7 if weight sum is 8
pickWeighted :: Integer -> [(a, Integer)] -> Maybe a
pickWeighted n xs = go n xs 0
  where go _ [] _ = Nothing
        go n (x:xs) i = if n >= i && n < endpoint
                        then Just $ fst x
                        else go n xs endpoint
          where endpoint = i + (snd x)


-- We can calculate this, but we don't actually need to, because the sum of the
-- weights is always the same as the total length of the input trigram list.
weightSum :: [(String, Integer)] -> Integer
weightSum xs = sum $ map snd xs



g d = pickWeighted 0 (M.toList h)
  where h = genHistogram $ concatMap trigrams d

allWords :: IO [String]
allWords = do
  dict <- readFile "/usr/share/dict/words"
  return $ lines dict


fooMain :: IO ()
fooMain = do
  putStrLn "Starting."
  words <- allWords
  putStrLn ("Read " ++ (show (length words)) ++ " words.")
  let t = concatMap trigrams words
  let nTrigrams = length t
  putStrLn $ "Calculated " ++ show nTrigrams ++ " trigrams."
  let y = genHistogram t
  putStrLn $ "Histogram size was: " ++ show (M.size y)
  r <- randomRIO (0, nTrigrams - 1)
  putStrLn $ "Picked number: " ++ show r

  let picked = pickWeighted (fromIntegral r) (M.toList y)

  case picked of
    (Just x) -> putStrLn $ "Picked trigram: " ++ x
    Nothing -> error "No!"
  
  putStrLn "End."
  
