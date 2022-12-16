module Main where

import Data.Bits
import Data.Char
import System.Environment (getArgs)
import Data.List (foldl')
import Data.Bifunctor (bimap)

main1 :: IO ()
main1 = do
  args <- getArgs
  content <- readFile (head args)
  let halves = map (bimap bitSet bitSet . half) (lines content)
  let commons = map (uncurry (.&.)) halves
  let total = sum $ map bitToPriority commons
  print total

half :: String -> (String, String)
half s = splitAt (length s `div` 2) s

priority :: Char -> Int
priority c
  | isUpper c = fromEnum c - 38 -- A-Z are 27-53
  | otherwise = fromEnum c - 96 -- a-z are 1-26

-- turn a string like "abd" to binary like 1011
-- where each item type is represented by a 1 bit
-- in a certain position 
bitSet :: String -> Int
bitSet s = foldl' (\bits priority -> bits .|. (2 ^ (priority-1)) :: Int) 0 (map priority s)

bitToPriority :: Int -> Int
bitToPriority 0 = 0
bitToPriority 1 = 1
bitToPriority x = 1 + bitToPriority (shift x (-1))

--- part 2

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (head args)
  let badges = map (\(one, two, three) -> badge one two three) (elfGroups $ lines content)
  let total = sum badges
  print total

badge :: String -> String -> String -> Int
badge one two three = bitToPriority $ bitSet one .&. bitSet two .&. bitSet three

elfGroups :: [String] -> [(String, String, String)]
elfGroups [] = []
elfGroups (a : b : c : rest) = (a, b, c) : elfGroups rest
elfGroups _ = [] -- won't happen