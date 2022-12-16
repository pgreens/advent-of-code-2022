module Main where

import Data.List (stripPrefix, foldl')
import System.Environment (getArgs)

type Range = (Int, Int)

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (head args)
  let ranges = map parseLine (lines content)
  let fullOverlaps = foldl' (\count (r1, r2) -> count + isOverlapping r1 r2) 0 ranges
  print fullOverlaps

parseLine :: String -> (Range, Range)
parseLine line = (range left, range right)
  where
    (left, ',' : right) = break (== ',') line

range :: String -> Range
range s = (read left :: Int, read right :: Int)
  where
    (left, '-' : right) = break (== '-') s

isOverlapping :: Range -> Range -> Int
isOverlapping r1@(min1, max1) r2@(min2, max2)
  | min1 < min2 && max1 < min2 = 0
  | min2 < min1 && max2 < min1 = 0
  | otherwise = 1
