module Main where

import Data.List (stripPrefix, foldl', transpose, uncons, elemIndices, elemIndex, find)
import System.Environment (getArgs)
import Data.Maybe (isJust, isNothing)

type Range = (Int, Int)

main1 :: IO ()
main1 = do
  args <- getArgs
  content <- readFile (head args)
  print (signalStart 4 content 0)

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (head args)
  print (signalStart 14 content 0)

signalStart :: Int -> String -> Int -> Int
signalStart num s i = if unique num s then i+num else signalStart num (tail s) (i+1)

unique :: Int -> String -> Bool
unique num s = isNothing $ find (\c -> length (elemIndices c segment) > 1) segment
  where segment = take num s