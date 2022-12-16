module Main where

import Data.List (stripPrefix, foldl', transpose)
import System.Environment (getArgs)

type Range = (Int, Int)

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (head args)
  let (cratesInput, movesInput) = span (/="") (lines content)
  let crates = removeEmpties $ transpose $ map parseCrates (init cratesInput)
  let moves = map parseMove (tail movesInput)
  let final = execMoves crates moves
  print (map head final)

data Crate = Full Char | Empty
  deriving (Show, Eq)

isFull :: Crate -> Bool
isFull (Full _) = True
isFull Empty = False

parseCrates :: String -> [Crate]
parseCrates [] = []
parseCrates (' ':'[':rest) = parseCrates ('[':rest)
parseCrates ('[':c:']':rest) = Full c : parseCrates rest
parseCrates (' ':' ':' ':' ':rest) = Empty : parseCrates rest
parseCrates _ = []

removeEmpties :: [[Crate]] -> [[Crate]]
removeEmpties = map (filter isFull)

-- moves

type Move = (Int, Int, Int)

parseMove :: String -> Move
parseMove line = (num, fromStack - 1, toStack - 1)
  where (_:numStr:_:fromStackStr:_:toStackStr:_) = parts line
        num = read numStr :: Int
        fromStack = read fromStackStr :: Int
        toStack = read toStackStr :: Int

parts :: String -> [String]
parts [] = []
parts (' ':rest) = parts rest
parts s = part : parts rest
  where (part, rest) = break (==' ') s


execMoves :: [[Crate]] -> [Move] -> [[Crate]]
execMoves = foldl execMove

execMove :: [[Crate]] -> Move -> [[Crate]]
execMove stacks move = zipWith (\stackNum stack -> mapStack stackNum stack move crates) [0..] stacks
        where crates = items stacks move

items :: [[Crate]] -> Move -> [Crate]
items stacks (num,from,_) = take num (stacks !! from)

mapStack :: Int -> [Crate] -> Move -> [Crate] -> [Crate]
mapStack stackNum stack (num, from,to) crates
  | stackNum == from = drop num stack
  | stackNum == to   = crates ++ stack
  | otherwise        = stack