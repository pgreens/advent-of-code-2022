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

-- didn't need to make this type, but I don't feel like unraveling it now
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
execMoves stacks [] = stacks
execMoves stacks ((0,_,_):rest) = execMoves stacks rest
execMoves stacks (move@(num,from,to):rest) = 
  execMoves (singleMove stacks move) ((num-1,from,to):rest)

singleMove :: [[Crate]] -> Move -> [[Crate]]
singleMove stacks move = zipWith (\stackNum stack -> mapStack stackNum stack move crate) [0..] stacks
        where crate = item stacks move

item :: [[Crate]] -> Move -> Crate
item stacks (_,from,_) = head (stacks !! from)

mapStack :: Int -> [Crate] -> Move -> Crate -> [Crate]
mapStack stackNum stack (_,from,to) crate
  | stackNum == from = drop 1 stack
  | stackNum == to   = crate : stack
  | otherwise        = stack