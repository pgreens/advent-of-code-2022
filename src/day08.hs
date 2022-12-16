{-# LANGUAGE RankNTypes #-}
import System.Environment (getArgs)
import Data.List

main1 :: IO ()
main1 = do
  args <- getArgs
  content <- readFile (head args)
  let trees = parseTrees content
  let total = nub (lookAtTrees trees 
                ++ lookAtTrees (fromBottom trees) 
                ++ lookAtTrees (fromRight trees) 
                ++ lookAtTrees (fromTop trees))
  print $ length total

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (head args)
  let trees = parseTrees content
  print $ maximum (map maximum (scores trees))

type Point = (Int, Int)
newtype Tre = T (Int, (Int, Int))
  deriving Show

instance Ord Tre where
  compare (T (h1, _)) (T (h2, _)) = compare h1 h2
instance Eq Tre where
  (T (h1, _)) == (T (h2, _)) = h1 == h2

-- get a grid of (height, (x, y)) elements
parseTrees :: String -> [[Tre]]
parseTrees s = 
  zipWith (\ row y -> zipWith (
      \ height x -> T (height, (x, y))
    ) row [0..] ) rows [0..]
  where
    rows = map (map readInt) (lines s)
    readInt s = read [s] :: Int

pos :: Tre -> Point
pos (T (_, p)) = p

outer :: Tre
outer = T (-1, (-1,-1))

lookAtTrees :: [[Tre]] -> [Point]
lookAtTrees = filter (/= (-1,-1)) . map pos . concatMap findVis
  where
    findVis :: [Tre] -> [Tre]
    findVis = foldl' (\maxes tree -> if tree > head maxes then tree : maxes else maxes) [outer]

fromBottom :: [[a]] -> [[a]]
fromBottom trees = map reverse (transpose trees)

fromBottom' :: [[a]] -> [[a]]
fromBottom' trees = transpose (map reverse trees)

fromRight :: [[a]] -> [[a]]
fromRight = map reverse

fromTop :: [[a]] -> [[a]]
fromTop = transpose

--- part 2

-- count the sitelines for each tree, going left to right
siteLines :: [[Tre]] -> [[Int]]
siteLines = map siteLine
  where
    siteLine :: [Tre] -> [Int]
    siteLine row = zipWith (\t rest -> length $ takeWhilePlusLast (t>) rest) row (tail $ tails row)

-- want to count the first tree you can't see past
takeWhilePlusLast :: forall a. (a -> Bool) -> [a] -> [a]
takeWhilePlusLast fn [] = []
takeWhilePlusLast fn (a:as) = if fn a then a : takeWhilePlusLast fn as else [a]

scores :: [[Tre]] -> [[Int]]
scores trees =
  -- a little uglier than I had hoped
  siteLines trees `times`
  fromTop (siteLines (fromTop trees)) `times`
  fromRight (siteLines (fromRight trees)) `times`
  fromBottom' (siteLines (fromBottom trees))
  where
    -- multiply each matrix element with the number in the same position of a second matrix
    times :: [[Int]] -> [[Int]] -> [[Int]]
    times = zipWith (zipWith (*))