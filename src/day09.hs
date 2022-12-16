import System.Environment (getArgs)
import Data.List

main1 :: IO ()
main1 = do
  args <- getArgs
  content <- readFile (head args)
  let moves = map parseMove (lines content)
  let coverage = nub $ map snd $ run (0,0) (0,0) moves
  print $ length coverage

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (head args)
  let moves = map parseMove (lines content)
  -- simplify the moves by splitting them into 1-move increments
  let moves2 = concatMap (\(dir, num) -> replicate num (dir, 1) ) moves
  let history = move2 (replicate 10 (0,0)) moves2
  let coverage = nub $ map last history
  print $ length coverage

type Point = (Int, Int)
type Move = (Char, Int)

run :: Point -> Point -> [Move] -> [(Point, Point)]
run head tail = foldl' (\hist@((head', tail'):rest) m -> reverse (move head' tail' m) ++ hist) [(head, tail)]

move :: Point -> Point -> Move -> [(Point, Point)]
move head tail (dir, num) = scanl' (\(head', tail') ht -> (transform head' headTrans, transform tail' $ tailTransform (relative (transform head' headTrans) tail'))) (head, tail) (replicate num headTrans)
  where headTrans = fromDirection dir

--- part 2

move2 :: [Point] -> [Move] -> [[Point]]
move2 = scanl' moveRope 

moveRope :: [Point] -> Move -> [Point]
moveRope knots (dir,_) = scanl' follow first (tail knots)
  where 
    first = transform (head knots) (fromDirection dir)

-- move tail based on head's position
follow :: Point -> Point -> Point
follow movedHead tail = transform tail tailMove
  where
    tailMove = tailTransform (relative movedHead tail)

transform :: Point -> Point -> Point
transform (x, y) (tx, ty) = (x + tx, y + ty)

-- get the distance between two points
relative :: Point -> Point -> Point
relative (hx, hy) (tx, ty) = (tx-hx, ty-hy)

parseMove :: String -> Move
parseMove (dir:' ':num) = (dir, read num :: Int)
parseMove _ = error "Invalid move"

fromDirection :: Char -> Point
fromDirection 'U' = ( 0,-1)
fromDirection 'R' = ( 1, 0)
fromDirection 'D' = ( 0, 1)
fromDirection 'L' = (-1, 0)
fromDirection _ = error "Invalid direction"

tailTransform :: Point -> Point
tailTransform rel@(relX, relY)
  | abs relX <= 1 && abs relY <= 1 = (0, 0)
  | otherwise = (-signum relX, -signum relY) -- diagonal
  