import System.Environment ( getArgs )
import Data.List ( intercalate, unfoldr, uncons )

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (head args)
  let cmds = lines content
  print $ signalStrength cmds

signalStrength :: [String] -> Int
signalStrength cmds = twenty + sum (pluck 40 rest)
  where
    regValues = cycles 1 cmds
    signals =  zipWith (*) regValues [1..]
    twenty = signals !! 19
    rest = drop 20 signals

-- take every nth element
pluck :: Int -> [a] -> [a]
pluck n = unfoldr (uncons . drop (n-1))

cycles :: Int -> [String] -> [Int]
cycles x [] = [x]
cycles x (cmd:rest) =
  case cmd of
    "noop" -> x : cycles x rest
    addx -> let val = read (words addx !! 1) :: Int
            in x : x : cycles (x + val) rest

-- part 2

main2 :: IO ()
main2 = do
  args <- getArgs
  content <- readFile (head args)
  let cmds = lines content
  putStrLn $ intercalate "\n" $ chunkBy 40 (pixels cmds)

chunkBy :: Int -> String -> [String]
chunkBy n [] = []
chunkBy n s = take n s : chunkBy n (drop n s)

pixels :: [String] -> String
pixels cmds = zipWith drawPixel crtPos (cycles 1 cmds)
  where
    crtPos = cycle [0..39]
    drawPixel pixel x = if pixel >= x - 1 && pixel <= x + 1 then '#' else '.'