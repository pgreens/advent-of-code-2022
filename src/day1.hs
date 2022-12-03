import Data.List
import Data.Ord (Down(Down))
import System.Environment

-- part 1

mainPt1 :: IO ()
mainPt1 = do
  args <- getArgs
  content <- readFile (head args)
  let (calories, elf) = fancyFold (lines content) (0,0) (0,1)
  putStrLn ("max calories: " ++ show calories ++ ", elf number: " ++ show elf)

fancyFold :: [String] -> (Int, Int) -> (Int, Int) -> (Int, Int)
fancyFold []         max             curr                   = max
fancyFold ("":rest)  max@(maxCal, _) curr@(currCal, currElf)
                                        | currCal > maxCal  = fancyFold rest curr (0, currElf + 1)
                                        | otherwise         = fancyFold rest max (0, currElf + 1)
fancyFold (num:rest) max            curr@(currCal, currElf) = fancyFold rest max (currCal + (read num :: Int), currElf)

-- part 2

type Entry = (Int, Int)

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (head args)
  let elves = sumPerElf (lines content) (0,1)
  let [one, two, three] = take 3 (sortOn Down elves)
  putStrLn (show one ++ ", " ++ show two ++ ", " ++ show three)
  putStrLn ("total: " ++ show (fst one + fst two + fst three))

sumPerElf :: [String] -> Entry -> [Entry]
sumPerElf []         curr               = [curr]
sumPerElf ("":rest)  curr@(_, elfNum)   = curr : sumPerElf rest (0, elfNum + 1)
sumPerElf (num:rest) (calories, elfNum) = sumPerElf rest (calories + (read num :: Int), elfNum)
