import Data.Char (isUpper)
import System.Environment (getArgs)

main1 :: IO ()
main1 = do
  args <- getArgs
  content <- readFile (head args)
  let commons = map (commonChar . half) (lines content)
  let total = sum $ map priority commons
  print total

priority :: Char -> Int
priority c
  | isUpper c = fromEnum c - 38 -- start at 27
  | otherwise = fromEnum c - 96 -- start at 1

commonChar :: (String, String) -> Char
commonChar (a, b) = head [c | c <- a, c `elem` b]

half :: String -> (String, String)
half s = splitAt (length s `div` 2) s

-- part 2

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (head args)
  let badges = map (\(one, two, three) -> badge one two three) (elfGroups $ lines content)
  let total = sum $ map priority badges
  print total

elfGroups :: [String] -> [(String, String, String)]
elfGroups [] = []
elfGroups (a : b : c : rest) = (a, b, c) : elfGroups rest
elfGroups _ = [] -- won't happen

-- find the common char across the three strings
badge :: String -> String -> String -> Char
badge a b c = head [x | x <- a, x `elem` b, x `elem` c]
