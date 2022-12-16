import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (head args)
  let total = sum (map scorePt2 (lines content))
  putStrLn ("total score: " ++ show total)

score :: String -> Int
score "A X" = 1 + 3
score "A Y" = 2 + 6
score "A Z" = 3 + 0
score "B X" = 1 + 0
score "B Y" = 2 + 3
score "B Z" = 3 + 6
score "C X" = 1 + 6
score "C Y" = 2 + 0
score "C Z" = 3 + 3
score _ = 0

scorePt2 :: String -> Int
scorePt2 "A X" = 3 + 0
scorePt2 "A Y" = 1 + 3
scorePt2 "A Z" = 2 + 6
scorePt2 "B X" = 1 + 0
scorePt2 "B Y" = 2 + 3
scorePt2 "B Z" = 3 + 6
scorePt2 "C X" = 2 + 0
scorePt2 "C Y" = 3 + 3
scorePt2 "C Z" = 1 + 6
scorePt2 _ = 0