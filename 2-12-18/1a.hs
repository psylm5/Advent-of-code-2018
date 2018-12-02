import Data.List

main :: IO Int
main = do
  input <- (fmap lines $ readFile "input.txt")
  let twosAndThrees = map (filter (\x -> x == 2 || x == 3)) $ map( map length) (map group $ (map sort input))
  let justTwosAndThrees = (map checkTwosAndThrees twosAndThrees)
  let twos = sum $ fst(unzip justTwosAndThrees)
  let threes = sum $ snd(unzip justTwosAndThrees)
  return $ twos * threes

checkTwosAndThrees :: [Int] -> (Int,Int)
checkTwosAndThrees xs
  |elem 2 xs && elem 3 xs = (1,1)
  |elem 2 xs              = (1,0)
  |elem 3 xs              = (0,1)
  |otherwise              = (0,0)
