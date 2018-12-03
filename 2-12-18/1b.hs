import Data.List
import Data.List.Split

main = do
  input <- (fmap lines $ readFile "input.txt")
  let x = correct input
  let y = map (checkUnique.sort) x
  return $ y

correct :: [String] -> [[String]]
correct input = chunksOf 250 [inputN | n <- [1..length (head input)],inputN <- map (splitList n) input]

splitList ::  Int -> String -> String
splitList n s = (take (n-1)) s ++ (drop n s)

checkUnique :: [String] -> String
checkUnique [] = []
checkUnique (a:b:xs)
    | a == b = a
    | otherwise = checkUnique (b:xs)
checkUnique (a:[]) = []
