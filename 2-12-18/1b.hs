import Data.List
import Data.List.Split

main = do
  input <- (fmap lines $ readFile "input.txt")
  let x = correct input
  let y = map (checkUnique.unique.sort) x
  return $ y 

correct :: [String] -> [[String]]
correct input = chunksOf 250 [inputN | n <- [1..length (head input)],inputN <- map (splitList n) input]

splitList ::  Int -> String -> String
splitList n s = (take (n-1)) s ++ (drop n s)

unique :: [String] -> [String]
unique s = [a | a <- s, b <- s, a == b]

checkUnique :: [String] -> String
checkUnique [] = []
--checkUnique (_:[]) = []
checkUnique (a:b:xs)
    | a == b = a
    | otherwise = checkUnique (b:xs)
checkUnique (a:[]) = []
