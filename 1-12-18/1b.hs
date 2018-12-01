import System.IO

getString :: IO [String]
getString = do
  handle <- openFile "input-1a.txt" ReadMode
  contents <- hGetContents handle
  return (lines contents)

main :: IO Int
main = do
  str <- getString
  let ints = map parse str
  let ((x:_), _)= sumList (cycle ints, [0])
  return x

parse :: String -> Int
parse x@('-':xs) = (read x)
parse ('+':xs) = (read xs)

sumList :: ([Int],[Int]) ->  ([Int],[Int])
sumList ((a:xs),yss@(c:ys))
    | elem (a+c) yss = ([(a+c)], [])
    | otherwise = sumList (xs, (a+c):yss)
