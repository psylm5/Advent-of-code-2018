import System.IO

getString :: IO [String]
getString = do
  handle <- openFile "input-1a.txt" ReadMode
  contents <- hGetContents handle
  return (lines contents)

main :: IO Int
main = do
  str <- getString
  let ints = foldr ((+).parse) 0 str
  return $ ints

parse :: String -> Int
parse x@('-':xs) = (read x)
parse ('+':xs) = (read xs)
