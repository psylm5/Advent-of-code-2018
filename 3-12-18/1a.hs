import Data.Maybe
type Claim = (Int,Int,Int,Int,Int)

main = do
  input <- (fmap lines $ readFile "input.txt")
  let claims = map parse input
  let classified = oneA claims fabric
  let area = sum $ (map (length.catMaybes) classified)
  print ((1000*1000) - area)
  return $ oneB claims classified

fabric :: [[Maybe Int]]
fabric = take 1000 (repeat $ take 1000 (repeat (Just 0)))

oneB :: [Claim] -> [[Maybe Int]] -> Maybe Int
oneB [] _ = Nothing
oneB ((p@(c,l,r,w,h)):ps) fabric
  |(w*h) == (sum $ map (length.(filter (==c)).catMaybes) fabric) = Just c
  | otherwise = oneB ps fabric

oneA :: [Claim] -> [[Maybe Int]] -> [[Maybe Int]]
oneA [] fabric = fabric
oneA (p:ps) fabric = oneA ps (classifyColumn fabric p)


parse :: String -> Claim
parse xs = (read claim::Int, read left::Int, read right::Int, read width::Int, read height::Int)
  where
    (claim, rest1) = span (/='@') (tail xs)
    (left, rest2) = span (/=',') (tail $ tail rest1)
    (right, rest3) = span (/=':') (tail rest2)
    (width, _:height) = span (/='x') (tail $ tail rest3)

classifyColumn :: [[Maybe Int]] -> Claim -> [[Maybe Int]]
classifyColumn fabric (_,_,_,_,0) = fabric
classifyColumn fabric claim@(c,l,t,w,h) = classifyColumn newfabric (c,l,(t+1),w,(h-1))
  where
    newfabric = (take t fabric) ++ [(classifyRow (fabric !! (t)) claim)] ++ (drop (t+1) fabric)

classifyRow :: [Maybe Int] -> Claim -> [Maybe Int]
classifyRow fabric (c,l,t,w,h) = left ++ (map (classify c) middle) ++ right
  where
    left = take l fabric
    right = drop (l + w) fabric
    middle = take w (drop l fabric)

classify :: Int -> Maybe Int -> Maybe Int
classify c mf = case mf of
  Nothing -> Nothing
  Just 0  -> Just c
  Just _  -> Nothing
