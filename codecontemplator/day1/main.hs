pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

solve :: [Int] -> Int
solve  = length . filter id . map (uncurry (<)) . pairs

triples :: [a] -> [[a]]
triples xs = zipWith sum
main = do
  fileContext <- readFile "data.txt"
  let xs = map read (lines fileContext) :: [Int]
  let solution = solve  xs
  putStrLn $ "solution " ++ show solution
