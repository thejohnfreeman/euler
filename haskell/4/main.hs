is x = let s = show x in s == reverse s

largest n =
  let scan valid start level = dropWhile (not . is) $ takeWhile valid $ map (level*) [start, start-1..level]
      levels = [n, n-1..1]
      first = head $ concat $ flip map levels $ scan (const True) n
  in maximum $ concat $ flip map levels $ scan (>=first) n

answer d =
  let n = 10^d - 1
  in largest n

main = putStrLn $ show $ answer 3

