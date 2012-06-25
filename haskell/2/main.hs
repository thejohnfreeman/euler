fibs :: [Integer]
fibs = 0 : scanl (+) 1 fibs

main = let
  threshold = 4000000
  as = dropWhile (< threshold) fibs
  bs = dropWhile (not . even) as
  e = head bs -- next even
  cs = drop 2 bs
  s = (head cs) - 1 -- sum up to next even
  in putStrLn $ show $ (s `div` 2) - e

