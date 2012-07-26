sumOfSquares n = n * (n + 1) * (2*n + 1) `div` 6

squareOfSums n = (n * (n + 1) `div` 2) ^ 2

main = let n = 100
  in putStrLn $ show $ squareOfSums n - sumOfSquares n

