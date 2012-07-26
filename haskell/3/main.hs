-- dropFactor will divide a factor out of a number as many times as possible.
-- Returns the largest divisor of `n` that is not divisible by `f`.
dropFactor :: Integer -> Integer -> Integer
dropFactor n f =
  let (q,r) = n `divMod` f
  in if (r == 0) then dropFactor q f else n

-- Start the wedge with f = 2.
wedge :: Integer -> Integer -> [(Integer, Integer)]
wedge n f = 
  let n' = dropFactor n f
  in (n',f) : wedge n' (f+1)

gpf n =
  let ws = wedge n 2
      ws' = dropWhile (\(n,f) -> f < n) ws
  in snd $ head $ ws'

main = putStrLn $ show $ gpf 600851475143

