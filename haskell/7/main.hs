isPrime :: Integer -> Bool
isPrime n =
  let fmax = floor $ sqrt $ fromIntegral n
      fs = [2..fmax]
  in not $ any (==0) $ map (n `mod`) fs

main = putStrLn $ show $ head $ drop 10000 $ filter isPrime [2..]

