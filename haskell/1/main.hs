triNum n = n * (n + 1) `div` 2

main :: IO ()
main = putStrLn $ show $
  --sum $ filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0) [1..999]
  factorSum 999 3 + factorSum 999 5 - factorSum 999 15
  where factorSum n f = f * triNum (n `div` f)

