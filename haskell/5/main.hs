import Data.List (foldl1')
import Prelude hiding (lcm)

-- newFactor takes a list of factors and a number. It divides each factor
-- into the number, slowly whittling away at it. What's returned in the end
-- is either 1 or a new factor.
newFactor :: [Integer] -> Integer -> Integer
newFactor [] n = n
newFactor (f:fs) n =
  let (q,r) = n `divMod` f
  in newFactor fs $ if (r == 0) then q else n

factors :: Integer -> [Integer]
factors 1 = [1]
factors n =
  let fs = factors $ n - 1
      f  = newFactor fs n
  in if (f == 1) then fs else (f:fs)

lcm n = foldl1' (*) $ factors n

main = putStrLn $ show $ lcm 20

