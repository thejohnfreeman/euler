s = 1000

as = [1..s `div` 2]
bs = concat [ map (a,) [a..s - 1 - a] | a <- as ]
cs = map (\(a,b) -> (a,b,s - a - b)) bs

isPyTriplet (a,b,c) = a^2 + b^2 == c^2

answer = head $ flip dropWhile cs $ not . isPyTriplet
answer' = let (a,b,c) = answer in a*b*c

main = putStrLn $ show $ answer'

