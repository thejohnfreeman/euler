import Char (isDigit, digitToInt)
import Data.List (foldl1')

prompt :: IO String
prompt = do
  putStrLn "Paste the number; press Ctrl+D when finished:"
  n <- getContents
  return $ filter isDigit n

consecutiveGroupsOf :: Int -> [a] -> [[a]]
consecutiveGroupsOf n xs = take n xs : consecutiveGroupsOf n (tail xs)

consecutiveGroupsOf' :: Int -> [a] -> [[a]]
consecutiveGroupsOf' n xs = take (length xs - n + 1) $ consecutiveGroupsOf n xs

internalProducts :: Int -> [Char] -> [Integer]
internalProducts n cs =
  let toIntegers = map $ fromIntegral . digitToInt
      ds = consecutiveGroupsOf' n $ toIntegers cs
      multiply = foldl1' (*)
  in map multiply ds

main = do
  n <- prompt
  putStrLn $ ("["++) . shows (length n) . (" characters] "++) $ n
  putStrLn $ show $ maximum $ internalProducts 5 n

