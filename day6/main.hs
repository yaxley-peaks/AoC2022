import Data.List ( nub, tails )


windows' :: Int -> [a] -> [[a]]
windows' n xs = map (take n) $ tails xs

windows :: Int -> [a] -> [[a]]
windows n xs = take (length xs - n + 1) (windows' n xs)

solve :: Int -> String -> Int
solve n xs =
  (+) n $
    length $
      takeWhile
        (== 1)
        (map (\ls -> if length (nub ls) == length ls then 0 else 1) $ windows n xs)

main :: IO ()
main = do
  contents <- readFile "./inp.txt"
  print $ solve 4 contents
  print $ solve 14 contents
