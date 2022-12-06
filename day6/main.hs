import Data.List (findIndex, nub, tails)
import Data.List.Split (divvy)
import Data.Maybe

windows' :: Int -> [a] -> [[a]]
windows' n xs = map (take n) $ tails xs

windows :: Int -> [a] -> [[a]]
windows n xs = take (length xs - n + 1) (windows' n xs)

solve' n xs = fmap (+ 1) $ findIndex (\x -> nub x == x) $ divvy n 1 xs

solve :: Int -> String -> Int
solve n xs =
  1 + fromJust (findIndex (\x -> nub x == x) $ windows n xs)

main :: IO ()
main = do
  contents <- readFile "./inp.txt"
  print $ solve' 4 contents
  print $ solve 14 contents
