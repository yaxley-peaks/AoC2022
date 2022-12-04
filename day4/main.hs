import Data.List
import Data.List.Split (splitOn)

processStr :: String -> [(String, String)]
processStr str =
  let slines = lines str
      splits = map (splitOn ",") slines
   in map (\l -> (head l, last l)) splits

toLists :: [(String, String)] -> [([Int], [Int])]
toLists =
  map
    ( \(x, y) ->
        let xspl = splitOn "-" x
            yspl = splitOn "-" y
            xspl_s = read $ head xspl
            xspl_e = read $ last xspl
            yspl_s = read $ head yspl
            yspl_e = read $ last yspl
         in ([xspl_s .. xspl_e], [yspl_s .. yspl_e])
    )

overlaps :: Eq a => [a] -> [a] -> Bool
overlaps x y = not (null (x `intersect` y))

subList :: Eq a => [a] -> [a] -> Bool
subList x y = intersect x y == x || intersect x y == y

p1 :: String -> Int
p1 ys = sum (map (\(x, y) -> if x `subList` y then 1 else 0) xs)
  where
    xs = toLists $ processStr ys

p2 :: String -> Int
p2 ys = sum (map (\(x, y) -> if x `overlaps` y then 1 else 0) xs)
  where
    xs = toLists $ processStr ys

main :: IO ()
main = do
  contents <- readFile "./inp.txt"
  print $ p1 contents
  print $ p2 contents