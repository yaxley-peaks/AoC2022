import System.IO
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Data.Text (splitOn, pack, unpack) 

type Stack = [Char]
type Move = (Integer, Integer, Integer)

replace :: Integral i => i -> a -> [a] -> [a]
replace pos newVal list = take (fromIntegral pos) list ++ newVal : drop (fromIntegral pos + 1) list

getTops :: [Stack] -> String
getTops = map head

-- applyMove stacks (size, from, to) = 
--     replace (from - 1) 
--         (reverse $ take (fromIntegral $ size - from) $ reverse $ stacks !! (fromIntegral from - 1)) 
--     $ replace (to - 1) (reverse $ take (fromIntegral size) (stacks !! (fromIntegral from - 1))) stacks 
applyMove :: [Stack] -> Move -> [Stack]
applyMove stacks (0, from, to) = stacks
applyMove stacks (size, from, to) = 
    applyMove (replace from (tail $ stacks !! fromInteger from)
    $ replace to (head (stacks !! fromInteger from):(stacks !! fromInteger to)) stacks)
    (size - toInteger 1, from, to)

applyMove2 :: [Stack] -> Move -> [Stack]
applyMove2 stacks (0, from, to) = stacks
applyMove2 stacks (size, from, to) = 
    applyMove2 (replace from (
    take (fromIntegral size - 1) (stacks !! fromInteger from)
    ++ drop (fromIntegral size) (stacks !! fromInteger from))
    $ replace to (((stacks !! fromInteger from) !! (fromIntegral size - 1)):(stacks !! fromInteger to)) stacks)
    (size - toInteger 1, from, to)

solve :: ([Stack] -> Move -> [Stack]) ->  [Stack] -> [Move] -> String
solve f stacks = getTops . foldl f stacks

parseCrates :: [String] -> [Stack]
parseCrates lines = 
    if length (head lines) >= 4 then
        filter (/= ' ') (map (!! 1) lines):parseCrates (map (tail . tail . tail . tail) lines)
    else
        [filter (/= ' ') (map (!! 1) lines)]

parseMove :: String -> Move
parseMove str = let 
        parts = map unpack $ splitOn (pack " ") (pack str)
    in
        (read (parts !! 1) :: Integer, (read (parts !! 3) :: Integer) - toInteger 1, (read (parts !! 5) :: Integer) - toInteger 1)

main = do
    handle <- openFile "./inp.txt" ReadMode
    content <- hGetContents handle
    
    let 
        crates = takeWhile (\xs -> (head xs /= ' ') || ((xs !! 1) /= '1')) $ lines content
        stacks = parseCrates crates
        movesLines = lines $ reverse $ take (length content - fromMaybe 0 (elemIndex 'm' content)) $ reverse content
        moves = map parseMove movesLines
        in do
            print $ solve applyMove stacks moves
            print $ solve applyMove2 stacks moves

    hClose handle