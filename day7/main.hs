{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.List (elemIndex, sort)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Text (pack, splitOn, unpack)
import System.IO

--                      Name   Size                  Name    Children                   Parent
data FsObject = FsFile String Integer | FsDirectory String (Map.Map String FsPointer) FsPointer | FsEmpty deriving (Show)

data Command = Ls [FsObject] | Cd String

type FsMemory = [FsObject]

type FsPointer = Integer

replace :: (Eq a, Num a, Enum a) => a -> b -> [b] -> [b]
replace index elem = zipWith (\index' elem' -> (if index' == index then elem else elem')) [0 ..]

addChild :: FsMemory -> FsPointer -> String -> FsPointer -> FsMemory
addChild mem ptr childName child =
  let newMem = case mem !! fromInteger ptr of
        FsDirectory name children parent ->
          replace
            ptr
            (FsDirectory name (Map.insert childName child children) parent)
            mem
   in case mem !! fromInteger child of
        FsDirectory name children _ -> replace child (FsDirectory name children ptr) newMem
        FsFile _ _ -> newMem

addFsObject :: FsMemory -> FsObject -> (Integer, FsMemory)
addFsObject mem obj = (toInteger $ length mem, mem ++ [obj])

addLsObject :: FsPointer -> FsMemory -> FsObject -> FsMemory
addLsObject ptr mem obj@(FsFile name _) =
  let (child, newMem) = addFsObject mem obj
   in addChild newMem ptr name child
addLsObject ptr mem obj@(FsDirectory name _ _) =
  let (child, newMem) = addFsObject mem obj
   in addChild newMem ptr name child

getFs :: [Command] -> FsMemory -> FsPointer -> FsMemory
getFs [] mem ptr = mem
getFs (command : rest) mem ptr =
  case (command, mem !! fromInteger ptr) of
    (Cd "..", FsDirectory _ _ parent) -> getFs rest mem parent
    (Cd "/", FsEmpty) -> uncurry (flip $ getFs rest) $ addFsObject mem $ FsDirectory "/" Map.empty 0
    (Cd "/", FsDirectory {}) -> getFs rest mem 1
    (Cd dir, FsDirectory _ children _) ->
      if Map.member dir children
        then getFs rest mem $ fromJust $ Map.lookup dir children
        else
          let (newPtr, newMem) = addFsObject mem $ FsDirectory dir Map.empty 0
           in getFs rest (addChild newMem ptr dir newPtr) newPtr
    (Ls contents, FsDirectory _ children _) ->
      getFs rest (foldl (addLsObject ptr) mem contents) ptr

getDirectories :: FsMemory -> FsObject -> [FsObject]
getDirectories mem (FsFile _ _) = []
getDirectories mem dir@(FsDirectory _ children _) =
  dir : concatMap (getDirectories mem . (mem !!) . fromInteger . snd) (Map.toList children)

getFsSize :: FsMemory -> FsObject -> Integer
getFsSize _ (FsFile _ size) = size
getFsSize mem (FsDirectory _ children _) = sum $ map (getFsSize mem . (mem !!) . fromInteger . snd) (Map.toList children)

countResult :: FsMemory -> FsObject -> Integer
countResult mem = sum . takeWhile (<= 100000) . sort . map (getFsSize mem) . getDirectories mem

p1 :: [Command] -> Integer
p1 commands = let mem = getFs commands [FsEmpty] 0 in countResult mem (mem !! 1)

p2 :: [Command] -> Integer
p2 commands =
  let mem = getFs commands [FsEmpty] 0
   in minimum $ filter (>= (getFsSize mem (mem !! 1) - 40000000)) $ map (getFsSize mem) $ getDirectories mem (mem !! 1)

parseLsObject :: String -> FsObject
parseLsObject ('d' : 'i' : 'r' : ' ' : name) = FsDirectory name Map.empty 0
parseLsObject text =
  let (size, ' ' : name) = splitAt (fromJust $ ' ' `elemIndex` text) text
   in FsFile name (read size)

parseCommand :: String -> Command
parseCommand ('c' : 'd' : ' ' : arg) = Cd arg
parseCommand text@('l' : 's' : _) = Ls $ map parseLsObject $ tail $ lines text
parseCommand _ = error "Unreachable!"

main = do
  handle <- openFile "./inp.txt" ReadMode
  content <- hGetContents handle

  let commandsText = splitOn (pack "\n$ ") (pack $ tail $ tail content)
      commands = map (parseCommand . unpack) commandsText
   in do
        print $ p1 commands
        print $ p2 commands

  hClose handle