module Check (firstRow, firstSplit, game, gameStr, restStr1, gameId, setsStr, gameRules, sets, firstSet, cubesSet, cubesGroup, foldcube, games) where
import Sample (sampleSet)
import Lib (extractGame, Game, extractLineSets, Sets (..), Cube (..), Games, processLine, GameRules(..))
import Data.List.Split (splitOn)
import Data.List (group, sort)

firstRow :: String
firstRow = head sampleSet

game :: Game
game = extractGame firstRow

games :: Games
games = processLine sampleSet

firstSplit :: [[Char]]
firstSplit = splitOn ":" firstRow

secondSplit :: ([Char], [Char])
secondSplit = (head firstSplit, head  $ tail firstSplit)

gameStr :: [Char]
gameStr = fst secondSplit

restStr1 :: [Char]
restStr1 = snd secondSplit

gameId :: Int
gameId = read $ (head . tail . words) gameStr :: Int

setsStr :: [[Char]]
setsStr = splitOn ";" restStr1

sets :: [Sets]
sets = extractLineSets setsStr

firstSet :: Sets
firstSet = head sets

cubesSet :: [Cube]
(Sets cubesSet) = firstSet

cubesGroup :: [[Cube]]
cubesGroup = (group . sort) cubesSet

foldcube :: Cube
foldcube = foldl (+) (toEnum (fromEnum (head [Blue 8,Blue 1]))) [Blue 8,Blue 1]

gameRules :: GameRules
gameRules = GameRules
            { blue = 14
            , red = 12
            , green = 13
            }


