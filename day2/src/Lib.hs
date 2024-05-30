{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Lib
    ( Cube(..)
    , Sets(..)
    , Game(..)
    , Games(..)
    , GameRules(..)
    , extractLinecubes
    , extractLineSets
    , extractGame
    , gameId
    , extractGames
    , processLine
    , getLines
    , extractEligibleStatus
    , compareToRules
    , isEligible
    , collectEligibleStatus
    , testtest
    , sumEligibleID) where

import Data.List.Split (splitOn)
import qualified Data.Text as T
import Data.Maybe (isJust, fromJust)
import System.IO (openFile, IOMode (ReadMode), hGetContents)

data Cube = Blue Int
          | Red Int
          | Green Int
          | InvalidCube
          deriving (Show)

instance Eq Cube where
   (==) :: Cube -> Cube -> Bool
   (==) (Blue _) (Blue _) = True
   (==) (Red _) (Red _) = True
   (==) (Green _) (Green _) = True
   (==) _ _ = False

instance Ord Cube where
  compare :: Cube -> Cube -> Ordering
  compare x y = compare (priority x) (priority y)
    where
        priority :: Cube -> Int
        priority (Blue _) = 1
        priority (Red _) = 2
        priority (Green _) = 3
        priority InvalidCube = 4

instance Enum Cube where
  toEnum :: Int -> Cube
  toEnum 1 = Blue 0
  toEnum 2 = Red 0
  toEnum 3 = Green 0
  toEnum _ = InvalidCube

  fromEnum :: Cube -> Int
  fromEnum (Blue _) = 1
  fromEnum (Red _) = 2
  fromEnum (Green _) = 3
  fromEnum InvalidCube = 0

instance Num Cube where
  (+) :: Cube -> Cube -> Cube
  (+) (Blue x) (Blue y) = Blue (x+y)
  (+) (Red x) (Red y) = Red (x+y)
  (+) (Green x) (Green y) = Green (x+y)
  (+) _ _ = InvalidCube

  (-) :: Cube -> Cube -> Cube
  (-) (Blue x) (Blue y) = Blue (x-y)
  (-) (Red x) (Red y) = Red (x-y)
  (-) (Green x) (Green y) = Green (x-y)
  (-) _ _ = InvalidCube

  (*) :: Cube -> Cube -> Cube
  (*) (Blue x) (Blue y) = Blue (x*y)
  (*) (Red x) (Red y) = Red (x*y)
  (*) (Green x) (Green y) = Green (x*y)
  (*) _ _ = InvalidCube

  negate :: Cube -> Cube
  negate (Blue x) = Blue (-x)
  negate (Red x) = Red (-x)
  negate (Green x) = Green (-x)
  negate InvalidCube= InvalidCube

  abs :: Cube -> Cube
  abs x = x

  signum :: Cube -> Cube
  signum (Blue x)
        | x > 0 = 1
        | x < 0 = -1
        | otherwise = 0
  signum (Red x)
        | x > 0 = 1
        | x < 0 = -1
        | otherwise = 0
  signum (Green x)
        | x > 0 = 1
        | x < 0 = -1
        | otherwise = 0
  signum InvalidCube = 0

  fromInteger :: Integer -> Cube
  fromInteger 1 = Blue 0
  fromInteger 2 = Red 0
  fromInteger 3 = Green 0
  fromInteger _ = InvalidCube

newtype Sets = Sets [Cube] deriving Show

instance Semigroup Sets where
  (<>) :: Sets -> Sets -> Sets
  (<>) (Sets []) (Sets cubes2) = Sets cubes2
  (<>) (Sets cubes1) (Sets []) = Sets cubes1
  (<>) (Sets cubes1) (Sets cubes2) = Sets $ mconcat [cubes1, cubes2]

instance Monoid Sets where
  mempty = Sets []

data Game = Game Int Sets deriving Show

gameId :: Game -> Int
gameId (Game gid _) = gid

newtype Games = Games [Game] deriving Show

data GameRules = GameRules 
    { blue :: Int
    , red :: Int
    , green :: Int
    } deriving Show

getLines :: FilePath -> IO Games
getLines filename = do
    file <- openFile filename ReadMode
    input <- hGetContents file
    let games = (processLine . lines) input
    return games

processLine :: [String] -> Games
processLine lineStr = Games games
    where
        games = extractGames lineStr

extractGames :: [String] -> [Game]
extractGames [] = []
extractGames (x:xs) = game : extractGames xs
    where
        game = extractGame x

extractGame :: String -> Game
extractGame lineStr = Game gameId sets
    where
        firstSplit = splitOn ":" lineStr
        (gameStr, restStr1) = (head firstSplit, head  $ tail firstSplit)
        gameId = read $ (head . tail . words) gameStr :: Int
        setsStr = splitOn ";" restStr1
        sets = mconcat $ extractLineSets setsStr

extractLineSets :: [String] -> [Sets]
extractLineSets [] = []
extractLineSets (x:xs) = Sets cubes : extractLineSets xs
    where
        cubesStr = map (T.unpack . T.strip . T.pack) $ splitOn "," x
        maybecubes = extractLinecubes cubesStr
        cubes = map fromJust maybecubes

extractLinecubes :: [String] -> [Maybe Cube]
extractLinecubes [] = []
extractLinecubes (x:xs) = filter isJust $ cube : extractLinecubes xs
    where
        cubeStr = words x
        cube = case (head . tail) cubeStr of
               "green" -> Just $ Green (read (head cubeStr))
               "red" -> Just $ Red (read (head cubeStr))
               "blue" -> Just $ Blue (read (head cubeStr))
               _ -> Nothing

sumEligibleID :: GameRules -> Games -> Int
sumEligibleID rules (Games games) = sum eligibleIds
    where
        extractedStatus = extractEligibleStatus rules games
        eligibleIds = map fst $ filter and extractedStatus

testtest :: GameRules -> Games -> [(Int, Bool)]
testtest rules (Games games) = extractEligibleStatus rules games


extractEligibleStatus :: GameRules -> [Game] -> [(Int, Bool)]
extractEligibleStatus _ [] = []
extractEligibleStatus rules (x:xs) = gameIdsStatus : extractEligibleStatus rules xs
    where
        (Game gameId sets) = x
        gameIdsStatus = collectEligibleStatus rules gameId sets

compareToRules :: GameRules -> Cube -> Bool
compareToRules rules (Blue x) = x <= rules.blue
compareToRules rules (Red x) = x <= rules.red
compareToRules rules (Green x) = x <= rules.green
compareToRules _ _ = False

collectEligibleStatus :: GameRules -> Int -> Sets -> (Int, Bool)
collectEligibleStatus rules gameId sets = (gameId, isEligible rules sets)

isEligible :: GameRules -> Sets -> Bool
isEligible  rules (Sets cubes) = all (compareToRules rules) cubes