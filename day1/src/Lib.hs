module Lib
    ( someFunc
    , CalibrationKey(..)
    , calibrationKeyValue
    , fromTuple
    , findKeyPieces
    , processLines) where

import Data.Char (isDigit, ord)
import Data.Maybe (isNothing, isJust, fromJust)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type RKey = Int
type LKey = Int

data CalibrationKey = CalibrationKey LKey RKey deriving Show

fromTuple :: (Int, Int) -> CalibrationKey
fromTuple (l, r) = CalibrationKey l r

calibrationKeyValue :: CalibrationKey -> Int
calibrationKeyValue (CalibrationKey l r) = ((+ r) . (*10)) l

charToInt :: Char -> Int
charToInt c = ord c - ord '0'

findKeyPieces' :: (Maybe Int, Maybe Int) -> String -> Maybe (Int, Int)
findKeyPieces' (Nothing, _) [] = Nothing
findKeyPieces' (Just l, Nothing) [] = Just (l, l)
findKeyPieces' (Just l, Just r) []  = Just (l, r)
findKeyPieces' (l, r) (x:xs)
        | (not . isDigit) x = findKeyPieces' (l,r) xs
        | isNothing l = findKeyPieces' (Just (charToInt x), r) xs
        | otherwise = findKeyPieces' (l, Just (charToInt x)) xs

findKeyPieces :: String -> Maybe (Int, Int)
findKeyPieces = findKeyPieces' (Nothing, Nothing)

processLines :: [String] -> [Int]
processLines keys = map fromJust jValue
    where
        mKeys = map (fmap fromTuple . findKeyPieces) keys
        mValue = map (calibrationKeyValue <$>) mKeys
        jValue = filter isJust mValue
