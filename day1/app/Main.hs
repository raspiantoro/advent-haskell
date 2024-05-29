module Main (main) where

import System.Environment (getArgs)
import System.IO (openFile, IOMode (ReadMode), hGetContents)
import Lib (processLines)

main :: IO ()
main = do
    args <- getArgs
    let filename = head args
    file <- openFile filename ReadMode
    input <- hGetContents file
    let keys = (processLines . lines) input
    putStrLn $ "Your calibration keys is: " ++ show (sum keys)
