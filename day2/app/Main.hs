module Main (main) where

import System.Environment (getArgs)
import Lib (getLines, sumEligibleID, GameRules(..))

gameRules :: GameRules
gameRules = GameRules
            { blue = 14
            , red = 12
            , green = 13
            }

main :: IO ()
main = (getArgs >>= getLines . head) >>= (print . sumEligibleID gameRules)
