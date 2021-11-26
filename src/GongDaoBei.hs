module GongDaoBei
  ( goodBye,
    waitConstant,
  )
where

import Data.Char (toLower)
import System.Exit (exitSuccess)

goodBye :: String -> IO b
goodBye msg = do
  putStrLn msg
  exitSuccess

waitConstant :: Int
waitConstant = 1000000