module GongDaoBei
  ( goodBye,
    waitConstant,
    whites,
    blacks,
    greens,
    yellows,
    oolongballs,
    oolongsstrips,
    puerhrripes,
    puerhrstrips,
    printR
  )
where

import           Data.Char   (toLower)
import           System.Exit (exitSuccess)
import           System.IO   (hPutStr, putStr, stderr)

goodBye :: String -> IO b
goodBye msg = do
  putStrLn msg
  exitSuccess

waitConstant :: Int
waitConstant = 1000000

printR :: String -> IO ()
printR = hPutStr stderr

whites, greens, yellows, blacks, oolongsstrips, oolongballs, puerhrripes, puerhrstrips :: [String]
whites = [
           "white"
         , "weiß"
         ]

greens = [
           "green"
         , "grün"
         ]

yellows = [
            "yellow"
          , "gelb"
          ]

blacks = [
           "black"
         , "schwarz"
         ]

oolongsstrips = [
            "oolongstrip"
          , "oolong_strip"
          , "oolongs"
          ]

oolongballs = [
            "oolong"
          , "oolongball"
          , "oolong_ball"
          , "oolongb"
          ]

puerhrripes = [
            "puerhripe"
          , "puerhr"
          , "puerh"
          , "puerh_ripe"
          ]

puerhrstrips = [
            "puerhstrip"
          , "puerhs"
          , "puerh_strip"
          ]

