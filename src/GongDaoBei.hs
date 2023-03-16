module GongDaoBei
  ( Tea(..)
  , calcInfusion
  , goodBye
  , waitConstant
  , whites
  , blacks
  , greens
  , yellows
  , oolongballs
  , oolongsstrips
  , puerhrripes
  , puerhrstrips
  , printR
  , pShow
  , isCustom
  )
where

import           Data.Text   (Text)
import qualified Data.Text   as T
import           System.Exit (exitSuccess)
import           System.IO   (hPutStr, stderr)

-- Linear (affine) function to map the time to brewing time
calcInfusion :: Tea -> Int -> Int
calcInfusion White x              = x * 10 + 20
calcInfusion Green x              = x * 3  + 15
calcInfusion Yellow x             = x * 5  + 15
calcInfusion OolongStrip x        = x * 5  + 20
calcInfusion OolongBall x         = x * 5  + 25
calcInfusion PuerhRipe x          = x * 5  + 10
calcInfusion PuerhStrip x         = x * 3  + 10
calcInfusion Black x              = x * 5  + 10
calcInfusion (Custom _ inf inc) x = x * inc + inf


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

type BaseInfusion        = Int
type IncreasePerInfusion = Int
type Name                = Text

data Tea
  = White
  | Green
  | Yellow
  | OolongStrip
  | OolongBall
  | PuerhRipe
  | PuerhStrip
  | Black
  | Custom Name BaseInfusion IncreasePerInfusion
  deriving (Eq, Show)

isCustom :: Tea -> Bool
isCustom Custom {} = True
isCustom _         = False

pShow :: Tea -> String
pShow (Custom n b i) = unwords $ T.unpack n : map show [b, i]
pShow t              = show t

