module Main where

import Control.Concurrent (threadDelay)
import GHC.Unicode (toLower)
import System.Exit (exitFailure, exitSuccess)

data Tea
  = White
  | Green
  | Yellow
  | OolongStrip
  | OolongBall
  | PuerhRipe
  | PuerhStrip
  | Black
  | NoTea
  deriving (Show)

waitConstant :: Int
waitConstant = 1000000

getTea = getInput >>= mapStringToTea

main :: IO ()
main = do
  tea <- getTea
  teapot (0, tea)

teapot :: (Int, Tea) -> IO ()
teapot (i, NoTea) = do
  teaInfo
  main
teapot (i, tea) = mapTeaToTime tea i >>= brewTheTea >>= teapot

toLowerCase :: IO String -> IO String
toLowerCase = (fmap . map) toLower

getInput :: IO String
getInput = do
  putStrLn "Enter the tea."
  getLine

mapStringToTea :: String -> IO Tea
mapStringToTea "white"        = return White
mapStringToTea "weiß"         = return White
mapStringToTea "green"        = return Green
mapStringToTea "grün"         = return Green
mapStringToTea "yellow"       = return Yellow
mapStringToTea "gelb"         = return Yellow
mapStringToTea "oolongstrip"  = return OolongStrip
mapStringToTea "oolongs"      = return OolongStrip
mapStringToTea "oolong"       = return OolongStrip
mapStringToTea "oolongball"   = return OolongBall
mapStringToTea "oolongb"      = return OolongBall
mapStringToTea "puerhripe"    = return PuerhRipe
mapStringToTea "puerhr"       = return PuerhRipe
mapStringToTea "puerhstrip"   = return PuerhStrip
mapStringToTea "puerhs"       = return PuerhStrip
mapStringToTea "black"        = return Black
mapStringToTea "schwarz"      = return Black
mapStringToTea _              = return NoTea

mapTeaToTime :: Tea -> Int -> IO (Int, Tea)
mapTeaToTime tea i = do
  case tea of
    NoTea -> goodBye "Please enter a valid tea you doofus"
    _ -> return (calcInfusion tea i, tea)

intToString :: Int -> IO String
intToString = return . show

goodBye :: String -> IO b
goodBye msg = do
  putStrLn msg
  exitSuccess

brewTheTea :: (Int, Tea) -> IO (Int, Tea)
brewTheTea (waitTime, tea) = do
  moreTeaValidation waitTime
  threadDelay $ waitTime * waitConstant
  putStrLn "Enjoy!"
  return (waitTime, tea)

calcInfusion :: Tea -> Int -> Int
calcInfusion White 0        = 20
calcInfusion Green 0        = 15
calcInfusion Yellow 0       = 15
calcInfusion OolongStrip 0  = 20
calcInfusion OolongBall 0   = 25
calcInfusion PuerhRipe 0    = 10
calcInfusion PuerhStrip 0   = 10
calcInfusion Black 0        = 10
calcInfusion NoTea 0        = 0
calcInfusion White x        = x + 10
calcInfusion Green x        = x + 3
calcInfusion Yellow x       = x + 5
calcInfusion OolongStrip x  = x + 5
calcInfusion OolongBall x   = x + 5
calcInfusion PuerhRipe x    = x + 5
calcInfusion PuerhStrip x   = x + 3
calcInfusion Black x        = x + 5
calcInfusion NoTea x        = x + 0

moreTeaValidation :: Int -> IO ()
moreTeaValidation s = do
  let seconds = show s
  putStrLn $ "Continue brewing for " ++ seconds ++ " seconds?"
  val <- getLine
  case val of
    "yes"     -> putStrLn $  "Brewing the tea for " ++ seconds ++ " seconds!"
    "y"       -> putStrLn $ "Brewing the tea for " ++ seconds ++ " seconds!"
    "yea"     -> putStrLn $ "Brewing the tea for " ++ seconds ++ " seconds!"
    "yeah"    -> putStrLn $ "Brewing the tea for " ++ seconds ++ " seconds!"
    "ja"      -> putStrLn $ "Brewing the tea for " ++ seconds ++ " seconds!"
    "j"       -> putStrLn $ "Brewing the tea for " ++ seconds ++ " seconds!"
    _         -> goodBye "I hope you enjoyed your tea!"

teaInfo :: IO ()
teaInfo =  
  putStr "|The following teas are available:\n\
          \|kind           gr/100ml  time      temp\n\
          \|white          3.5/4gr   20/+10s    85C\n\
          \|green          3/3.5gr   15/+3s     80C\n\
          \|yellow         3.5/4gr   15/+5s     85C\n\
          \|oolong_strip   4.5/5gr   20/+5s     99C\n\
          \|oolong_ball    6/6.5gr   25/+5s     99C\n\
          \|black          4/4.5gr   10-15/+5s  99C\n\
          \|puerh_ripe     5gr       10/+3s     99C\n\
          \|puerh_raw      5gr       10/+5s     99C\n\
          \|custom         Enter first first-infusion time and then the added time per infusion.\n\
          \|\n\
          \|Keep in mind that those values are made for 功夫 style brewing!\n"