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

main :: IO ()
main = teapot (0, toLowerCase getInput)

teapot :: (Int, IO String) -> IO ()
teapot (x, teaString) = teaString >>= mapTeaToTime . mapStringToTea x >>= brewTheTea >>= teapot2

teapot2 :: (Int, Tea) -> IO ()
teapot2 (x, tea) = moreTeaValidation getLine >> mapTeaToTime (return tea, x) >>= brewTheTea >>= teapot2

toLowerCase :: IO String -> IO String
toLowerCase = (fmap . map) toLower

getInput :: IO String
getInput = do
  putStrLn "Enter the tea."
  getLine

mapStringToTea :: Int -> String -> (IO Tea, Int)
mapStringToTea x "white" = (return White, x)
mapStringToTea x "weiß" = (return White, x)
mapStringToTea x "green" = (return Green, x)
mapStringToTea x "grün" = (return Green, x)
mapStringToTea x "yellow" = (return Yellow, x)
mapStringToTea x "gelb" = (return Yellow, x)
mapStringToTea x "oolongstrip" = (return OolongStrip, x)
mapStringToTea x "oolongs" = (return OolongStrip, x)
mapStringToTea x "oolong" = (return OolongStrip, x)
mapStringToTea x "oolongball" = (return OolongBall, x)
mapStringToTea x "oolongb" = (return OolongBall, x)
mapStringToTea x "puerhripe" = (return PuerhRipe, x)
mapStringToTea x "puerhr" = (return PuerhRipe, x)
mapStringToTea x "puerhstrip" = (return PuerhStrip, x)
mapStringToTea x "puerhs" = (return PuerhStrip, x)
mapStringToTea x "black" = (return Black, x)
mapStringToTea x "schwarz" = (return Black, x)
mapStringToTea _ _ = (return NoTea, 0)

mapTeaToTime :: (IO Tea, Int) -> IO (Int, Tea)
mapTeaToTime (tea, oldTime) = do
  val <- tea
  case val of
    NoTea -> goodBye "Please enter a valid tea you doofus"
    _ -> return (calcInfusion val oldTime, val)

intToString :: Int -> IO String
intToString = return . show

goodBye :: String -> IO b
goodBye msg = do
  putStrLn msg
  exitSuccess

brewTheTea :: (Int, Tea) -> IO (Int, Tea)
brewTheTea (wait, tea) = do
  putStrLn $ "Brewing the tea for " ++ show wait ++ " seconds!"
  threadDelay $ wait * waitConstant
  putStrLn "Enjoy!"
  return (wait, tea)

calcInfusion :: Tea -> Int -> Int
calcInfusion White 0 = 20
calcInfusion Green 0 = 15
calcInfusion Yellow 0 = 15
calcInfusion OolongStrip 0 = 20
calcInfusion OolongBall 0 = 25
calcInfusion PuerhRipe 0 = 10
calcInfusion PuerhStrip 0 = 10
calcInfusion Black 0 = 10
calcInfusion NoTea 0 = 0
calcInfusion White x = x + 10
calcInfusion Green x = x + 3
calcInfusion Yellow x = x + 5
calcInfusion OolongStrip x = x + 5
calcInfusion OolongBall x = x + 5
calcInfusion PuerhRipe x = x + 5
calcInfusion PuerhStrip x = x + 3
calcInfusion Black x = x + 5
calcInfusion NoTea x = x + 0

moreTeaValidation :: IO String -> IO ()
moreTeaValidation x = do
  putStrLn "Do you want to continue your session?"
  val <- x
  let byebye = goodBye "I hope you enjoyed your tea!"
  case val of
    "yes" -> putStrLn "Then lets continue..."
    "y" -> putStrLn "Then lets continue..."
    "yea" -> putStrLn "Then lets continue..."
    "yeah" -> putStrLn "Then lets continue..."
    "ja" -> putStrLn "Then lets continue..."
    _ -> byebye
