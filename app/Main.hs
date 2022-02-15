module Main where

import           Control.Concurrent (threadDelay)
import           Data.List.Split    (splitOn)
import           GHC.Unicode        (toLower)
import           GongDaoBei         (goodBye, waitConstant)
import           System.IO          (hPutStr, putStr, stderr)
import           System.Process     (callCommand)

type BaseInfusion        = Int
type IncreasePerInfusion = Int

data Tea
  = White
  | Green
  | Yellow
  | OolongStrip
  | OolongBall
  | PuerhRipe
  | PuerhStrip
  | Black
  | Custom BaseInfusion IncreasePerInfusion
  deriving (Show)

getTea :: IO (Maybe Tea)
getTea = mapStringToTea <$> getInput

main :: IO ()
main = do
  tea <- getTea
  case tea of
    Nothing   -> teaInfo >> main
    Just tea' -> teapot tea' 0

teapot :: Tea -> Int -> IO ()
teapot tea i = brewTheTea tea i >>= uncurry teapot

getInput :: IO String
getInput = do
  putStrLn "Enter the tea."
  input <- getLine
  case (filter (/="") . splitOn " ") input of
    c : s -> if map toLower c == "custom"
                then case s of
                    _ : _ : _ -> pure . map toLower $ input
                    _         -> putStrLn "Please enter a base infusion time as well as an increase in every step (Eg. custom 2 3)."
                                 >> getInput
                else if map toLower c == "leave"
                       then goodBye "I just heated the water..."
                       else pure . map toLower $ c

    [] -> goodBye "Please enter any tea..."

mapStringToTea :: String -> Maybe Tea
mapStringToTea "white"                          = Just White
mapStringToTea "weiß"                           = Just White
mapStringToTea "green"                          = Just Green
mapStringToTea "grün"                           = Just Green
mapStringToTea "yellow"                         = Just Yellow
mapStringToTea "gelb"                           = Just Yellow
mapStringToTea "oolongstrip"                    = Just OolongStrip
mapStringToTea "oolong_strip"                   = Just OolongStrip
mapStringToTea "oolongs"                        = Just OolongStrip
mapStringToTea "oolong"                         = Just OolongStrip
mapStringToTea "oolongball"                     = Just OolongBall
mapStringToTea "oolong_ball"                    = Just OolongBall
mapStringToTea "oolongb"                        = Just OolongBall
mapStringToTea "puerhripe"                      = Just PuerhRipe
mapStringToTea "puerhr"                         = Just PuerhRipe
mapStringToTea "puerhstrip"                     = Just PuerhStrip
mapStringToTea "puerhs"                         = Just PuerhStrip
mapStringToTea "black"                          = Just Black
mapStringToTea "schwarz"                        = Just Black
mapStringToTea ('c':'u':'s':'t':'o':'m':rest)   = Just . Custom ((read . head) times) $ (read . last) times
  where
    times = (filter (/="") . splitOn " ") rest
mapStringToTea _                                = Nothing

brewTheTea :: Tea -> Int -> IO (Tea, Int)
brewTheTea tea waitTime = do
  moreTeaValidation (calcInfusion tea waitTime)
  updateTxt (calcInfusion tea waitTime)
  -- mac command, might not work in Windows or Linux
  callCommand "say 'Enjoy!'"
  pure (tea, waitTime + 1)
  where
    updateTxt :: Int -> IO ()
    updateTxt w = mapM_ progress [w, w-1..0]

    progress :: Int -> IO ()
    progress 0 = hPutStr stderr $ "\r\ESC[K" ++ "Enjoy!\n"
    progress s = do
      hPutStr stderr $ "\r\ESC[K" ++ "Brewing the tea for " ++ show s ++ " seconds!"
      threadDelay $ 1 * waitConstant
    -- source : https://stackoverflow.com/questions/8953636/simple-progress-indication-in-console

-- Linear (affine) function to map the time to brewing time
calcInfusion :: Tea -> Int -> Int
calcInfusion White x            = x * 10 + 20
calcInfusion Green x            = x * 3  + 15
calcInfusion Yellow x           = x * 5  + 15
calcInfusion OolongStrip x      = x * 5  + 20
calcInfusion OolongBall x       = x * 5  + 25
calcInfusion PuerhRipe x        = x * 5  + 10
calcInfusion PuerhStrip x       = x * 3  + 10
calcInfusion Black x            = x * 5  + 10
calcInfusion (Custom inf inc) x = x * inc + inf

yeses :: [[Char]]
yeses   = ["yes", "y", "yea", "yeah", "ja", "j"]

moreTeaValidation :: Int -> IO ()
moreTeaValidation s = do
  putStrLn $ "Continue brewing for " ++ show s ++ " seconds?"
  val <- getLine
  -- list is short so using a set with Set.member is not necessary
  if val `elem` yeses
     then pure ()
     else goodBye "I hope you enjoyed your tea!"

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

