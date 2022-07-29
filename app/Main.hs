module Main where

import           Control.Concurrent         (threadDelay)
import           Control.Monad.Reader       (MonadIO (liftIO),
                                             MonadReader (ask),
                                             ReaderT (runReaderT), guard)
import           Control.Monad.State        (MonadState (..), StateT, runStateT)
import           Data.Functor               (($>))
import           Data.List.Split            (splitOn)
import           GHC.Unicode                (toLower)
import           GongDaoBei                 (blacks, goodBye, greens,
                                             oolongballs, oolongsstrips, printR,
                                             puerhrripes, puerhrstrips,
                                             waitConstant, whites, yellows)
import           System.Process             (callCommand)

import           Control.Monad.State.Strict (State)
import           System.Exit                (exitFailure)

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

type TeaMonad a = ReaderT Tea (StateT Int IO) a

getTea :: IO (Maybe Tea)
getTea = mapStringToTea <$> getInput

main :: IO ()
main = do
  tea <- getTea
  case tea of
    Nothing   -> teaInfo >> main
    Just tea' -> teapot tea' 0

teapot :: Tea -> Int -> IO ()
teapot tea i = runStateT (runReaderT brew tea) i $> ()

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
                else pure . map toLower $ c
    [] -> goodBye "Please enter any tea..."

mapStringToTea :: String -> Maybe Tea
mapStringToTea ('c':'u':'s':'t':'o':'m':rest)   =
  let times = filter (/="") . splitOn " " $ rest
   in Just . Custom ((read . head) times) $ (read . last) times
mapStringToTea string
     | string `elem` whites        = Just White
     | string `elem` blacks        = Just Black
     | string `elem` greens        = Just Green
     | string `elem` yellows       = Just Yellow
     | string `elem` oolongsstrips = Just OolongStrip
     | string `elem` oolongballs   = Just OolongBall
     | string `elem` puerhrripes   = Just PuerhRipe
     | string `elem` puerhrstrips  = Just PuerhStrip
     | otherwise                   = Nothing

brew :: TeaMonad ()
brew = do
  tea     <- ask
  newtime <- incTime
  liftIO $ moreTeaValidation newtime
  waitForBrew newtime
  brew

incTime :: TeaMonad Int
incTime = do
  time <- get
  tea  <- ask
  let newtime = calcInfusion tea time
  put (time + 1)
  return newtime

waitForBrew :: Int -> TeaMonad ()
waitForBrew time = do
  liftIO $ updateTxt time
  -- mac command, might not work in Windows or Linux
  liftIO $ callCommand "say 'Enjoy!'"
  where
    updateTxt :: Int -> IO ()
    updateTxt w = mapM_ progress [w, w-1..0]

    progress :: Int -> IO ()
    progress 0 = printR $ "\r\ESC[K" ++ "Enjoy!\n"
    progress s = do
      printR $ "\r\ESC[K" ++ "Brewing the tea for " ++ show s ++ " seconds!"
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

moreTeaValidation :: Int -> IO ()
moreTeaValidation s = do
  putStrLn $ "Continue brewing for " ++ show s ++ " seconds?"
  val <- getLine
  -- list is short so using a set with Set.member is not necessary
  if val `elem` yeses
     then pure ()
     else goodBye "I hope you enjoyed your tea!"
   where
     yeses :: [[Char]]
     yeses   = ["yes", "y", "yea", "yeah", "ja", "j"]

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

