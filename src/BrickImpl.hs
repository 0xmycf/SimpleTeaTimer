module BrickImpl
  ( module UI
  , module G
  , runBrick
  ) where

import           Brick         (App(..), customMain, get, showFirstCursor)
import           Brick.BChan   (newBChan)
import           Graphics.Vty
import           UI.Attributes as UI
import           UI.Glue       as G
import           UI.MainMenu   as UI
import           UI.UIState    as UI


-- The App state EventType RessourceType
app' :: App BrickState UI.Event Name
app' = App { appStartEvent   = pure ()
           , appHandleEvent  = \e -> get >>= flip G.handleEvent e
           , appDraw         = G.drawApp
           , appChooseCursor = showFirstCursor
           , appAttrMap      = const UI.defAttrMap
           }

runBrick :: IO ()
runBrick = do
  chan <- newBChan 2
  let buildVty = mkVty defaultConfig
  initialVty  <- buildVty
  _finalState <- customMain initialVty buildVty
                    (Just chan) app' (BS (mainMenu mmList) chan)
  pure ()

