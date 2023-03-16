{-# LANGUAGE ViewPatterns #-}
module UI.MainMenu
  ( mmList
  , drawMainMenu
  , handleMainMenu
  , mmState
  , lenmmList
  ) where

import           Brick
import           Brick.Widgets.Border       (hBorder)
import qualified Brick.Widgets.Border       as Border
import qualified Brick.Widgets.Border.Style as BorderStyle
import           Brick.Widgets.Center
import           Brick.Widgets.List
import           Control.Monad.IO.Class
import           Data.Maybe                 (fromMaybe)
import           GongDaoBei                 (calcInfusion)
import           Graphics.Vty
import           Json                       (readJSON)
import           Lens.Micro.Platform
import           System.IO.Error            (catchIOError)
import           UI.UIState
import           Util                       (intoGList)


drawMainMenu :: MainMenuState -> [Widget Name]
drawMainMenu ((^. mmlist) -> mml) =
                   [  joinBorders $
                      center $
                      withBorderStyle BorderStyle.unicodeRounded $
                      hLimit 40 $
                      Border.borderWithLabel
                      {----------}(str "SimpleTeaTimer") ${--------}
                      {-   |   -}                         {-   |   -}
                      {-   |   -}  padTop (Pad 1)         {-   |   -}
                      {-   |   -}  (drawList mml)         {-   |   -}
                   ]  {-   --------------------------------------  -}

drawList :: MML -> Widget Name
drawList = (vLimit . (+1) . length) <*> renderListWithIndex drawListElem True
  where
  drawListElem _ isSelected (Right tea') = padLeft (Pad 3) $
                                if isSelected
                                  then attrName "listSelected" `withAttr` str (show tea')
                                  else str (show tea')
  drawListElem 9 isSelected (Left s) = hBorder <=> padLeft (Pad 3)
                                (if isSelected
                                  then attrName "listSelected" `withAttr` str s
                                  else str s)
  drawListElem _ isSelected (Left s) = padLeft (Pad 3) $
                                if isSelected
                                  then attrName "listSelected" `withAttr` str s
                                  else str s

handleMainMenu :: MainMenuState -> BrickEvent Name e2 -> EventM Name BrickState ()
handleMainMenu ((^. mmlist) -> currentList) event = do
                        case event of
                          VtyEvent event' ->
                            case event' of
                              EvKey (KChar 'q') [] -> halt
                              -- I think its nicer to move to Custom with vi keybindings
                              -- than to "quit"
                              EvKey (KChar 'G') [] -> zoom (ibrickState . mms . mmlist) $
                                get >>= \l -> put $ listMoveToElement (Left "Custom") l
                              EvKey KEnter [] -> do
                                case listSelectedElement currentList of
                                  Nothing    -> error "pressed enter on idk what"
                                  Just (_, Right tea') -> zoom ibrickState $
                                    put $ BrewingMenu BMS
                                      { _timeLeft = calcInfusion tea' 0
                                      , _tea      = tea'
                                      , _infusion = 0
                                      , _pause    = True
                                      , _runningThread = Nothing
                                      }
                                  Just (_, Left "Quit")   -> halt
                                  Just (_, Left "Custom") -> zoom ibrickState $ do
                                    tss <- liftIO $ readJSON `catchIOError` (\_ -> pure Nothing)
                                    put $ customMenu $ intoGList $ fromMaybe [] tss
                                  Just (_, Left "Info") -> pure ()
                                  Just (_, Left l) -> error $ "Report this as a bug: pressed on " <> l
                              _                    -> zoom (ibrickState . mms . mmlist) $ handleListEventVi handleListEvent event'
                          _ -> pure ()

