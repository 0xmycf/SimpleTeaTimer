{-# LANGUAGE ViewPatterns #-}
module UI.CustomMenu
    ( drawCustomMenu
    , handleEvent
    ) where

import           Brick
import qualified Brick.Widgets.Border       as Border
import qualified Brick.Widgets.Border.Style as BorderStyle
import           Brick.Widgets.Center
import           Brick.Widgets.List
import           Data.Maybe                 (fromMaybe)
import           GongDaoBei
import           Graphics.Vty
import           Lens.Micro.Platform
import           UI.MainMenu
import           UI.UIState

type CustomMenuState = MainMenuState

drawCustomMenu :: CustomMenuState -> [Widget Name]
drawCustomMenu ((^. mmlist) -> mml) =
                   let focusElem = fromMaybe 0 $ listSelected mml
                       lenMml = length mml
                       s | lenMml > lenmmList && focusElem  < lenMml - lenmmList - 1 = "..."
                         | otherwise = " "
                   in [  joinBorders $
                      center $
                      withBorderStyle BorderStyle.unicodeRounded $
                      hLimit 40 $
                      Border.borderWithLabel
                      {----------}(str "SimpleTeaTimer") ${--------}
                      {-   |   -}                         {-   |   -}
                      {-   |   -}  padTop (Pad 1)         {-   |   -}
                      {-   |   -}  (drawList mml)         {-   |   -}
                      {-   |   -}      <=>                {-   |   -}
                      {-   |   -}  padLeft(Pad 3)(str s)  {-   |   -}
                   ]  {-   --------------------------------------  -}

drawList :: MML -> Widget Name
drawList = vLimit (lenmmList + 1) . renderListWithIndex drawListElem True
  where
  drawListElem _ isSelected (Right tea') = padLeft (Pad 3) $
                                if isSelected
                                  then attrName "listSelected" `withAttr` str (pShow tea')
                                  else str (pShow tea')
  drawListElem _ isSelected (Left s) = padLeft (Pad 3) $
                                if isSelected
                                  then attrName "listSelected" `withAttr` str s
                                  else str s

handleEvent :: CustomMenuState -> BrickEvent Name e2 -> EventM Name BrickState ()
handleEvent ((^. mmlist) -> currentList) event = do
                        case event of
                          VtyEvent event' ->
                            case event' of
                              EvKey (KChar 'q') [] -> halt
                              EvKey (KChar 'b') [] -> goBack
                              EvKey KBS         [] -> goBack
                              EvKey KEnter [] -> do
                                case listSelectedElement currentList of
                                  Nothing      -> error "pressed enter on idk what"
                                  Just (_, Right tea') -> zoom ibrickState $
                                    put $ BrewingMenu BMS
                                      { _timeLeft = calcInfusion tea' 0
                                      , _tea      = tea'
                                      , _infusion = 0
                                      , _pause    = True
                                      , _runningThread = Nothing
                                      }
                                  Just (_, Left "New") -> zoom ibrickState $ put customForm
                                  Just (_, Left s)     ->
                                    error $ "Something went wrong. Please report this as a bug: " <> show s
                              _                    -> zoom (ibrickState . cms . mmlist) $ handleListEventVi handleListEvent event'
                          _ -> pure ()
  where
  goBack = zoom ibrickState $
    put $ mainMenu (listMoveToElement (Left "Custom") mmList)

