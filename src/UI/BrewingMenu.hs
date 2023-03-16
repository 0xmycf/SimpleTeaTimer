{-# LANGUAGE RecordWildCards #-}
module UI.BrewingMenu
  ( drawBrewingMenu
  , handleEvent
  ) where

import           Brick
import qualified Brick.Widgets.Border       as Border
import qualified Brick.Widgets.Border.Style as BorderStyle
import           Brick.Widgets.Center
import           Brick.Widgets.List         (listMoveToElement)
import           Control.Concurrent         (killThread)
import           Control.Monad              (unless)
import           Control.Monad.IO.Class
import           Data.Maybe                 (fromMaybe, isNothing)
import qualified Data.Text                  as T
import           GongDaoBei                 (Tea(Custom), calcInfusion,
                                             isCustom)
import           Graphics.Vty
import           Lens.Micro.Platform
import           UI.Attributes
import           UI.UIState


drawBrewingMenu :: BrewingMenuState -> [Widget Name]
drawBrewingMenu bms' = [
                      joinBorders $
                      center $
                      withBorderStyle BorderStyle.unicodeRounded $
                      hLimit 40 $
                      Border.borderWithLabel
                      {--}(str $ "Infusion " <> show (bms'^.infusion))$ {-   |   -}
                      {-   |   -}  drawBrewingScreen bms'               {-   |   -}
                   ]  {-   ---------------------------------------------------   -}

drawBrewingScreen :: BrewingMenuState -> Widget Name
drawBrewingScreen BMS {..}
  | _pause     =
    hCenter (str $ "Continue to brew " <> pshow _tea <> " for ")
              <=>
    padBottom (Pad 1)
      (hCenter (str $ show _timeLeft <> " seconds?"))
              <=>
    hCenter (padLeftRight 1 ((bol `withAttr` str "y") <+> str "es")
      <+> padLeftRight 1 ((bol `withAttr` str "n") <+> str "o"))
              <=>
    hCenter ((bol `withAttr` str "q") <+> str "uit")

  | not _pause =
    hCenter (str $ "Brewing " <> pshow _tea <> " Tea")
              <=>
    padBottom (Pad 1) (hCenter (str "Seconds left: "))
              <=>
    padBottom (Pad 3) (hCenter (str $ show _timeLeft))
    where
    pshow (Custom name' _ _) = T.unpack name'
    pshow t                  = show t

drawBrewingScreen _ = str "Some error happened"

handleEvent :: BrickState -> BrewingMenuState -> BrickEvent Name UI.UIState.Event -> EventM Name BrickState ()
handleEvent BS {..} BMS {..} event = case event of
    VtyEvent event' ->
      case event' of
        EvKey (KChar 'q') []          -> halt

        EvKey (KChar 'n') [] | _pause -> goBack
        EvKey (KChar 'y') [] | _pause -> startBrew'

        EvKey (KChar 'b') []          -> goBack
        EvKey KBS         []          -> goBack
        _                             -> pure ()

    AppEvent Tick      -> zoom (ibrickState . bms) $ timeLeft -= 1
    AppEvent Finished  -> zoom (ibrickState . bms) $ do
      pause    .= True
      infusion += 1
      inf  <- use infusion
      tea' <- use tea
      timeLeft .= calcInfusion tea' inf
    _                  -> undefined
  where
  goBack = zoom ibrickState $ do
    -- just crash if its nothing because it will never be
    thid <- gets (^?! bms . runningThread)
    unless (isNothing thid) $
      liftIO (killThread (thid ^?! _Just))

    put $ mainMenu $ if isCustom _tea
      then listMoveToElement (Left "Custom") mmList
      else listMoveToElement (Right _tea) mmList

  startBrew' = zoom ibrickState $ do
    tl <- gets (^? bms . timeLeft) <&> fromMaybe 0
    thid <- startBrew tl _evChan
    zoom bms $ do
      pause .= False
      runningThread ?= thid
