{-# LANGUAGE RecordWildCards #-}
module UI.Glue
  ( drawApp
  , handleEvent
  ) where

import           Brick
import           Lens.Micro.Platform
import qualified UI.BrewingMenu      as BM
import qualified UI.CustomFormPage   as CF
import qualified UI.CustomMenu       as CM
import qualified UI.MainMenu         as MM
import           UI.UIState

drawApp :: BrickState -> [Widget Name]
drawApp state = case state ^. ibrickState of
  MainMenu {..}    -> MM.drawMainMenu _mms
  BrewingMenu {..} -> BM.drawBrewingMenu _bms
  CustomMenu {..}  -> CM.drawCustomMenu _cms
  CustomForm {..}  -> CF.drawCustomForm _cfs

handleEvent :: BrickState -> BrickEvent Name Event -> EventM Name BrickState ()
handleEvent state = case state ^. ibrickState of
                      MainMenu {..}    -> MM.handleMainMenu _mms
                      BrewingMenu {..} -> BM.handleEvent state _bms
                      CustomMenu {..}  -> CM.handleEvent _cms
                      CustomForm {..}  -> CF.handleEvent _cfs

