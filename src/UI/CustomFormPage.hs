module UI.CustomFormPage
  ( drawCustomForm
  , handleEvent
  ) where

import           Brick
import           Brick.Forms
import qualified Brick.Widgets.Border       as Border
import qualified Brick.Widgets.Border.Style as BorderStyle
import           Brick.Widgets.Center
import           Brick.Widgets.List
import           Control.Monad              (when)
import           Control.Monad.IO.Class
import           Data.Maybe                 (fromMaybe)
import           GongDaoBei
import           Graphics.Vty
import           Json
import           Lens.Micro.Platform
import           System.IO.Error            (catchIOError)
import           UI.UIState
import           Util                       (intoGList)

drawCustomForm :: CustomFormState -> [Widget Name]
drawCustomForm cfs' = [  joinBorders $
                  center $
                  withBorderStyle BorderStyle.unicodeRounded $
                  hLimit 40 $
                  Border.borderWithLabel
                  {----------}(str "SimpleTeaTimer") ${--------}
                  {-   |   -}                         {-   |   -}
                  -- {-   |   -}  padTop (Pad 1)      {-   |   -}
                  {-   |   -}  renderForm (cfs' ^. form)  {-   |   -}
               ]  {-   --------------------------------------  -}

handleEvent :: CustomFormState -> BrickEvent Name UI.UIState.Event -> EventM Name BrickState ()
--handleEvent _cfs ev = zoom ibrickState $ do
handleEvent _cfs ev = zoom ibrickState $ do
                      case ev of
                          VtyEvent event' ->
                            case event' of
                              EvKey (KChar 'q') [] -> halt
                              EvKey (KChar 'b') [] -> goBack
                              -- Can't enable because of the text field
                              -- EvKey KBS         [] -> goBack
                              EvKey KEnter      [] -> handleForm
                              -- I don't like these keybindings that much but
                              -- I'll leave it like that for now
                              _                    -> zoom (cfs . form) $ handleFormEvent ev
                          _ -> pure ()
  where
  goBack = do
    tts' <- liftIO $ readJSON `catchIOError` (\_ -> pure Nothing)
    let tts = fromMaybe [] tts' & intoGList
    put $ customMenu (listMoveToBeginning tts)
  handleForm = let fs   = _cfs ^. form . to formState
                   tea' = Custom (fs ^. name) (fs ^. baseDuration) (fs ^. incrementDuration)
                in do
                when (fs ^. shouldSave) $ do
                  liftIO $ writeJSON fs
                put $ BrewingMenu BMS
                          { _timeLeft = calcInfusion tea' 0
                          , _tea      = tea'
                          , _infusion = 0
                          , _pause    = True
                          , _runningThread = Nothing
                          }

