{-# LANGUAGE DeriveAnyClass, LambdaCase, RecordWildCards #-}
module UI.UIState
  ( Name(..)
  , BrickState(..)
  , InternalBrickState(..)
  , BrewingMenuState(..)
  , MainMenuState(..)
  , CustomFormState(..)
  , TeaSettings(..)
  , MML
  , Event(..)
  , mmlist
  , cms
  , form
  , shouldSave
  , bms
  , name
  , tea
  , infusion
  , incrementDuration
  , baseDuration
  , timeLeft
  , pause
  , runningThread
  , evChan
  , ibrickState
  , mms
  , mainMenu
  , customMenu
  , cfs
  , mmList
  , lenmmList
  , mmState
  , customForm
  , startBrew
  , toTea
  ) where

import           Brick.BChan            (BChan, writeBChan)
import           Brick.Forms
import           Brick.Widgets.List
import           Control.Concurrent     (ThreadId, forkFinally, threadDelay)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.Aeson             as A
import           Data.Text              (Text)
import qualified Data.Vector            as V
import qualified GHC.Exception          as E
import           GHC.Generics
import           GongDaoBei
import           Lens.Micro.Platform

type MML = List Name (Either String Tea)

data Name
  = MMList
  | CMList
  | CustomTeaNameField
  | CustomTeaBaseInfField
  | CustomTeaIncrInfField
  | CustomTeaSaveField
  deriving (Eq, Ord, Show)

data BrickState
  = BS
      { _ibrickState :: InternalBrickState
      , _evChan      :: BChan Event
      }

data InternalBrickState
  = MainMenu
      { _mms :: MainMenuState
      }
  | CustomMenu
      { _cms :: MainMenuState
      }
  | CustomForm
      { _cfs :: CustomFormState
      }
  | BrewingMenu
      { _bms :: BrewingMenuState
      }

newtype MainMenuState
  = MMS { _mmlist :: MML }

mmList :: MML
mmList = list MMList (V.fromList
                 [ Right Green
                 , Right Black
                 , Right OolongBall
                 , Right OolongStrip
                 , Right White
                 , Right Yellow
                 , Right PuerhRipe
                 , Right PuerhStrip
                 , Left "Custom"
 --                , Left "Save Custom"
                 , Left "Info"
                 , Left "Quit"
                 ]) 1

lenmmList :: Int
lenmmList = length mmList

mmState :: InternalBrickState
mmState = mainMenu mmList

mainMenu :: MML -> InternalBrickState
mainMenu = MainMenu . MMS
customMenu :: MML -> InternalBrickState
customMenu = CustomMenu . MMS

data BrewingMenuState
  = BMS
      { _tea           :: Tea
      , _infusion      :: Int
      , _timeLeft      :: Int
      , _pause         :: Bool
      , _runningThread :: Maybe ThreadId
      }

newtype CustomFormState
  = CFS { _form :: Form TeaSettings Event Name }

data Event
  = Tick
  | Finished

data TeaSettings
  = TSettings
      { _name              :: Text
      , _baseDuration      :: Int
      , _incrementDuration :: Int
      , _shouldSave        :: Bool
      }
  deriving (Generic, Show)

instance ToJSON TeaSettings where
    toJSON :: TeaSettings -> Value
    toJSON TSettings {..} =
        object [ "name" A..= _name
               , "baseDuration" A..= _baseDuration
               , "incrementDuration" A..= _incrementDuration
               ]

instance FromJSON TeaSettings where
  parseJSON = withObject "TeaSettings" $ \v -> do
    name'    <- v .: "name"
    basedur  <- v .: "baseDuration"
    incdur   <- v .: "incrementDuration"

    pure (TSettings name' basedur incdur False)

toTea :: TeaSettings -> Tea
toTea = Custom <$> _name <*> _baseDuration <*> _incrementDuration

makeLenses ''BrickState
makeLenses ''InternalBrickState
makeLenses ''TeaSettings
makeLenses ''BrewingMenuState
makeLenses ''MainMenuState
makeLenses ''CustomFormState

customFormState :: CustomFormState
customFormState = CFS f
  where
  f = newForm [ editTextField name CustomTeaNameField (Just 1)
              , editShowableField baseDuration CustomTeaBaseInfField
              , editShowableField incrementDuration CustomTeaIncrInfField
              , checkboxField shouldSave CustomTeaSaveField "Save to disk?"
              ]
        TSettings { _name               = "Custom Tea"
                  , _incrementDuration  = 5
                  , _baseDuration       = 10
                  , _shouldSave         = False
                  }

customForm :: InternalBrickState
customForm = CustomForm customFormState

startBrew :: (MonadIO m, Num a, Enum a) => a -> BChan Event -> m ThreadId
startBrew timeLeft' evChan' = do
  liftIO $ forkFinally
    (forM_ [1..timeLeft'] $
      \_ -> threadDelay waitConstant *> writeBChan evChan' Tick)
    (\case
      Left err -> E.throw err
      Right  _ -> writeBChan evChan' Finished
    )
