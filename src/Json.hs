module Json
    ( TeaSettings(..)
    , name
    , baseDuration
    , incrementDuration
    , writeJSON
    , readJSON
    ) where

import qualified Control.Exception        as E
import           Control.Monad.IO.Class
import           Data.Aeson               (decodeStrict)
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString          as BS
import           System.FilePath          ((</>))
import           System.IO.Error          (catchIOError, isDoesNotExistError)
import           UI.UIState
import           Util                     (getXdgCache)

-- | appends the TeaSettings to the teas file
-- Creates the tea file if it does't exist
--
-- Assumes that the xdg XdgDirectory exists
--
-- May crash with IOError
writeJSON :: MonadIO m => TeaSettings -> m ()
writeJSON content = do
  cache <- liftIO getXdgCache
  let filepath = cache </> "teas.json"
  ts <- liftIO $ readJSON `catchIOError`
    (\err -> if isDoesNotExistError err
              then pure Nothing
              else E.throw err)
  liftIO $ case ts of
    Nothing  ->
      BS.writeFile filepath
        (encodeStrict [content])
    Just ts' ->
      BS.writeFile filepath
        (encodeStrict $ content : ts')
  where encodeStrict = BS.toStrict . encodePretty

-- | May crash with IOError
--
-- Assumes that the xdg XdgDirectory exists
readJSON :: MonadIO m => m (Maybe [TeaSettings])
readJSON = do
  cache <- liftIO getXdgCache
  ts <- liftIO $ BS.readFile (cache </> "teas.json")
  pure $ decodeStrict ts

