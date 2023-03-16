{-|
Module      : Util
Description : Other utility functions that have no other namespace to live in
Copyright   : (c) 0xmycf, 2023
License     : MIT
Maintainer  : mycf.mycf.mycf@gmail.com
Stability   : experimental
-}
module Util
  ( intoGList
  , getXdgCache
  ) where

import           Brick.Widgets.List
import qualified Data.Vector        as V
import qualified System.Directory   as D
import           UI.UIState

intoGList :: [TeaSettings] -> MML
intoGList tss = list CMList (V.fromList $ Left "New" : map (Right . toTea) tss) 1

getXdgCache :: IO FilePath
getXdgCache = D.getXdgDirectory D.XdgCache "simple-tea-timer"

