module UI.Attributes
  ( defAttrMap
  , listSelectedA
  , bol
  ) where

import           Brick                   (AttrMap, AttrName, attrMap, attrName)
import           Brick.Util
import           Graphics.Vty
import qualified Graphics.Vty.Attributes as Vty

defAttrMap :: AttrMap
defAttrMap = attrMap defAttr [ (listSelectedA , fg yellow `withStyle` Vty.underline)
                             , (bol           , defAttr `withStyle` Vty.bold)
                             ]
listSelectedA :: AttrName
listSelectedA = attrName "listSelected"

bol :: AttrName
bol = attrName "bold"
