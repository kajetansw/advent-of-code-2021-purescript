module Ex1 (runEx1) where

import Prelude

import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding as NE
import Node.FS.Sync (readTextFile)

getFileContent :: Effect String
getFileContent = readTextFile NE.UTF8 "static/ex1.txt"

toDepthMeasurements :: String -> Array String
toDepthMeasurements text = split (Pattern "\n") text

runEx1 :: Effect Unit
runEx1 = do
  fileContent <- getFileContent
  log $ show $ toDepthMeasurements fileContent