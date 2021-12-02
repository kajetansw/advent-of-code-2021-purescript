module Ex1 (runEx1) where

import Prelude

import Data.List (List(..), fromFoldable, mapMaybe, snoc)
import Data.Foldable (foldl)
import Data.String (split)
import Data.Maybe (Maybe(..))
import Data.Int (fromString)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding as NE
import Node.FS.Sync (readTextFile)
import Data.List.Types (List)

data DepthChange = Decrease | NotChanged | Increase

derive instance eqDepthChange :: Eq DepthChange

type Depth =
  { measurement :: Int
  , change :: DepthChange
  }

-- TODO add text format validation
getFileContent :: Effect String
getFileContent = readTextFile NE.UTF8 "static/ex1.txt"

toList :: forall a. Array a -> List a
toList = fromFoldable

toDepths :: String -> List Int
toDepths text = mapMaybe fromString (toList $ split (Pattern "\n") text)

toDepthMeasurements :: List Int -> List Depth
toDepthMeasurements = go Nothing Nil
  where
  go :: Maybe Int -> List Depth -> List Int -> List Depth
  go previous measurements depths =
    case depths of
      Nil -> measurements
      Cons d ds -> case previous of
        Nothing -> go (Just d) (snoc measurements { measurement: d, change: NotChanged }) ds
        Just prev -> go (Just d) (snoc measurements { measurement: d, change: if d < prev then Decrease else Increase }) ds

countIncreases :: List Depth -> Int
countIncreases measurements = foldl (\acc curr -> if curr.change == Increase then acc + 1 else acc) 0 measurements

runEx1 :: Effect Unit
runEx1 = do
  fileContent <- getFileContent
  log $ show $ countIncreases $ toDepthMeasurements $ toDepths fileContent