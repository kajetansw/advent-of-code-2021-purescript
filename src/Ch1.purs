module Ch1 (runCh1) where

import Prelude

import Data.List (List(..), fromFoldable, mapMaybe, snoc, (:), length, singleton)
import Data.Tuple.Nested (Tuple3(..), tuple3, (/\))
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

sumTuple3 :: Tuple3 Int Int Int -> Int
sumTuple3 (x0 /\ x1 /\ x2 /\ _) = x0 + x1 + x2

toGroupsOfThree :: forall a. List a -> List (Tuple3 a a a)
toGroupsOfThree (x0 : (x1 : (x2 : xs))) = (tuple3 x0 x1 x2) : (toGroupsOfThree (x1 : (x2 : xs)))
toGroupsOfThree (x : xs) = toGroupsOfThree xs
toGroupsOfThree Nil = Nil

toWindowedDepths :: List Int -> List Int
toWindowedDepths list = map (\x -> sumTuple3 x) (toGroupsOfThree list)

toDepthMeasurements :: List Int -> List Depth
toDepthMeasurements = go Nothing Nil
  where
  go :: Maybe Int -> List Depth -> List Int -> List Depth
  go previous measurements depths =
    case depths of
      Nil -> measurements
      Cons d ds -> case previous of
        Nothing -> go (Just d) (snoc measurements { measurement: d, change: NotChanged }) ds
        Just prev -> go (Just d) (snoc measurements { measurement: d, change: if d > prev then Increase else Decrease }) ds

countIncreases :: List Depth -> Int
countIncreases = foldl (\acc curr -> if curr.change == Increase then acc + 1 else acc) 0

runCh1 :: Effect Unit
runCh1 = do
  fileContent <- getFileContent
  log "Part 1:"
  log $ show $ countIncreases $ toDepthMeasurements $ toDepths fileContent
  log "Part 2:"
  log $ show $ countIncreases $ toDepthMeasurements $ toWindowedDepths $ toDepths fileContent
