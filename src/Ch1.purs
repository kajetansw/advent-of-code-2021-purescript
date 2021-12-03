module Ch1 (runCh1) where

import Prelude

import Data.List (List(..), fromFoldable, mapMaybe, snoc, (:), length)
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

sum :: List Int -> Int
sum = foldl (\x y -> x + y) 0

-- TODO how to prevent from using non-positive integer as input?
nwise :: forall a. Int -> List a -> List (List a)
nwise = go { lastElem: Nil, result: Nil }
  where
  go :: forall b. { lastElem :: List b, result :: List (List b) } -> Int -> List b -> List (List b)
  go { lastElem, result } _ Nil = snoc result lastElem
  go { lastElem: Nil, result } maxCount (x : xs) = go { lastElem: snoc Nil x, result: result } maxCount xs
  go { lastElem: lastElem@(_ : les), result } maxCount (x : xs)
    | (length lastElem) < maxCount = go { lastElem: snoc lastElem x, result: result } maxCount xs
    | otherwise = go { lastElem: snoc les x, result: snoc result lastElem } maxCount xs

toFramedDepths :: List Int -> List Int
toFramedDepths list = map (\x -> sum x) (nwise 3 list)

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
  log $ show $ countIncreases $ toDepthMeasurements $ toFramedDepths $ toDepths fileContent
