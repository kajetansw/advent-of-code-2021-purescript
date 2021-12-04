module Ch2 (runCh2) where

import Prelude

import Data.Foldable (foldl)
import Data.Int (fromString)
import Data.List (List, fromFoldable, mapMaybe, (:))
import Data.Maybe (Maybe(..))
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding as NE
import Node.FS.Sync (readTextFile)

type Distance =
  { horizontal :: Int
  , vertical :: Int
  }

data Course = Forward Int | Down Int | Up Int

-- TODO refactor to utils module
getFileContent :: Effect String
getFileContent = readTextFile NE.UTF8 "static/ch2.txt"

toList :: forall a. Array a -> List a
toList = fromFoldable

toCourseStringList :: String -> List String
toCourseStringList text = toList $ split (Pattern "\n") text

toCourses :: List String -> List Course
toCourses = mapMaybe f
  where
  f :: String -> Maybe Course
  f line =
    let
      lineSplit :: List String
      lineSplit = toList $ split (Pattern " ") line
    in
      case lineSplit of
        ("forward" : (units : _)) -> map (\a -> Forward a) (fromString units)
        ("down" : (units : _)) -> map (\a -> Down a) (fromString units)
        ("up" : (units : _)) -> map (\a -> Up a) (fromString units)
        _ -> Nothing

toDistance :: List Course -> Distance
toDistance = foldl f { horizontal: 0, vertical: 0 }
  where
  f :: Distance -> Course -> Distance
  f dist course = case course of
    Forward d -> dist { horizontal = dist.horizontal + d }
    Up d -> dist { vertical = dist.vertical - d }
    Down d -> dist { vertical = dist.vertical + d }

toResult :: Distance -> Int
toResult d = d.horizontal * d.vertical

runCh2 :: Effect Unit
runCh2 = do
  fileContent <- getFileContent
  fileContent
    # toCourseStringList
    # toCourses
    # toDistance
    # toResult
    # show
    # log
