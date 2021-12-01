module Ex1 (runEx1) where

import Prelude

import Effect (Effect)
import Effect.Console (log)

runEx1 :: Effect Unit
runEx1 = do
  log "test1"