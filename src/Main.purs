module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Ch1 (runCh1)
import Ch2 (runCh2)

main :: Effect Unit
main = do
  log "----------------"
  log "Challenge 1:"
  runCh1
  log "----------------"
  log "Challenge 2:"
  runCh2
  log "----------------"
