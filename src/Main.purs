module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Ex1 (runEx1)

main :: Effect Unit
main = do
  log "--------------"
  log "Exercise 1"
  log "--------------"
  runEx1
