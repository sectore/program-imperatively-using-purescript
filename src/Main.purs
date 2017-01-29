module Main where

import Prelude
import Game.Data.Lenses as L
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.State (execState)
import Data.Lens (view)
import Game (fireBreath, initialState, setScore, strike, strike', updateScore)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  logDelimiter
  log "get score "
  logShow $ view (L._Game <<< L.score) initialState
  logDelimiter
  log "set score "
  logShow $ execState setScore initialState
  log "update score "
  logShow $ execState updateScore initialState
  logDelimiter
  log "strike "
  logShow $ execState strike initialState
  logDelimiter
  log "strike' "
  logShow $ execState strike' initialState
  logDelimiter
  log "fireBreath "
  logShow $ execState fireBreath initialState
  logDelimiter

logDelimiter :: forall e. Eff (console :: CONSOLE | e) Unit
logDelimiter =
  log "------------------"
