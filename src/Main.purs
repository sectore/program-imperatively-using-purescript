module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.State (execState)
import Control.Monad.State.Trans (execStateT)
import Data.Lens ((^..))
import Game (battle, fireBreath, fireBreath', getScore, initialState, partyLoc, retreat, setScore, strike, strike', updateScore)
import Game.Data (GamePoint(..))

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  chapter "Getter / Setter"
  label "get score"
  logShow $ execState getScore initialState
  label "set score"
  logShow $ execState setScore initialState
  label "update score"
  logShow $ execState updateScore initialState
  chapter "Composition"
  label "strike"
  logShow =<< execStateT strike initialState
  label "strike'"
  logShow =<< execStateT strike' initialState
  chapter "Traversal"
  label "fireBreath"
  logShow =<< execStateT fireBreath initialState
  label "fireBreath'"
  logShow =<< execStateT (fireBreath' $ GamePoint {x: 0.5, y:1.5}) initialState
  chapter "Zooming"
  label "partyLoc"
  logShow $ initialState ^.. partyLoc
  label "retreat"
  logShow =<< execStateT retreat initialState
  label "retreat newstate"
  execStateT retreat initialState >>= \st -> logShow $ st ^.. partyLoc
  chapter "Combining"
  label "battle"
  logShow =<< execStateT battle initialState

label :: forall e. String -> Eff (console :: CONSOLE | e) Unit
label text = do
  log ""
  log $ "# " <> text

chapter :: forall e. String -> Eff (console :: CONSOLE | e) Unit
chapter title = do
  log ""
  log "~~~~~~~~~~~~~~~~~~"
  log title
  log "~~~~~~~~~~~~~~~~~~"
