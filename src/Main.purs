module Main where

import Prelude
import Game.Data.Lenses as L
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.State (execState)
import Control.Monad.State.Trans (execStateT)
import Data.Lens ((^..), view)
import Data.Newtype (unwrap)
import Game (fireBreath, fireBreath', initialState, partyLoc, retreat, setScore, strike, strike', updateScore)
import Game.Data (GamePoint(..))

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  chapter "Getter / Setter"
  label "get score"
  logShow $ view (L._Game <<< L.score) initialState
  label "set score"
  logShow $ execState setScore initialState
  label "update score"
  logShow $ execState updateScore initialState
  chapter "Composition"
  label "strike"
  logShow $ execState strike initialState
  label "strike'"
  logShow $ execState strike' initialState
  chapter "Traversal"
  label "fireBreath"
  logShow $ execState fireBreath initialState
  label "fireBreath'"
  logShow $ execState (fireBreath' $ GamePoint {x: 0.5, y:1.5} ) initialState
  chapter "Zooming"
  label "partyLoc"
  logShow $ initialState ^.. partyLoc
  label "retreat"
  logShow $ execStateT retreat initialState
  label "retreat newstate"
  let newState = unwrap $ execStateT retreat initialState
  logShow $ newState ^.. partyLoc

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
