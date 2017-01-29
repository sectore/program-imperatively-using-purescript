module Game where

import Prelude
import Game.Data.Lenses as L
import Control.Monad.State (State, get, put)
import Data.Lens (Lens', over, set, view)
import Game.Data (Game(..), GameUnit(..), GamePoint(..))

initialState :: Game
initialState = Game
    { score: 0
    , units:
        [ GameUnit
            { health: 10
            , position: GamePoint { x: 3.5, y: 7.0 }
            }
        , GameUnit
            { health: 15
            , position: GamePoint { x: 1.0, y: 1.0 }
            }
        , GameUnit
            { health: 8
            , position: GamePoint { x: 0.0, y: 2.1 }
            }
        ]
    , boss: GameUnit
        { health: 100
        , position: GamePoint { x: 0.0, y: 0.0 }
        }
    }


-- Intro (to warm up ;) ): set | view | over lenses


-- get score
getScore :: State Game Unit
getScore = do
    g <- get
    let s = view (L._Game <<< L.score) g
    pure unit

-- set score
setScore :: State Game Unit
setScore = do
    put <<< set (L._Game <<< L.score) 10000 =<< get
    pure unit

-- update score
updateScore :: State Game Unit
updateScore = do
    put <<< over (L._Game <<< L.score) (_ + 222) =<< get
    pure unit


-- Composition


-- update boss' health
strike :: State Game Unit
strike = do
    put <<< over (L._Game <<< L.boss <<< L._GameUnit <<< L.health) (_ + 33) =<< get
    pure unit

-- update boss' health using bossHP
strike' :: State Game Unit
strike' = do
    put <<< over bossHP (_ + 33) =<< get
    pure unit

-- composite lens to get health of boss
bossHP :: Lens' Game Int
bossHP =
  L._Game <<< L.boss <<< L._GameUnit <<< L.health
