module Game where

import Prelude
import Game.Data.Lenses as L
import Control.Monad.State (State, get, put)
import Data.Array (filter)
import Data.Lens (Lens', Traversal', over, set, view, (^.))
import Data.Lens.Traversal (traversed)
import Game.Data (Game(..), GameUnit(..), GamePoint(..))
import Math (pow)

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
    --
    -- OR using ^. operator
    --
    -- g <- get
    -- let s = g ^. L._Game <<< L.score
    --
    -- OR using ^. operatore and deconstructing Game
    --
    -- Game g <- get
    -- let s = g ^. L.score
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



-- Traversal

fireBreath :: State Game Unit
fireBreath = do
    put <<< over partyHP (_ - 3) =<< get
    pure unit

partyHP :: Traversal' Game Int
partyHP =
  L._Game <<< L.units <<< traversed <<< L._GameUnit <<< L.health


-- fireBreath' :: GamePoint -> State Game Unit
-- fireBreath' target = do
--     put <<< over L._Game <<< L.units <<< traversed <<< (around target 1.0) <<<
--         L._GameUnit <<< L.health (_ - 3) =<< get
--     pure unit
--
-- -- around :: GamePoint -> Number -> Traversal' GameUnit Unit
-- around center radius = filter (\unit ->
--     (pow (diffX unit center) 2.0) + (pow (diffY unit center) 2.0) < pow radius 2.0 )
--     where
--       diffX (GameUnit u) (GamePoint p) =
--         (u ^. L.position <<< L._GamePoint <<< L.x) - (p ^. L.x)
--       diffY (GameUnit u) (GamePoint p) =
--         (u ^. L.position <<< L._GamePoint <<< L.y) - (p ^. L.y)
