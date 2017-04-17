module Game where

import Prelude
import Game.Lenses as L
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.State (State, get, put)
import Control.Monad.State.Trans (StateT, lift)
import Data.Lens (Lens', Traversal', filtered, over, set, view, (^.))
import Data.Lens.Traversal (traversed)
import Data.Lens.Zoom (zoom)
import Data.List.Lazy (replicateM)
import Data.Profunctor.Choice (class Choice)
import Data.Traversable (for)
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


-- Intro (just to warm up ;) ): set | view | over lenses


-- get score
getScore :: State Game Unit
getScore = do
    g <- get
    let s = view (L._Game <<< L.score) g
    --
    -- OR using ^. operator
    -- g <- get
    -- let s = g ^. L._Game <<< L.score
    --
    -- OR using ^. operatore and deconstructing Game
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
strike :: forall e. StateT Game (Eff (console :: CONSOLE | e)) Unit
strike = do
    lift $ log "*shink*"
    put <<< over (L._Game <<< L.boss <<< L._GameUnit <<< L.health) (_ + 33) =<< get
    pure unit

-- update boss' health using bossHP
strike' :: forall e. StateT Game (Eff (console :: CONSOLE | e)) Unit
strike' = do
    lift $ log "*shink*"
    put <<< over bossHP (_ + 33) =<< get
    pure unit

-- composite lens to get health of boss
bossHP :: Lens' Game Int
bossHP =
  L._Game <<< L.boss <<< L._GameUnit <<< L.health



-- Traversal

fireBreath :: forall e. StateT Game (Eff (console :: CONSOLE | e)) Unit
fireBreath = do
    lift $ log "*srawr*"
    put <<< over partyHP (_ - 3) =<< get
    pure unit

partyHP :: Traversal' Game Int
partyHP =
  L._Game <<< L.units <<< traversed <<< L._GameUnit <<< L.health

fireBreath' :: forall e. GamePoint -> StateT Game (Eff (console :: CONSOLE | e)) Unit
fireBreath' target = do
    lift $ log "*srawr*"
    put <<< over (L._Game <<< L.units <<< traversed <<< (around target 1.0) <<< L._GameUnit <<< L.health) (_ - 3) =<< get
    pure unit

around :: forall p. Choice p => GamePoint -> Number -> p GameUnit GameUnit -> p GameUnit GameUnit
around center radius = filtered (\unit ->
    (pow (diffX unit center) 2.0) + (pow (diffY unit center) 2.0) < (pow radius 2.0))
    where
      diffX (GameUnit u) (GamePoint p) =
        (u ^. L.position <<< L._GamePoint <<< L.x) - (p ^. L.x)
      diffY (GameUnit u) (GamePoint p) =
        (u ^. L.position <<< L._GamePoint <<< L.y) - (p ^. L.y)

-- Zooming

partyLoc :: Traversal' Game GamePoint
partyLoc = L._Game <<< L.units <<< traversed <<< L._GameUnit <<< L.position

-- retreat :: StateT Game Identity Unit
retreat :: forall e. StateT Game (Eff (console :: CONSOLE | e)) Unit
retreat = do
    lift $ log "Retreat!"
    zoom (partyLoc <<< L._GamePoint) $
      put <<< over L.x (_ + 10.0) <<< over L.y (_ + 10.0) =<< get
    pure unit


-- Combining commands

battle :: forall e. StateT Game (Eff (console :: CONSOLE | e)) Unit
battle = do
    -- Charge!
    _ <- for ["Take that!", "and that!", "and that!"] $
            \taunt -> do
                lift $ log taunt
                strike
    -- The dragon awakes!
    fireBreath' $ GamePoint {x: 0.5, y:1.5}

    _ <- replicateM 3 $ do
            -- The better part of valor
            retreat
            -- Boss chases them
            zoom (partyLoc <<< L._GamePoint) $
              put <<< over L.x (_ + 10.0) <<< over L.y (_ + 10.0) =<< get

    pure unit
