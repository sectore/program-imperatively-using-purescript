module Game.Data where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

newtype Game = Game
    { score :: Int
    , units :: Array GameUnit
    , boss  :: GameUnit
    }

derive instance genericGame :: Generic Game _
instance showGame :: Show Game where
  show x = genericShow x

newtype GameUnit = GameUnit
    { health   :: Int
    , position :: GamePoint
    }

derive instance genericGameUnit :: Generic GameUnit _
instance showGameUnit :: Show GameUnit where
  show x = genericShow x

newtype GamePoint = GamePoint
    { x :: Number
    , y :: Number
    }

derive instance genericGamePoint :: Generic GamePoint _
instance showGamePoint :: Show GamePoint where
  show x = genericShow x
