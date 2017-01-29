module Game.Data.Lenses where

import Prelude as Prelude
import Data.Lens as Lens
import Data.Either as Either
import Game.Data


score :: forall a b r. Lens.Lens { "score" :: a | r } { "score" :: b | r } a b
score = Lens.lens _."score" (_ { "score" = _ })

units :: forall a b r. Lens.Lens { "units" :: a | r } { "units" :: b | r } a b
units = Lens.lens _."units" (_ { "units" = _ })

boss :: forall a b r. Lens.Lens { "boss" :: a | r } { "boss" :: b | r } a b
boss = Lens.lens _."boss" (_ { "boss" = _ })

_Game :: Lens.Iso' Game
           { "score" :: Int
           , "units" :: Array GameUnit
           , "boss" :: GameUnit
           }
_Game = Lens.iso unwrap Game
  where
    unwrap (Game x) = x

health :: forall a b r. Lens.Lens { "health" :: a | r } { "health" :: b | r } a b
health = Lens.lens _."health" (_ { "health" = _ })

position :: forall a b r. Lens.Lens { "position" :: a | r } { "position" :: b | r } a b
position = Lens.lens _."position" (_ { "position" = _ })

_GameUnit :: Lens.Iso' GameUnit
               { "health" :: Int
               , "position" :: GamePoint
               }
_GameUnit = Lens.iso unwrap GameUnit
  where
    unwrap (GameUnit x) = x

x :: forall a b r. Lens.Lens { "x" :: a | r } { "x" :: b | r } a b
x = Lens.lens _."x" (_ { "x" = _ })

y :: forall a b r. Lens.Lens { "y" :: a | r } { "y" :: b | r } a b
y = Lens.lens _."y" (_ { "y" = _ })

_GamePoint :: Lens.Iso' GamePoint
                { "x" :: Number
                , "y" :: Number
                }
_GamePoint = Lens.iso unwrap GamePoint
  where
    unwrap (GamePoint x) = x
