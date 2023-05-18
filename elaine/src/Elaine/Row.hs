module Elaine.Row where

import Elaine.TypeVar
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet

type Effect = String

data Row = Row {
  effects :: MultiSet Effect,
  extend :: Maybe TypeVar
}
  deriving (Show, Eq, Ord)

class IntoEffects a where
    intoEffects :: a -> MultiSet Effect

instance IntoEffects Effect where
    intoEffects = MultiSet.singleton

instance IntoEffects [Effect] where
    intoEffects = MultiSet.fromList

instance IntoEffects (MultiSet Effect) where
    intoEffects = id
 
update :: Row -> Row -> Row
update (Row effsA _) (Row effsB exB) = Row (MultiSet.union effsA effsB) exB

var :: TypeVar -> Row
var v = Row MultiSet.empty (Just v)

open :: IntoEffects a => a -> TypeVar -> Row
open e v = Row (intoEffects e) (Just v) 

closed :: IntoEffects a => a -> Row
closed e = Row (intoEffects e) Nothing

empty :: Row
empty = Row MultiSet.empty Nothing
