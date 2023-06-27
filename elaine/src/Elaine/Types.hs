module Elaine.Types (DataType (..), Constructor (..), Path, Arrow (..), Row (..), TypeScheme (..), CompType (..), ValType (..), Effect (..), rowUpdate, rowMerge, rowInsert, rowVar, rowOpen, rowClosed, rowEmpty, rowIsEmpty, rowMaybe) where

import Data.Map (Map)
import Data.Maybe (isNothing)
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import Elaine.Ident (Ident)
import Elaine.TypeVar

type Path = [Ident]

data TypeScheme = TypeScheme
  { typeVars :: [TypeVar],
    effectVars :: [TypeVar],
    typ :: CompType
  }
  deriving (Show, Eq, Ord)

data CompType = CompType Row ValType
  deriving (Show, Eq, Ord)

data Effect = Effect Path (Map Ident Arrow)
  deriving (Show, Eq, Ord)

-- A custom data type, possibly with type vars
-- These vars are constant and do not change while typechecking
data DataType = DataType Path [TypeVar] [Constructor]
  deriving (Show, Eq, Ord)

data Constructor = Constructor Ident [CompType]
  deriving (Show, Eq, Ord)

data Arrow = Arrow [CompType] CompType
  deriving (Show, Eq, Ord)

data ValType
  = TypeInt
  | TypeString
  | TypeBool
  | TypeUnit
  | TypeV TypeVar
  | TypeArrow Arrow
  | TypeHandler Effect ValType ValType
  | TypeData DataType [ValType]
  | TypeTuple [ValType]
  | TypeElaboration Effect Row
  deriving (Show, Eq, Ord)

data Row = Row
  { rowEffects :: MultiSet Effect,
    rowExtension :: Maybe TypeVar
  }
  deriving (Show, Eq, Ord)

rowUpdate :: Row -> Row -> Row
rowUpdate (Row effsA _) (Row effsB exB) = Row (MultiSet.union effsA effsB) exB

rowMerge :: Row -> Row -> Row
rowMerge (Row effsA _) (Row effsB exB) = Row new exB
  where new = MultiSet.union effsA effsB MultiSet.\\ MultiSet.intersection effsA effsB

rowInsert :: Effect -> Row -> Row
rowInsert eff (Row effs ex) = Row (MultiSet.insert eff effs) ex

rowVar :: TypeVar -> Row
rowVar v = Row MultiSet.empty (Just v)

rowOpen :: [Effect] -> TypeVar -> Row
rowOpen e v = Row (MultiSet.fromList e) (Just v)

rowClosed :: [Effect] -> Row
rowClosed e = Row (MultiSet.fromList e) Nothing

rowEmpty :: Row
rowEmpty = Row MultiSet.empty Nothing

rowIsEmpty :: Row -> Bool
rowIsEmpty (Row a b) = MultiSet.null a && isNothing b

rowMaybe :: [Effect] -> Maybe TypeVar -> Row
rowMaybe effs = Row (MultiSet.fromList effs)
