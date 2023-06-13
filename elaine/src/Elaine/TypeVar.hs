{-# LANGUAGE DeriveGeneric #-}

module Elaine.TypeVar where

import Data.Aeson (ToJSON)
import GHC.Generics
import Elaine.Ident (Ident)

data TypeVar = ImplicitVar Int | ExplicitVar Ident
  deriving (Show, Eq, Ord, Generic)

instance ToJSON TypeVar