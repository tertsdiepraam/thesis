module Elaine.TypeVar where

data TypeVar = ImplicitVar Int | ExplicitVar String
  deriving (Show, Eq, Ord)