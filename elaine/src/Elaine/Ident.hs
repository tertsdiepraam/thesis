module Elaine.Ident (Ident (Ident, idText), Location(LocOffset, LocBuiltIn, LocNone)) where

data Ident = Ident
  { idText :: String,
    location :: Location
  }
  deriving (Show)

data Location = LocOffset Int | LocBuiltIn | LocNone
  deriving (Show)

instance Ord Ident where
  a <= b = idText a <= idText b

instance Eq Ident where
  a == b = idText a == idText b