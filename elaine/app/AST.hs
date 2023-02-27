module AST where

type Program = [Module]

type Ident = String

data Module = Mod String [Declaration]
  deriving (Show)

data Declaration
  = Import Ident
  | Fun Function
  | TypeDec Ident [Constructor]
  | Effect EffectType [Operation]
  | Handler Ident [Function]
  | Elaboration Ident [Function]
  deriving (Show)

data Function = Function Ident [(Ident, Type)] Type Do
  deriving (Show)

data EffectType = Algebraic Ident | HigherOrder Ident
  deriving (Show)

data Do
  = Do Let Do
  | Pure Expr
  deriving (Show)

data Constructor = Constructor Ident [(Ident, Type)]
  deriving (Show)

data Operation = Operation Ident [(Ident, Type)] Type
  deriving (Show)

data Let = Let Ident Expr
  deriving (Show)

data Lit
  = Int Int
  | String String
  | Bool Bool
  deriving (Show)

data Leaf = Lit Lit | Var Ident
  deriving (Show)

data Expr
  = App Ident [Leaf]
  | Match Leaf [MatchArm]
  | Leaf Leaf
  deriving (Show)

-- A match is as simple as possible:
--  - Only constructors can be matches
--  - All constructors must be present
data MatchArm = MatchArm Pattern Expr
  deriving (Show)

-- A pattern consisting of a constructor identifier and
-- and a list of variables to bind.
data Pattern = Pattern Ident [Ident]
  deriving (Show)

data Type = Type Ident [EffectType]
  deriving (Show)
