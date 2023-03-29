{-# LANGUAGE DataKinds #-}
module Elaine.AST where

type Program = [Module]

type Ident = String

data Module = Mod String [Declaration]
  deriving (Show)

data Visibility = Private | Public
  deriving (Show)

data Declaration = Declaration Visibility DeclarationType
  deriving (Show)

data DeclarationType
  = Import Ident
  | DecLet Ident Expr
  | DecType Ident [Constructor]
  | DecEffect Effect [OperationSignature]
  | DecElaboration Elaboration
  deriving (Show)

data OperationSignature = OperationSignature Ident [ValueType] ValueType
  deriving (Show)

data Elaboration = Elaboration Ident EffectRow [OperationClause]
  deriving (Show)

data Handler = Handler HandleReturn [OperationClause]
  deriving (Show)

data HandleReturn = HandleReturn Ident Expr
  deriving (Show)

data Function = Function [(Ident, ComputationType)] ComputationType Expr
  deriving (Show)

data Effect = Algebraic Ident | HigherOrder Ident
  deriving (Show)

data EffectRow = Cons Effect EffectRow | Empty | Extend Ident
  deriving (Show)

data Constructor = Constructor Ident [ComputationType]
  deriving (Show)

data OperationClause = OperationClause Ident [Ident] Expr
  deriving (Show)

data Expr
  = App Expr [Expr]
  | If Expr Expr Expr
  | Fn Function
  | Handle Expr Expr
  | Elab Expr
  | Var Ident
  | Let Ident Expr Expr
  | Val Value
  deriving (Show)

data Value
  = Int Int
  | String String
  | Bool Bool
  | Lam [Ident] Expr
  | Hdl Handler
  | Unit
  deriving (Show)

-- We define value equality by hand, because equality for lambdas and
-- handlers does not make sense.
instance Eq Value where
  (Int a) == (Int b) = a == b
  (String a) == (String b) = a == b
  (Bool a) == (Bool b) = a == b
  Unit == Unit = True
  _ == _ = False

-- A match is as simple as possible:
--  - Only constructors can be matches
--  - All constructors must be present
data MatchArm = MatchArm Pattern Expr
  deriving (Show)

-- A pattern consisting of a constructor identifier and
-- and a list of variables to bind.
data Pattern = Pattern Ident [Ident]
  deriving (Show)

data ComputationType = ComputationType ValueType EffectRow
  deriving (Show)

data ValueType
  = TypeName Ident
  | ValueFunctionType FunctionType
  | UnitType
  deriving (Show)

data FunctionType = FunctionType [ComputationType] ComputationType
  deriving (Show)

data HandlerType = HandlerType ComputationType ComputationType
  deriving (Show)