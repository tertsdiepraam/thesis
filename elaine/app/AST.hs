module AST where

type Program = [Module]

type Ident = String

data Module = Mod String [Declaration]
  deriving (Show)

data Declaration
  = Import Ident
  | DecFun Function
  | DecType Ident TypeParams [Constructor]
  | DecEffect Effect [Operation]
  | DecHandler Handler
  | DecElaboration Elaboration
  deriving (Show)

data Elaboration = Elaboration Ident HandlerType [Function]
  deriving (Show)

data Handler = Handler Ident HandlerType [Function]
  deriving (Show)

data Function = Function Ident FunSig Do
  deriving (Show)

data FunSig = FunSig TypeParams [(Ident, ComputationType)] ComputationType
  deriving (Show)

data Effect = Algebraic Ident | HigherOrder Ident
  deriving (Show)

data Do
  = Do Let Do
  | Pure Expr
  deriving (Show)

data Constructor = Constructor Ident [ComputationType]
  deriving (Show)

data Operation = Operation Ident FunSig
  deriving (Show)

data Let = Let Ident Expr
  deriving (Show)

newtype TypeParams = TypeParams [Ident]
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

data ComputationType = ComputationType ValueType [Effect]
  deriving (Show)

data ValueType
  = TypeName Ident TypeParams
  | ValueFunctionType FunctionType
  deriving (Show)

data FunctionType = FunctionType TypeParams [ComputationType] ComputationType
  deriving (Show)

data HandlerType = HandlerType ComputationType ComputationType
  deriving (Show)