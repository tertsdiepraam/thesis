{-# LANGUAGE DataKinds #-}
module Elaine.AST where

type Program = [Module]

type Ident = String
type Effect = String

data Module = Mod String [Declaration]
  deriving (Show, Eq)

modName :: Module -> String
modName (Mod x _) = x

data Visibility = Private | Public
  deriving (Show, Eq)

data Declaration = Declaration Visibility DeclarationType
  deriving (Show, Eq)

data DeclarationType
  = Import Ident
  | DecLet Ident Expr
  | DecType Ident [Constructor]
  | DecEffect Effect [OperationSignature]
  deriving (Show, Eq)

data OperationSignature = OperationSignature Ident [ValueType] ValueType
  deriving (Show, Eq)

data Elaboration = Elaboration Ident EffectRow [OperationClause]
  deriving (Show, Eq)

clauses :: Elaboration -> [OperationClause]
clauses (Elaboration _ _ cs) = cs

data Handler = Handler HandleReturn [OperationClause]
  deriving (Show, Eq)

data HandleReturn = HandleReturn Ident Expr
  deriving (Show, Eq)

data Function = Function [(Ident, ComputationType)] ComputationType Expr
  deriving (Show, Eq)

data EffectRow = Cons Effect EffectRow | Empty | Extend Ident
  deriving (Show, Eq)

data Constructor = Constructor Ident [ComputationType]
  deriving (Show, Eq)

data OperationClause = OperationClause Ident [Ident] Expr
  deriving (Show, Eq)

clauseName :: OperationClause -> Ident
clauseName (OperationClause name _ _) = name

data Expr
  = App Expr [Expr]
  | If Expr Expr Expr
  | Fn Function
  | Handle Expr Expr
  | Match Expr [MatchArm]
  | ImplicitElab Expr
  | Elab Expr Expr
  | Var Ident
  | Let Ident Expr Expr
  | Val Value
  deriving (Show, Eq)

data Value
  = Int Int
  | String String
  | Bool Bool
  | Lam [Ident] Expr
  | Hdl Handler
  | Elb Elaboration
  | Constant BuiltIn
  | Data String String [Expr]
  | Unit
  deriving (Show, Eq)

data BuiltIn = BuiltIn Ident ([Value] -> Value)

instance Show BuiltIn where
  show (BuiltIn x _) = "<built-in " ++ x ++ ">"

instance Eq BuiltIn where
  (BuiltIn x _) == (BuiltIn y _) = x == y

-- A match is as simple as possible:
--  - Only constructors can be matches
--  - All constructors must be present
data MatchArm = MatchArm Pattern Expr
  deriving (Show, Eq)

-- A pattern consisting of a constructor identifier and
-- and a list of variables to bind.
data Pattern = Pattern Ident [Ident]
  deriving (Show, Eq)

data ComputationType = ComputationType ValueType EffectRow
  deriving (Show, Eq)

data ValueType
  = TypeName Ident
  | ValueFunctionType FunctionType
  | UnitType
  deriving (Show, Eq)

data FunctionType = FunctionType [ComputationType] ComputationType
  deriving (Show, Eq)

data HandlerType = HandlerType ComputationType ComputationType
  deriving (Show, Eq)