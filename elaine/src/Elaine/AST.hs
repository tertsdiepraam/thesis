module Elaine.AST where

type Program = [Declaration]

type Ident = String
type Effect = String

data Visibility = Private | Public
  deriving (Show, Eq)

data Declaration = Declaration Visibility DeclarationType
  deriving (Show, Eq)

data DeclarationType
  = Use Ident
  | Module String [Declaration]
  | DecLet Ident (Maybe ComputationType) Expr
  | DecType Ident [Constructor]
  | DecEffect Effect [OperationSignature]
  deriving (Show, Eq)

data OperationSignature = OperationSignature Ident [ValueType] ValueType
  deriving (Show, Eq)

data Elaboration = Elaboration Ident EffectRow [OperationClause]
  deriving (Show, Eq)

data Handler = Handler Function [OperationClause]
  deriving (Show, Eq)

data Function = Function [(Ident, Maybe ComputationType)] (Maybe ComputationType) Expr
  deriving (Show, Eq)

lam :: [Ident] -> Expr -> Value
lam a = Fn . Function (zip a (repeat Nothing)) Nothing

data EffectRow = Cons Effect EffectRow | Empty | Extend TypeVar
  deriving (Show, Eq, Ord)

data Constructor = Constructor Ident [ComputationType]
  deriving (Show, Eq)

data OperationClause = OperationClause Ident [Ident] Expr
  deriving (Show, Eq)

clauseName :: OperationClause -> Ident
clauseName (OperationClause name _ _) = name

data Expr
  = App Expr [Expr]
  | If Expr Expr Expr
  | Handle Expr Expr
  | Match Expr [MatchArm]
  | ImplicitElab Expr
  | Elab Expr Expr
  | Var Ident
  | Let Ident (Maybe ComputationType) Expr Expr
  | Val Value
  deriving (Show, Eq)

data Value
  = Int Int
  | String String
  | Bool Bool
  | Fn Function
  | Hdl Handler
  | Elb Elaboration
  | Constant BuiltIn
  | Data String String [Expr]
  | Unit
  deriving (Show, Eq)

data BuiltIn = BuiltIn Ident TypeScheme ([Value] -> Value)

instance Show BuiltIn where
  show (BuiltIn x _ _) = "<built-in " ++ x ++ ">"

instance Eq BuiltIn where
  (BuiltIn x _ _) == (BuiltIn y _  _) = x == y

-- A match is as simple as possible:
--  - Only constructors can be matches
--  - All constructors must be present
data MatchArm = MatchArm Pattern Expr
  deriving (Show, Eq)

-- A pattern consisting of a constructor identifier and
-- and a list of variables to bind.
data Pattern = Pattern Ident [Ident]
  deriving (Show, Eq)

data ComputationType = ComputationType EffectRow ValueType
  deriving (Show, Eq, Ord)

data TypeScheme = TypeScheme {
  typeVars :: [TypeVar],
  effectVars :: [TypeVar],
  typ :: ComputationType
}
  deriving (Show, Eq, Ord)

data TypeVar = ImplicitVar Int | ExplicitVar String
  deriving (Show, Eq, Ord)

data ValueType
  = TypeInt
  | TypeString
  | TypeBool
  | TypeUnit
  | TypeName String
  | TypeVar TypeVar
  | TypeArrow [ComputationType] ComputationType
  | TypeHandler Effect TypeVar ValueType
  deriving (Show, Eq, Ord)