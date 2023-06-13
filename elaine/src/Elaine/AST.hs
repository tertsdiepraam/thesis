{-# LANGUAGE DeriveGeneric #-}
module Elaine.AST where

import Elaine.TypeVar
import Elaine.Types (TypeScheme)
import Elaine.Ident (Ident (Ident))
import Data.Aeson hiding (Value)
import GHC.Generics (Generic)

type Program = [Declaration]

data Visibility = Private | Public
  deriving (Show, Eq, Generic)

instance ToJSON Visibility

data Declaration = Declaration Visibility DeclarationType
  deriving (Show, Eq, Generic)

instance ToJSON Declaration

data DeclarationType
  = Use Ident
  | Module Ident [Declaration]
  | DecLet Ident (Maybe ASTComputationType) Expr
  | DecType Ident [Constructor]
  | DecEffect Ident [OperationSignature]
  deriving (Show, Eq, Generic)

instance ToJSON DeclarationType

data OperationSignature = OperationSignature Ident [ASTComputationType] ASTComputationType
  deriving (Show, Eq, Generic)

instance ToJSON OperationSignature

data Elaboration = Elaboration Ident Row [OperationClause]
  deriving (Show, Eq, Generic)

instance ToJSON Elaboration

data Handler = Handler Function [OperationClause]
  deriving (Show, Eq, Generic)

instance ToJSON Handler

data Function = Function [(Ident, Maybe ASTComputationType)] (Maybe ASTComputationType) Expr
  deriving (Show, Eq, Generic)

instance ToJSON Function

lam :: [Ident] -> Expr -> Value
lam a = Fn . Function (zip a (repeat Nothing)) Nothing

data Constructor = Constructor Ident [ASTComputationType]
  deriving (Show, Eq, Generic)

instance ToJSON Constructor

data OperationClause = OperationClause Ident [Ident] Expr
  deriving (Show, Eq, Generic)

instance ToJSON OperationClause

clauseName :: OperationClause -> Ident
clauseName (OperationClause name _ _) = name

data Expr
  = App Expr [Expr]
  | If Expr Expr Expr
  | Handle Expr Expr
  | Match Expr [MatchArm]
  -- The integer is mapped to a unique identifier while type checking
  | ImplicitElab Int Expr
  | Elab Expr Expr
  | Var Ident
  | Let (Maybe Ident) (Maybe ASTComputationType) Expr Expr
  | Val Value
  deriving (Show, Eq, Generic)

instance ToJSON Expr

data Value
  = Int Int
  | String String
  | Bool Bool
  | Fn Function
  | Hdl Handler
  | Elb Elaboration
  | Constant BuiltIn
  | Data Ident Ident [Expr]
  | Unit
  deriving (Show, Eq, Generic)


instance ToJSON Value

data BuiltIn = BuiltIn Ident TypeScheme ([Value] -> Value)
  deriving (Generic)

instance ToJSON BuiltIn where
    toJSON (BuiltIn name scheme _) = object ["name" .= toJSON name, "scheme" .= toJSON scheme]


instance Show BuiltIn where
  show (BuiltIn (Ident x _) _ _) = "<built-in " ++ x ++ ">"

instance Eq BuiltIn where
  (BuiltIn x _ _) == (BuiltIn y _  _) = x == y

-- A match is as simple as possible:
--  - Only constructors can be matches
--  - All constructors must be present
data MatchArm = MatchArm Pattern Expr
  deriving (Show, Eq, Generic)

instance ToJSON MatchArm

-- A pattern consisting of a constructor identifier and
-- and a list of variables to bind.
data Pattern = Pattern Ident [Ident]
  deriving (Show, Eq, Generic)

instance ToJSON Pattern

data Row = Row [Ident] (Maybe Ident)
  deriving (Show, Eq, Generic)

instance ToJSON Row

data ASTComputationType = ASTComputationType Row ASTValueType
  deriving (Show, Eq, Generic)

instance ToJSON ASTComputationType


data ASTValueType
  = TypeName Ident
  | TypeUnit
  | TypeArrow [ASTComputationType] ASTComputationType
  | TypeHandler Ident TypeVar ASTValueType
  | TypeElaboration Ident Row
  deriving (Show, Eq, Generic)

instance ToJSON ASTValueType