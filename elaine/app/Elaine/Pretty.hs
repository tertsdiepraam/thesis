{-# LANGUAGE GADTs #-}
module Elaine.Pretty where

import Elaine.AST
import Data.List (intercalate)

-- Special typeclass for pretty printing the code
class Pretty a where
  pretty :: a -> String

pBlock :: String -> String
pBlock "" = "{}"
pBlock s = "{\n" ++ indent s ++ "}"

pParam :: (Ident, ComputationType) -> String
pParam (name, typ) = name ++ ": " ++ pretty typ

concatBlock :: Pretty a => [a] -> String
concatBlock as = pBlock $ unlines (map pretty as)

parens :: [String] -> String
parens x = "(" ++ intercalate ", " x ++ ")"

instance Pretty Declaration where
  pretty (Declaration Public decType) = "pub " ++ pretty decType
  pretty (Declaration Private decType) = pretty decType

instance Pretty DeclarationType where
  pretty (Import s) = "import " ++ s
  pretty (DecLet name expr) = "let " ++ name ++ " = " ++ pretty expr
  pretty (DecType name constructors) =
    "type " ++ name ++ " " ++ concatBlock constructors
  pretty (DecEffect name operations) =
    "effect " ++ name ++ " " ++ concatBlock operations

instance Pretty HandleReturn where
  pretty (HandleReturn var body) =
    "return(" ++ var ++ ") " ++ pBlock (pretty body)

instance Pretty OperationSignature where
  pretty (OperationSignature name args ret) =
    name ++ parens (map pretty args) ++ pretty ret

instance Pretty Function where
  pretty (Function params ret do') = "fn" ++ parens (map pParam params) ++ pretty ret ++ " " ++ pBlock (pretty do')

instance Pretty Module where
  pretty (Mod s decs) = "mod " ++ s ++ " " ++ concatBlock decs ++ "\n"

instance Pretty Expr where
  pretty (Let x e1 e2) = "let " ++ x ++ " = " ++ pretty e1 ++ "\n" ++ pretty e2
  pretty (If c e1 e2) = "if " ++ pretty c ++ " then " ++ pBlock (pretty e1) ++ " else " ++ pBlock (pretty e2)
  pretty (Fn function) = pretty function 
  pretty (App name params) = pretty name ++ "(" ++ intercalate ", " (map pretty params) ++ ")"
  -- pretty (Handle handler computation) = "handle " ++ pretty handler ++ " " ++ pretty computation
  pretty (Handle _ computation) = "handle ... " ++ pretty computation
  -- pretty (Elab e computation) = "elab[" ++ pretty e ++ "] " ++ pretty computation
  pretty (Elab _ computation) = "elab[...] " ++ pretty computation
  pretty (ImplicitElab e) = "elab " ++ pretty e
  pretty (Match e arms) = "match " ++ pretty e ++ " " ++ concatBlock arms
  pretty (Var var) = var
  pretty (Val v) = pretty v

instance Pretty Value where
  pretty (Int n) = show n
  pretty (String s) = show s
  pretty (Bool b) = if b then "true" else "false"
  pretty (Lam params body) = "fn" ++ parens params ++ pBlock (pretty body) 
  pretty (Hdl (Handler ret functions)) =
    "handler "
      ++ pBlock (unlines (pretty ret : map pretty functions))
  pretty (Elb (Elaboration from to clauses)) =
    "elaboration " ++ from ++ " -> " ++ pretty to ++ pBlock (unlines (map pretty clauses))
  pretty (Data type' variant args) = type' ++ "." ++ variant ++ "(" ++ concatMap pretty args ++ ")"
  pretty (Constant x) = pretty x
  pretty Unit = "()"

instance Pretty BuiltIn where
  pretty (BuiltIn name _) = "<|" ++ name ++ "|>"

instance Pretty MatchArm where
  pretty (MatchArm pat e) = pretty pat ++ " => " ++ pretty e

instance Pretty Pattern where
  pretty (Pattern name vars) = name ++ "(" ++ intercalate ", " vars ++ ")"

instance Pretty Program where
  pretty mods = intercalate "\n" $ map pretty mods

instance Pretty Constructor where
  pretty (Constructor name params) = name ++ parens (map pretty params)

instance Pretty OperationClause where
  pretty (OperationClause name args expr) = name ++ parens args ++ pBlock (pretty expr)

instance Pretty ComputationType where
  -- In the case where we have a function type and effects, we need to disambiguate that the effects belong
  -- outside the function with parentheses
  pretty (ComputationType valueType effs) = pretty effs ++ " " ++ pretty valueType

instance Pretty EffectRow where
  pretty row = "<" ++ prettyRow row ++ ">"
    where
      prettyRow Empty = ""
      prettyRow (Extend x) = "|" ++ x
      prettyRow (Cons x xs@(Cons _ _)) = x ++ ", " ++ prettyRow xs
      prettyRow (Cons x xs) = x ++ prettyRow xs

instance Pretty ValueType where
  pretty (TypeName name) = name
  pretty (ValueFunctionType sig) = pretty sig
  pretty UnitType = "()"

instance Pretty HandlerType where
  pretty (HandlerType from to) = pretty from ++ " -> " ++ pretty to

instance Pretty FunctionType where
  pretty (FunctionType params ret) = "(" ++ intercalate ", " (map pretty params) ++ ") -> " ++ pretty ret

indent :: String -> String
indent s = concatMap (\s' -> "  " ++ s' ++ "\n") $ lines s