module Elaine.Pretty where

import Data.List (intercalate)
import qualified Data.MultiSet as MS
import Elaine.AST
import Elaine.Ident (Ident (Ident))
import Elaine.TypeVar (TypeVar (ExplicitVar, ImplicitVar))
import Elaine.Types (CompType (CompType), Effect (..), Path, DataType (..))
import qualified Elaine.Types as T
import qualified Elaine.Types as Types

-- Special typeclass for pretty printing the code
class Pretty a where
  pretty :: a -> String

pBlock :: String -> String
pBlock "" = "{}"
pBlock s = "{\n" ++ indent s ++ "}"

pParam :: (Ident, Maybe ASTComputationType) -> String
pParam (name, typ') = pretty name ++ maybe "" ((": " ++) . pretty) typ'

concatBlock :: Pretty a => [a] -> String
concatBlock as = pBlock $ unlines (map pretty as)

parens :: [String] -> String
parens x = "(" ++ intercalate ", " x ++ ")"

optionalType :: Pretty a => Maybe a -> String
optionalType (Just a) = ":" ++ pretty a
optionalType Nothing = ""

pTypeVars :: Pretty a => [a] -> String
pTypeVars [] = ""
pTypeVars vs = "[" ++ intercalate "," (map pretty vs) ++ "]"

instance Pretty Ident where
  pretty (Ident x _) = x

instance Pretty Declaration where
  pretty (Declaration Public decType) = "pub " ++ pretty decType
  pretty (Declaration Private decType) = pretty decType

instance Pretty DeclarationType where
  pretty (Use s) = "use " ++ pretty s ++ ";"
  pretty (Module x decs) = "mod " ++ pretty x ++ concatBlock decs
  pretty (DecLet name t expr) = "let " ++ pretty name ++ optionalType t ++ " = " ++ pretty expr ++ ";"
  pretty (DecType name params constructors) =
    "type " ++ pretty name ++ pTypeVars params ++ " " ++ concatBlock constructors
  pretty (DecEffect name operations) =
    "effect " ++ pretty name ++ " " ++ concatBlock operations

instance Pretty OperationSignature where
  pretty (OperationSignature name args ret) =
    pretty name ++ parens (map pretty args) ++ " " ++ pretty ret

instance Pretty Function where
  pretty (Function params ret do') = parens (map pParam params) ++ " " ++ maybe "" pretty ret ++ " " ++ pBlock (pretty do')

instance Pretty Expr where
  pretty (Let x t e1 e2) = case x of
    Just ident -> "let " ++ pretty ident ++ optionalType t ++ " = " ++ pretty e1 ++ ";\n" ++ pretty e2
    Nothing -> pretty e1 ++ ";\n" ++ pretty e2
  pretty (If c e1 e2) = "if " ++ pretty c ++ " then " ++ pBlock (pretty e1) ++ " else " ++ pBlock (pretty e2)
  pretty (App name params) = pretty name ++ "(" ++ intercalate ", " (map pretty params) ++ ")"
  -- pretty (Handle handler computation) = "handle " ++ pretty handler ++ " " ++ pretty computation
  pretty (Handle h computation) = "handle[" ++ pretty h ++ "] " ++ pretty computation
  -- pretty (Elab e computation) = "elab[" ++ pretty e ++ "] " ++ pretty computation
  pretty (Elab e computation) = "elab[" ++ pretty e ++ "] " ++ pretty computation
  pretty (ImplicitElab _ e) = "elab " ++ pretty e
  pretty (Match e arms) = "match " ++ pretty e ++ " " ++ concatBlock arms
  pretty (Var var) = pretty var
  pretty (Val v) = pretty v

instance Pretty Value where
  pretty (Int n) = show n
  pretty (String s) = show s
  pretty (Bool b) = if b then "true" else "false"
  pretty (Fn function) = "fn" ++ pretty function
  pretty (Hdl (Handler ret functions)) =
    "handler "
      ++ pBlock (pRet ++ unlines (map pretty functions))
    where
      pRet = case ret of
        Just r -> "return" ++ pretty r ++ "\n"
        Nothing -> ""
  pretty (Elb (Elaboration from to clauses)) =
    "elaboration " ++ pretty from ++ " -> " ++ pretty to ++ " " ++ pBlock (unlines (map pretty clauses))
  pretty (Data type' variant args) = pretty type' ++ "::" ++ pretty variant ++ "(" ++ concatMap pretty args ++ ")"
  pretty (Constant x) = pretty x
  pretty Unit = "()"

instance Pretty BuiltIn where
  pretty (BuiltIn name _ _) = "<|" ++ pretty name ++ "|>"

instance Pretty MatchArm where
  pretty (MatchArm pat e) = pretty pat ++ " => " ++ pretty e

instance Pretty Pattern where
  pretty (Pattern name vars) =
    pretty name
      ++ "("
      ++ intercalate ", " (map pretty vars)
      ++ ")"

instance Pretty Program where
  pretty mods = intercalate "\n" $ map pretty mods

instance Pretty Constructor where
  pretty (Constructor name params) = pretty name ++ parens (map pretty params)

instance Pretty OperationClause where
  pretty (OperationClause name args expr) =
    pretty name
      ++ parens (map pretty args)
      ++ pBlock (pretty expr)

instance Pretty ASTComputationType where
  -- In the case where we have a function type and effects, we need to disambiguate that the effects belong
  -- outside the function with parentheses
  pretty (ASTComputationType effs valueType) = pretty effs ++ " " ++ pretty valueType

instance Pretty Row where
  pretty row = "<" ++ prettyRow row ++ ">"
    where
      prettyRow (Row effects extend) =
        intercalate
          ", "
          (map pretty effects)
          ++ maybe "" (("|" ++) . pretty) extend

instance Pretty TypeVar where
  pretty (ExplicitVar s) = pretty s
  pretty (ImplicitVar i) = "#" ++ show i

instance Pretty ASTValueType where
  pretty TypeUnit = "()"
  pretty (TypeConstructor name params) =
    pretty name
      ++ if not (null params) then "[" ++ intercalate "," (map pretty params) ++ "]" else ""
  pretty (TypeArrow args ret) = "(" ++ intercalate ", " (map pretty args) ++ ") -> " ++ pretty ret
  pretty (TypeHandler eff from to) = "handler[" ++ pretty eff ++ "," ++ pretty from ++ "," ++ pretty to ++ "]"
  pretty (TypeElaboration eff row) = "elaboration[" ++ pretty eff ++ "," ++ pretty row ++ "]"
  pretty (TypeTuple params) = "(" ++ intercalate "," (map pretty params) ++ ")"

instance Pretty Types.Row where
  pretty (Types.Row effs ext) = "<" ++ intercalate ", " (map (intercalate "::" . p) (MS.toList effs)) ++ maybe "" (("|" ++) . pretty) ext ++ ">"
    where
      p (Types.Effect path _) = map pretty path

indent :: String -> String
indent s = concatMap (\s' -> "  " ++ s' ++ "\n") $ lines s

instance Pretty CompType where
  pretty (CompType r v) = pretty r ++ " " ++ pretty v

instance Pretty T.ValType where
  pretty T.TypeUnit = "()"
  pretty T.TypeInt = "Int"
  pretty T.TypeString = "String"
  pretty T.TypeBool = "Bool"
  pretty (T.TypeHandler eff from to) = "handler[" ++ pretty eff ++ "," ++ pretty from ++ "," ++ pretty to ++ "]"
  pretty (T.TypeElaboration eff row) = "elaboration[" ++ pretty eff ++ "," ++ pretty row ++ "]"
  pretty (T.TypeV v) = pretty v
  pretty (T.TypeArrow (T.Arrow args ret)) = "(" ++ intercalate ", " (map pretty args) ++ ") -> " ++ pretty ret
  pretty (T.TypeTuple params) = "(" ++ intercalate "," (map pretty params) ++ ")"
  pretty (T.TypeData dataType params) =
    pretty dataType
      ++ if not (null params) then "[" ++ intercalate "," (map pretty params) ++ "]" else ""

instance Pretty DataType where
  pretty (DataType p _ _) = pretty p

instance Pretty Effect where
  pretty (Effect path _)= pretty path

instance Pretty Path where
  pretty path = intercalate "::" (map pretty path)
