module Pretty where

import AST
import Data.List (intercalate)

-- Special typeclass for pretty printing the code
class Pretty a where
  pretty :: a -> String

pBlock :: String -> String
pBlock "" = "{}"
pBlock s = "{\n" ++ indent s ++ "}"

pParams :: [(Ident, ComputationType)] -> String
pParams = intercalate ", " . map (\(name, typ) -> name ++ ": " ++ pretty typ)

concatBlock :: Pretty a => [a] -> String
concatBlock as = pBlock $ unlines (map pretty as)

instance Pretty Declaration where
  pretty (Import s) = "import " ++ s
  pretty (DecFun name function) = "fn " ++ name ++ pretty function
  pretty (DecType name params constructors) =
    "type " ++ name ++ pretty params ++ " " ++ concatBlock constructors
  pretty (DecEffect name operations) =
    "effect " ++ pretty name ++ " " ++ concatBlock operations
  pretty (DecHandler (Handler name handlerType ret functions)) =
    "handler "
      ++ name
      ++ ": "
      ++ pretty handlerType
      ++ " "
      ++ pBlock (unlines (pretty ret : map pretty functions))
  pretty (DecElaboration (Elaboration name handlerType functions)) =
    "elaboration " ++ name ++ ": " ++ pretty handlerType ++ " " ++ concatBlock functions ++ "\n"

instance Pretty HandleReturn where
  pretty (HandleReturn var body) =
    "return(" ++ var ++ ") " ++ pBlock (pretty body)

instance Pretty HandleFunction where
  pretty (HandleFunction operation body) =
    pretty operation ++ " " ++ pBlock (pretty body)

instance Pretty TypeParams where
  pretty (TypeParams []) = ""
  pretty (TypeParams ids) = "[" ++ intercalate ", " ids ++ "]"

instance Pretty Function where
  pretty (Function sig do') =
    pretty sig
      ++ " "
      ++ pBlock (pretty do')
      ++ "\n"

instance Pretty FunSig where
  pretty (FunSig typeParams params ret) =
    pretty typeParams
      ++ "("
      ++ pParams params
      ++ "): "
      ++ pretty ret

instance Pretty Effect where
  pretty (HigherOrder name) = "!!" ++ name
  pretty (Algebraic name) = "!" ++ name

instance Pretty Module where
  pretty (Mod s decs) = "mod " ++ s ++ " " ++ concatBlock decs ++ "\n"

instance Pretty Do where
  pretty (Do l d) = pretty l ++ ";\n" ++ pretty d
  pretty (Pure e) = pretty e

instance Pretty Let where
  pretty (Let x e) = x ++ " <- " ++ pretty e

instance Pretty Expr where
  pretty (App name params) = name ++ "(" ++ intercalate ", " (map pretty params) ++ ")"
  pretty (Leaf leaf) = pretty leaf
  pretty (Match e arms) = "match " ++ pretty e ++ " " ++ concatBlock arms

instance Pretty Leaf where
  pretty (Var var) = var
  pretty (Lit lit) = pretty lit

instance Pretty Lit where
  pretty (Int n) = show n
  pretty (String s) = show s
  pretty (Bool b) = if b then "true" else "false"
  pretty (Fn f) = "fn" ++ pretty f
  pretty Unit = "()"

instance Pretty MatchArm where
  pretty (MatchArm pat e) = pretty pat ++ " => " ++ pretty e ++ "\n"

instance Pretty Pattern where
  pretty (Pattern name vars) = name ++ "(" ++ intercalate ", " vars ++ ")"

instance Pretty Program where
  pretty mods = intercalate "\n" $ map pretty mods

instance Pretty Constructor where
  pretty (Constructor name params) = name ++ "(" ++ intercalate ", " (map pretty params) ++ ")\n"

instance Pretty Operation where
  pretty (Operation name funSig) = name ++ pretty funSig

instance Pretty ComputationType where
  -- In the case where we have a function type and effects, we need to disambiguate that the effects belong
  -- outside the function with parentheses
  pretty (ComputationType a@(ValueFunctionType _) (x : xs)) = unwords $ ("(" ++ pretty a ++ ")") : map pretty (x : xs)
  pretty (ComputationType valueType effs) = unwords $ pretty valueType : map pretty effs

instance Pretty ValueType where
  pretty (TypeName name typeParams) = name ++ pretty typeParams
  pretty (ValueFunctionType sig) = pretty sig
  pretty UnitType = "()"

instance Pretty HandlerType where
  pretty (HandlerType from to) = pretty from ++ " -> " ++ pretty to

instance Pretty FunctionType where
  pretty (FunctionType typeParams params ret) = "fn" ++ pretty typeParams ++ "(" ++ intercalate ", " (map pretty params) ++ "): " ++ pretty ret

indent :: String -> String
indent s = concatMap (\s' -> "  " ++ s' ++ "\n") $ lines s