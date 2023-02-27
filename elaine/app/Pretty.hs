module Pretty where

import AST
import Data.List (intercalate)

-- Special typeclass for pretty printing the code
class Pretty a where
  pretty :: a -> String

pBlock :: String -> String
pBlock "" = "{}"
pBlock s = "{\n" ++ indent s ++ "}"

pParams :: [(Ident, Type)] -> String
pParams = intercalate ", " . map (\(name, typ) -> name ++ ": " ++ pretty typ)

concatBlock :: Pretty a => [a] -> String
concatBlock as = pBlock $ concatMap pretty as

instance Pretty Declaration where
  pretty (Import s) = "import " ++ s ++ "\n"
  pretty (Fun function) = "fun " ++ pretty function
  pretty (TypeDec name params constructors) =
    "type " ++ name ++ pretty params ++ " " ++ concatBlock constructors ++ "\n"
  pretty (Effect name operations) =
    "effect " ++ pretty name ++ " " ++ concatBlock operations ++ "\n"
  pretty (Handler name functions) =
    "handler " ++ name ++ " " ++ concatBlock functions ++ "\n"
  pretty (Elaboration name functions) =
    "elaboration " ++ name ++ " " ++ concatBlock functions ++ "\n"

instance Pretty TypeParams where
  pretty (TypeParams []) = ""
  pretty (TypeParams ids) = "[" ++ intercalate ", " ids ++ "]"

instance Pretty Function where
  pretty (Function name typeParams params ret do') =
    name
      ++ pretty typeParams
      ++ "("
      ++ pParams params
      ++ ") : "
      ++ pretty ret
      ++ " "
      ++ pBlock (pretty do')

instance Pretty EffectType where
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

instance Pretty MatchArm where
  pretty (MatchArm pat e) = pretty pat ++ " => " ++ pretty e ++ "\n"

instance Pretty Pattern where
  pretty (Pattern name vars) = name ++ "(" ++ intercalate ", " vars ++ ")"

instance Pretty Program where
  pretty mods = intercalate "\n" $ map pretty mods

instance Pretty Constructor where
  pretty (Constructor name params) = name ++ "(" ++ intercalate ", " (map pretty params) ++ ")\n"

instance Pretty Operation where
  pretty (Operation name tps params ret) = name ++ pretty tps ++ "(" ++ pParams params ++ "): " ++ pretty ret ++ "\n"

instance Pretty Type where
  pretty (Type name effs) = unwords $ name : map pretty effs

indent :: String -> String
indent s = concatMap (\s' -> "  " ++ s' ++ "\n") $ lines s