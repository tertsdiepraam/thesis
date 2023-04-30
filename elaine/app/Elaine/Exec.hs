module Elaine.Exec where

import Control.Exception (assert)
import Data.List (foldl')
import Data.Map (lookup)
import Elaine.AST (Program, Value, modName)
import Elaine.Desugar (desugar)
import Elaine.Eval (envBindings, envName, evalModule)
import Elaine.Std (stdEnv)
import Prelude hiding (last, lookup)

last :: [a] -> Maybe a
last = foldl (\_ x -> Just x) Nothing

exec :: Program -> Value
exec program =
  let desugared = desugar program
      envs = foldl' (\es new -> es ++ [(modName new, evalModule es new)]) [("std", stdEnv)] desugared
      mainEnv = case last envs of
        Just (_, e) -> assert (envName e == "main") e
        Nothing -> error "Must define at least one module"
   in case lookup "main" (envBindings mainEnv) of
        Just f -> f
        Nothing -> error "Main module does not have a main binding"