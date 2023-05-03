module Elaine.Exec where

import Data.Map (lookup, singleton)
import Elaine.AST (Program, Value)
import Elaine.Desugar (desugar)
import Elaine.Eval (newEnv, privateEnv, envBindings, envModules, evalModule)
import Elaine.Std (stdEnv)
import Prelude hiding (last, lookup)

last :: [a] -> Maybe a
last = foldl (\_ x -> Just x) Nothing

exec :: Program -> Value
exec program =
  let desugared = desugar program
      initialEnv = newEnv { envModules = singleton "std" stdEnv }
      result = evalModule initialEnv desugared
   in case lookup "main" (envBindings $ privateEnv result ) of
        Just f -> f
        Nothing -> error "Main module does not have a main binding"